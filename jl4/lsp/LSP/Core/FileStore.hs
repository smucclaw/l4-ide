-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
module LSP.Core.FileStore (
    getFileModTimeContents,
    getFileContents,
    getUriContents,
    getVersionedTextDoc,
    setFileModified,
    setSomethingModified,
    fileStoreRules,
    modificationTime,
    resetFileStore,
    resetInterfaceStore,
    getModificationTimeImpl,
    addIdeGlobal,
    getFileContentsImpl,
    getModTime,
    isWatchSupported,
    registerFileWatches,
    shareFilePath,
    Log(..),
    ) where

import           Control.Concurrent.STM.Stats                 (STM, atomically)
import           Control.Exception
import           Control.Lens                                 ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.Binary                                  as B
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.HashMap.Strict                          as HashMap
import           Data.IORef
import qualified Data.Text                                    as T
import qualified Data.Text                                    as Text
import           Data.Text.Utf16.Rope.Mixed                   (Rope)
import           Data.Time
import           Data.Time.Clock.POSIX
import           LSP.Core.FileUtils
import           LSP.Core.IdeConfiguration        (isWorkspaceFile)
import           LSP.Core.RuleTypes
import           LSP.Core.Shake                   hiding (Log)
import qualified LSP.Core.Shake                   as Shake
import           Development.IDE.Graph
import           LSP.Core.Types.Diagnostics
import qualified LSP.Core.Types.Options as Options
import           LSP.Core.Types.Shake                  (toKey)
import           LSP.Logger                                   (Pretty (pretty),
                                                               Recorder,
                                                               WithPriority,
                                                               cmapWithPrio, viaShow,
                                                               (<+>))
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message                (toUntypedRegistration)
import qualified Language.LSP.Protocol.Message                as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types                  as LSP
import           Language.LSP.VFS
import           System.FilePath
import           System.IO.Error
import           System.IO.Unsafe
import Debug.Trace

data Log
  = LogCouldNotIdentifyReverseDeps !NormalizedFilePath
  | LogTypeCheckingReverseDeps !NormalizedFilePath !(Maybe [NormalizedFilePath])
  | LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogCouldNotIdentifyReverseDeps path ->
      "Could not identify reverse dependencies for" <+> viaShow path
    (LogTypeCheckingReverseDeps path reverseDepPaths) ->
      "Typechecking reverse dependencies for"
      <+> viaShow path
      <> ":"
      <+> pretty (fmap (fmap show) reverseDepPaths)
    LogShake msg -> pretty msg

addWatchedFileRule :: Recorder (WithPriority Log) -> (NormalizedFilePath -> Action Bool) -> Rules ()
addWatchedFileRule recorder isWatched = defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \AddWatchedFile f -> do
  isAlreadyWatched <- isWatched f
  isWp <- isWorkspaceFile f
  if isAlreadyWatched then pure (Just True) else
    if not isWp then pure (Just False) else do
        ShakeExtras{lspSink} <- getShakeExtras
        case lspSink of
            Just sink -> fmap Just $ liftIO $ do
              clientCaps <- Options.currentClientCapabilities sink
              case registerFileWatches clientCaps [fromNormalizedFilePath f] of
                Nothing -> pure False
                Just regParams -> do
                  void $ Options.sendRequestToClient sink LSP.SMethod_ClientRegisterCapability regParams (const $ pure ()) -- TODO handle response
                  pure $ True
            Nothing -> pure $ Just False


getModificationTimeRule :: Recorder (WithPriority Log) -> Rules ()
getModificationTimeRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \(GetModificationTime_ missingFileDiags) file ->
    getModificationTimeImpl missingFileDiags file

getModificationTimeImpl
  :: Bool
  -> NormalizedFilePath
  -> Action (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getModificationTimeImpl missingFileDiags file = do
    let file' = fromNormalizedFilePath file
    traceM ("========= get modification time impl " <> file')
    let wrap time = (Just $ LBS.toStrict $ B.encode $ toRational time, ([], Just $ ModificationTime time))
    mbVf <- getVirtualFile file
    case mbVf of
        Just (virtualFileVersion -> ver) -> do
            alwaysRerun
            pure (Just $ LBS.toStrict $ B.encode ver, ([], Just $ VFSVersion ver))
        Nothing -> do
            isWF <- use_ AddWatchedFile file
            if isWF
                then -- the file is watched so we can rely on FileWatched notifications,
                        -- but also need a dependency on IsFileOfInterest to reinstall
                        -- alwaysRerun when the file becomes VFS
                    void (use_ IsFileOfInterest file)
                else if isInterface file
                    then -- interface files are tracked specially using the closed world assumption
                        pure ()
                    else -- in all other cases we will need to freshly check the file system
                        alwaysRerun

            liftIO $ fmap wrap (getModTime file')
                `catch` \(e :: IOException) -> do
                    let err | isDoesNotExistError e = "File does not exist: " ++ file'
                            | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
                        diag = ideErrorText file (T.pack err)
                    if isDoesNotExistError e && not missingFileDiags
                        then return (Nothing, ([], Nothing))
                        else return (Nothing, ([diag], Nothing))

-- | Interface files cannot be watched, since they live outside the workspace.
--   But interface files are private, in that only HLS writes them.
--   So we implement watching ourselves, and bypass the need for alwaysRerun.
isInterface :: NormalizedFilePath -> Bool
isInterface f = takeExtension (fromNormalizedFilePath f) `elem` [".hi", ".hi-boot", ".hie", ".hie-boot", ".core"]

-- | Reset the GetModificationTime state of interface files
resetInterfaceStore :: ShakeExtras -> NormalizedFilePath -> STM [Key]
resetInterfaceStore state f = do
    deleteValue state GetModificationTime f

-- | Reset the GetModificationTime state of watched files
--   Assumes the list does not include any FOIs
resetFileStore :: IdeState -> [(NormalizedFilePath, LSP.FileChangeType)] -> IO [Key]
resetFileStore ideState changes = mask $ \_ -> do
    -- we record FOIs document versions in all the stored values
    -- so NEVER reset FOIs to avoid losing their versions
    -- FOI filtering is done by the caller (LSP Notification handler)
    fmap concat <$>
        forM changes $ \(nfp, c) -> do
            case c of
                LSP.FileChangeType_Changed
                    --  already checked elsewhere |  not $ HM.member nfp fois
                    -> atomically $ deleteValue (shakeExtras ideState) GetModificationTime nfp
                _ -> pure []


modificationTime :: FileVersion -> Maybe UTCTime
modificationTime VFSVersion{}             = Nothing
modificationTime (ModificationTime posix) = Just $ posixSecondsToUTCTime posix

getFileContentsRule :: Recorder (WithPriority Log) -> Rules ()
getFileContentsRule recorder = define (cmapWithPrio LogShake recorder) $ \GetFileContents file -> getFileContentsImpl file

getFileContentsImpl
    :: NormalizedFilePath
    -> Action ([FileDiagnostic], Maybe (FileVersion, Maybe Rope))
getFileContentsImpl file = do
    -- need to depend on modification time to introduce a dependency with Cutoff
    time <- use_ GetModificationTime file
    traceM "got modification time"
    res <- do
        mbVirtual <- getVirtualFile file
        traceM "got virtual file"
        pure $ _file_text <$> mbVirtual
    pure ([], Just (time, res))

-- | Returns the modification time and the contents.
--   For VFS paths, the modification time is the current time.
getFileModTimeContents :: NormalizedFilePath -> Action (UTCTime, Maybe Rope)
getFileModTimeContents f = do
    (fv, contents) <- use_ GetFileContents f
    modTime <- case modificationTime fv of
      Just t -> pure t
      Nothing -> do
        foi <- use_ IsFileOfInterest f
        liftIO $ case foi of
          IsFOI Modified{} -> getCurrentTime
          _ -> do
            posix <- getModTime $ fromNormalizedFilePath f
            pure $ posixSecondsToUTCTime posix
    return (modTime, contents)

getFileContents :: NormalizedFilePath -> Action (Maybe Rope)
getFileContents f = snd <$> use_ GetFileContents f

getUriContents :: NormalizedUri -> Action (Maybe Rope)
getUriContents uri =
    join <$> traverse getFileContents (uriToNormalizedFilePath uri)

-- | Given a text document identifier, annotate it with the latest version.
--
-- Like Language.LSP.Server.Core.getVersionedTextDoc, but gets the virtual file
-- from the Shake VFS rather than the LSP VFS.
getVersionedTextDoc :: TextDocumentIdentifier -> Action VersionedTextDocumentIdentifier
getVersionedTextDoc doc = do
  let uri = doc ^. L.uri
  mvf <-
    maybe (pure Nothing) getVirtualFile $
        uriToNormalizedFilePath $ toNormalizedUri uri
  let ver = case mvf of
        Just (VirtualFile lspver _ _) -> lspver
        Nothing                       -> 0
  return (VersionedTextDocumentIdentifier uri ver)

fileStoreRules :: Recorder (WithPriority Log) -> (NormalizedFilePath -> Action Bool) -> Rules ()
fileStoreRules recorder isWatched = do
    getModificationTimeRule recorder
    getFileContentsRule recorder
    addWatchedFileRule recorder isWatched

-- | Note that some buffer for a specific file has been modified but not
-- with what changes.
setFileModified :: VFSModified
                -> IdeState
                -> NormalizedFilePath
                -> IO [Key]
                -> IO ()
setFileModified vfs state nfp actionBefore = do
    restartShakeSession (shakeExtras state) vfs (fromNormalizedFilePath nfp ++ " (modified)") [] $ do
        keys<-actionBefore
        return (toKey GetModificationTime nfp:keys)

-- | Note that some keys have been modified and restart the session
--   Only valid if the virtual file system was initialised by LSP, as that
--   independently tracks which files are modified.
setSomethingModified :: VFSModified -> IdeState -> String -> IO [Key] -> IO ()
setSomethingModified vfs state reason actionBetweenSession = do
    -- Update database to remove any files that might have been renamed/deleted
    -- atomically $ writeTQueue (indexQueue $ hiedbWriter $ shakeExtras state) (\withHieDb -> withHieDb deleteMissingRealFiles)
    void $ restartShakeSession (shakeExtras state) vfs reason [] actionBetweenSession

registerFileWatches :: ClientCapabilities -> [String] -> Maybe RegistrationParams
registerFileWatches clientCapabilities globs =
      if  isWatchSupported clientCapabilities
      then
        let
          regParams    = LSP.RegistrationParams  [toUntypedRegistration registration]
          -- The registration ID is arbitrary and is only used in case we want to deregister (which we won't).
          -- We could also use something like a random UUID, as some other servers do, but this works for
          -- our purposes.
          registration = LSP.TRegistration { _id ="globalFileWatches"
                                           , _method = LSP.SMethod_WorkspaceDidChangeWatchedFiles
                                           , _registerOptions = Just regOptions}
          regOptions =
            DidChangeWatchedFilesRegistrationOptions { _watchers = watchers }
          -- See Note [File existence cache and LSP file watchers] for why this exists, and the choice of watch kind
          -- WatchKind_Custom 7 is for create, change, and delete
          watchKind = LSP.WatchKind_Custom 7
          -- See Note [Which files should we watch?] for an explanation of why the pattern is the way that it is
          -- The patterns will be something like "**/.hs", i.e. "any number of directory segments,
          -- followed by a file with an extension 'hs'.
          watcher glob = FileSystemWatcher { _globPattern = glob, _kind = Just watchKind }
          -- We use multiple watchers instead of one using '{}' because lsp-test doesn't
          -- support that: https://github.com/bubba/lsp-test/issues/77
          watchers = [ watcher (LSP.GlobPattern (LSP.InL (LSP.Pattern (Text.pack glob)))) | glob <- globs ]
        in
          Just regParams
      else
        Nothing

isWatchSupported :: ClientCapabilities -> Bool
isWatchSupported clientCapabilities = do
  case () of
    _ | LSP.ClientCapabilities{_workspace} <- clientCapabilities
      , Just LSP.WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
      , Just LSP.DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
      , Just True <- _dynamicRegistration
        -> True
      | otherwise -> False

filePathMap :: IORef (HashMap.HashMap FilePath FilePath)
filePathMap = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE filePathMap #-}

shareFilePath :: FilePath -> FilePath
shareFilePath k = unsafePerformIO $ do
  atomicModifyIORef' filePathMap $ \km ->
    let new_key = HashMap.lookup k km
    in case new_key of
          Just v  -> (km, v)
          Nothing -> (HashMap.insert k k km, k)
{-# NOINLINE shareFilePath  #-}
