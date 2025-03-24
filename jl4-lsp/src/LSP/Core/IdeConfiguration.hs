{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
module LSP.Core.IdeConfiguration
  ( IdeConfiguration(..)
  , registerIdeConfiguration
  , getIdeConfiguration
  , parseConfiguration
  , parseWorkspaceFolder
  , isWorkspaceFile
  , modifyWorkspaceFolders
  , modifyClientSettings
  , getClientSettings
  )
where

import           Control.Concurrent.Strict
import           Control.Exception (ErrorCall)
import           Control.Exception.Safe (try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Types               (Value)
import           Data.Hashable                  (Hashed, hashed, unhashed)
import           Data.HashSet                   (HashSet, singleton)
import           Data.Text                      (isPrefixOf)
import           LSP.Core.Shake
import           Development.IDE.Graph
import           Language.LSP.Protocol.Types
import           System.FilePath                (isRelative)

-- | Lsp client relevant configuration details
data IdeConfiguration = IdeConfiguration
  { workspaceFolders :: HashSet NormalizedUri
  , clientSettings   :: Hashed (Maybe Value)
  }
  deriving (Show)

newtype IdeConfigurationVar = IdeConfigurationVar {unIdeConfigurationRef :: Var IdeConfiguration}

instance IsIdeGlobal IdeConfigurationVar

registerIdeConfiguration :: ShakeExtras -> IdeConfiguration -> IO ()
registerIdeConfiguration extras =
  addIdeGlobalExtras extras . IdeConfigurationVar <=< newVar

getIdeConfiguration :: Action IdeConfiguration
getIdeConfiguration =
  getIdeGlobalAction >>= liftIO . readVar . unIdeConfigurationRef

parseConfiguration :: InitializeParams -> IdeConfiguration
parseConfiguration InitializeParams {..} =
  IdeConfiguration {..}
 where
  workspaceFolders =
    foldMap (singleton . toNormalizedUri) (nullToMaybe _rootUri)
      <> (foldMap . foldMap)
           (singleton . parseWorkspaceFolder)
           (nullToMaybe =<< _workspaceFolders)
  clientSettings = hashed _initializationOptions

parseWorkspaceFolder :: WorkspaceFolder -> NormalizedUri
parseWorkspaceFolder WorkspaceFolder{_uri} =
  toNormalizedUri _uri

modifyWorkspaceFolders
  :: IdeState -> (HashSet NormalizedUri -> HashSet NormalizedUri) -> IO ()
modifyWorkspaceFolders ide f = modifyIdeConfiguration ide f'
  where f' (IdeConfiguration ws initOpts) = IdeConfiguration (f ws) initOpts

modifyClientSettings
  :: IdeState -> (Maybe Value -> Maybe Value) -> IO ()
modifyClientSettings ide f = modifyIdeConfiguration ide f'
  where f' (IdeConfiguration ws clientSettings) =
            IdeConfiguration ws (hashed . f . unhashed $ clientSettings)

modifyIdeConfiguration
  :: IdeState -> (IdeConfiguration -> IdeConfiguration) -> IO ()
modifyIdeConfiguration ide f = do
  IdeConfigurationVar var <- getIdeGlobalState ide
  void $ modifyVar' var f

isWorkspaceFile :: NormalizedUri -> Action Bool
isWorkspaceFile uri =
  case uriToNormalizedFilePath uri of
    -- FIXME: if we don't have file://, we assume that the thing is a workspace file
    Nothing -> pure True
    Just file ->
      if isRelative (fromNormalizedFilePath file)
        then return True
        else do
          mconfig <- try getIdeConfiguration

          let toText = getUri . fromNormalizedUri
          pure case mconfig of
            -- NOTE: if we are not in an editor context, we assume that every file is part
            -- of our "workspace" as in this case there's not really a concept of a workspace
            Left (_ :: ErrorCall) -> True
            Right config ->
              any
                (\root -> toText root `isPrefixOf` toText (normalizedFilePathToUri file))
                config.workspaceFolders

getClientSettings :: Action (Maybe Value)
getClientSettings = unhashed . clientSettings <$> getIdeConfiguration
