-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The rule is 'IsFileOfInterest'
module LSP.Core.OfInterest(
    ofInterestRules,
    getFilesOfInterest,
    getFilesOfInterestUntracked,
    addFileOfInterest,
    deleteFileOfInterest,
    setFilesOfInterest,
    FileOfInterestStatus(..),
    OfInterestVar(..),
    scheduleGarbageCollection,
    Log(..),
    GarbageCollectVar(..),
    ) where

import           Control.Concurrent.Strict
import           Control.Monad.IO.Class
import           Data.HashMap.Strict                      (HashMap)
import qualified Data.HashMap.Strict                      as HashMap
import           Development.IDE.Graph

import qualified Data.ByteString                          as BS
import           LSP.Core.RuleTypes
import           LSP.Core.Shake               hiding (Log)
import qualified LSP.Core.Shake               as Shake
import           LSP.Core.Types.Shake              (toKey)
import           LSP.Logger                               (Pretty (pretty),
                                                           Priority (..),
                                                           Recorder,
                                                           WithPriority,
                                                           cmapWithPrio,
                                                           logWith)
import Language.LSP.Protocol.Types

data Log = LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \ case
    LogShake msg -> pretty msg

newtype OfInterestVar = OfInterestVar (Var (HashMap NormalizedUri FileOfInterestStatus))

instance IsIdeGlobal OfInterestVar

-- | The rule that initialises the files of interest state.
ofInterestRules :: Recorder (WithPriority Log) -> Rules ()
ofInterestRules recorder = do
    addIdeGlobal . OfInterestVar =<< liftIO (newVar HashMap.empty)
    addIdeGlobal . GarbageCollectVar =<< liftIO (newVar False)
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsFileOfInterest f -> do
        alwaysRerun
        filesOfInterest <- getFilesOfInterestUntracked
        let foi = maybe NotFOI IsFOI $ f `HashMap.lookup` filesOfInterest
            fp  = summarize foi
            res = (Just fp, Just foi)
        return res
    where
    summarize NotFOI                   = BS.singleton 0
    summarize (IsFOI OnDisk)           = BS.singleton 1
    summarize (IsFOI (Modified False)) = BS.singleton 2
    summarize (IsFOI (Modified True))  = BS.singleton 3

------------------------------------------------------------
newtype GarbageCollectVar = GarbageCollectVar (Var Bool)
instance IsIdeGlobal GarbageCollectVar

------------------------------------------------------------
-- Exposed API

getFilesOfInterest :: IdeState -> IO( HashMap NormalizedUri FileOfInterestStatus)
getFilesOfInterest state = do
    OfInterestVar var <- getIdeGlobalState state
    readVar var

-- | Set the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
setFilesOfInterest :: IdeState -> HashMap NormalizedUri FileOfInterestStatus -> IO ()
setFilesOfInterest state files = do
    OfInterestVar var <- getIdeGlobalState state
    writeVar var files

getFilesOfInterestUntracked :: Action (HashMap NormalizedUri FileOfInterestStatus)
getFilesOfInterestUntracked = do
    OfInterestVar var <- getIdeGlobalAction
    liftIO $ readVar var

addFileOfInterest :: IdeState -> NormalizedUri -> FileOfInterestStatus -> IO [Key]
addFileOfInterest state f v = do
    OfInterestVar var <- getIdeGlobalState state
    (prev, files) <- modifyVar var $ \dict -> do
        let (prev, new) = HashMap.alterF (, Just v) f dict
        pure (new, (prev, new))
    if prev /= Just v
    then do
        logWith (ideLogger state) Debug $
            LogSetFilesOfInterest (HashMap.toList files)
        return [toKey IsFileOfInterest f]
    else return []

deleteFileOfInterest :: IdeState -> NormalizedUri -> IO [Key]
deleteFileOfInterest state f = do
    OfInterestVar var <- getIdeGlobalState state
    files <- modifyVar' var $ HashMap.delete f
    logWith (ideLogger state) Debug $
        LogSetFilesOfInterest (HashMap.toList files)
    return [toKey IsFileOfInterest f]

scheduleGarbageCollection :: IdeState -> IO ()
scheduleGarbageCollection state = do
    GarbageCollectVar var <- getIdeGlobalState state
    writeVar var True
