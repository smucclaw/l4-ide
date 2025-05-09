-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module LSP.Core.Service(
    getIdeOptions, getIdeOptionsIO,
    IdeState, initialise, shutdown,
    runAction,
    getDiagnostics,
    ideLogger,
    updatePositionMapping,
    Log(..),
    ) where

import           Control.Applicative              ((<|>))
import           LSP.Core.Debouncer
-- import qualified LSP.Core.FileExists  as FileExists
-- import qualified LSP.Core.OfInterest  as OfInterest
import           Development.IDE.Graph
import           LSP.Core.Types.Options    (IdeOptions (..), LspSink)
import           LSP.Logger                       as Logger (Pretty (pretty),
                                                             Priority (Debug),
                                                             Recorder,
                                                             WithPriority,
                                                             cmapWithPrio)
import qualified Language.LSP.Protocol.Types      as LSP

import           Control.Monad
import           LSP.Core.Shake       hiding (Log)
import qualified LSP.Core.Shake       as Shake
import           LSP.Core.Types.Monitoring (Monitoring)
import           System.Environment               (lookupEnv)
import qualified LSP.Core.OfInterest as OfInterest
import qualified LSP.Core.FileExists as FileExists

data Log
  = LogShake Shake.Log
  | LogOfInterest OfInterest.Log
  | LogFileExists FileExists.Log
  deriving Show

instance Pretty Log where
  pretty = \ case
    LogShake msg      -> pretty msg
    LogOfInterest msg -> pretty msg
    LogFileExists msg -> pretty msg

------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Recorder (WithPriority Log)
           -> Rules ()
           -> Maybe LspSink
           -> Maybe LSP.ClientCapabilities
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> ThreadQueue
           -> Monitoring
           -> FilePath -- ^ Root directory see Note [Root Directory]
           -> IO IdeState
initialise recorder mainRule mLspSink mClientCapabilities debouncer options threadQueue metrics rootDir = do
    shakeProfiling <- do
        let fromConf = optShakeProfiling options
        fromEnv <- lookupEnv "GHCIDE_BUILD_PROFILING"
        return $ fromConf <|> fromEnv
    shakeOpen
        (cmapWithPrio LogShake recorder)
        mLspSink
        mClientCapabilities
        debouncer
        shakeProfiling
        options.optReportProgress
        options.optTesting
        threadQueue
        options.optShakeOptions
        metrics
        (do
            addIdeGlobal $ GlobalIdeOptions options
            OfInterest.ofInterestRules (cmapWithPrio LogOfInterest recorder)
            FileExists.fileExistsRules (cmapWithPrio LogFileExists recorder) mLspSink
            mainRule)
        rootDir

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- This will return as soon as the result of the action is
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runAction :: String -> IdeState -> Action a -> IO a
runAction herald ide act =
  join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug act)
