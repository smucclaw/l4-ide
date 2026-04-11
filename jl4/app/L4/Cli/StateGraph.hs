-- | @l4 state-graph FILE@ — extract the state transition graph of every
-- regulative rule in an L4 file as GraphViz DOT.
--
-- @REGULATIVE ... PARTY ... MUST/MAY/SHANT ...@ blocks define obligation
-- lifecycles: fulfilled, breached, dismissed. This command emits a DOT
-- diagram of those transitions so designers can visually check the flow.
--
-- Output goes to stdout; redirect with @>@. Exits 1 on typecheck failure.
module L4.Cli.StateGraph
  ( StateGraphOptions(..)
  , stateGraphOptionsParser
  , stateGraphCmd
  ) where

import Base (for_)
import qualified Base.Text as Text
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import qualified L4.StateGraph as StateGraph
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.Cli.Common

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

newtype StateGraphOptions = StateGraphOptions
  { stateGraphFile :: FilePath
  }

stateGraphOptionsParser :: Parser StateGraphOptions
stateGraphOptionsParser = StateGraphOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file with regulative rules")

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

stateGraphCmd :: StateGraphOptions -> IO ()
stateGraphCmd opts = do
  evalConfig <- makeEvalConfig (FixedNowOpt Nothing)
  (errs, mTc) <- runOneshot evalConfig opts.stateGraphFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.SuccessfulTypeCheck uri

  case mTc of
    Just tcRes | tcRes.success -> do
      let graphs = StateGraph.extractStateGraphs tcRes.module'
      case graphs of
        [] -> do
          hPutStrLn stderr "No regulative rules found in module"
          exitFailure
        _ -> do
          let sgOpts = StateGraph.defaultStateGraphOptions
          for_ graphs $ \sg ->
            TIO.putStrLn (StateGraph.stateGraphToDot sgOpts sg)
          exitSuccess
    _ -> do
      putDiagnostics errs
      hPutStrLn stderr "Type checking failed — cannot extract state graph"
      exitFailure

-- Silence unused-top-binds nag for Text.pack re-export via Base.Text.
_unusedText :: ()
_unusedText = let _ = Text.pack "" in ()
