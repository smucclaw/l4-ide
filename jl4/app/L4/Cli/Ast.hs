-- | @l4 ast FILE@ — dump the parsed AST of an L4 file.
--
-- Debugging aid for L4 itself and for tooling that introspects the
-- parse tree. Uses @pretty-simple@ for readable output; colors auto
-- when stdout is a TTY.
module L4.Cli.Ast
  ( AstOptions(..)
  , astOptionsParser
  , astCmd
  ) where

import qualified Data.Text.Lazy.IO as TL
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, stdout)
import Text.Pretty.Simple (pShow, pShowNoColor)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.Cli.Common

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

newtype AstOptions = AstOptions
  { astFile :: FilePath
  }

astOptionsParser :: Parser AstOptions
astOptionsParser = AstOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file whose AST to dump")

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

astCmd :: AstOptions -> IO ()
astCmd opts = do
  evalConfig <- makeEvalConfig (FixedNowOpt Nothing)
  (errs, mAst) <- runOneshot evalConfig opts.astFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.GetParsedAst uri

  case mAst of
    Just ast -> do
      isTTY <- hIsTerminalDevice stdout
      let showFn = if isTTY then pShow else pShowNoColor
      TL.putStrLn (showFn ast)
      -- Report diagnostics too (parse succeeded, but typecheck may have complained).
      putDiagnostics errs
      exitSuccess
    Nothing -> do
      putDiagnostics errs
      exitFailure
