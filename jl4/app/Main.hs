-- | Entry point for the @l4@ CLI.
--
-- Dispatches on a subcommand; if the first positional argument is a bare
-- filepath instead of a known subcommand name, falls through to @l4 run@
-- so legacy invocations like @l4 foo.l4@ (and @jl4-cli foo.l4@ from older
-- scripts) keep working.
module Main where

import Control.Applicative ((<|>))
import Options.Applicative
  ( Parser
  , ParserInfo
  , command
  , customExecParser
  , footer
  , fullDesc
  , header
  , helper
  , info
  , progDesc
  , prefs
  , showHelpOnEmpty
  , subparser
  )
import qualified Options.Applicative as Options

import L4.Cli.Ast (AstOptions, astCmd, astOptionsParser)
import L4.Cli.Batch (BatchOptions, batchCmd, batchOptionsParser)
import L4.Cli.Check (CheckOptions, checkCmd, checkOptionsParser)
import L4.Cli.Format (FormatOptions, formatCmd, formatOptionsParser)
import L4.Cli.Run (RunOptions, runCmd, runOptionsParser)
import L4.Cli.StateGraph (StateGraphOptions, stateGraphCmd, stateGraphOptionsParser)
import L4.Cli.Trace (TraceOptions, traceCmd, traceOptionsParser)

----------------------------------------------------------------------------
-- Top-level command
----------------------------------------------------------------------------

data Command
  = CmdRun        RunOptions
  | CmdCheck      CheckOptions
  | CmdFormat     FormatOptions
  | CmdAst        AstOptions
  | CmdBatch      BatchOptions
  | CmdTrace      TraceOptions
  | CmdStateGraph StateGraphOptions

commandParser :: Parser Command
commandParser =
      subparser subcommandSet
  -- Backward-compat fallthrough: `l4 foo.l4 [--flags]` = `l4 run foo.l4 [--flags]`.
  -- Must come after `subparser` so a well-typed subcommand name wins.
  <|> (CmdRun <$> runOptionsParser)
  where
    subcommandSet =
         command "run"
           (info (CmdRun <$> runOptionsParser)
             (progDesc "Typecheck and evaluate an L4 file (prints diagnostics + #EVAL results)"))
      <> command "check"
           (info (CmdCheck <$> checkOptionsParser)
             (progDesc "Typecheck an L4 file only (fast path; no evaluation)"))
      <> command "format"
           (info (CmdFormat <$> formatOptionsParser)
             (progDesc "Reformat an L4 file and print to stdout"))
      <> command "ast"
           (info (CmdAst <$> astOptionsParser)
             (progDesc "Dump the parsed AST of an L4 file"))
      <> command "batch"
           (info (CmdBatch <$> batchOptionsParser)
             (progDesc "Evaluate an @export function against many input rows (NDJSON streaming)"))
      <> command "trace"
           (info (CmdTrace <$> traceOptionsParser)
             (progDesc "Render #EVALTRACE evaluation traces as GraphViz (dot|png|svg)"))
      <> command "state-graph"
           (info (CmdStateGraph <$> stateGraphOptionsParser)
             (progDesc "Extract regulative-rule state transition graphs as GraphViz DOT"))

commandInfo :: ParserInfo Command
commandInfo = info (helper <*> commandParser)
  ( fullDesc
  <> header "l4 — the L4 computational-law CLI"
  <> progDesc "Typecheck, evaluate, format, and visualize .l4 files. Run `l4 <command> --help` for per-command options."
  <> footer "Part of the L4 tool family from Legalese.com"
  )

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnEmpty) commandInfo
  case cmd of
    CmdRun        opts -> runCmd        opts
    CmdCheck      opts -> checkCmd      opts
    CmdFormat     opts -> formatCmd     opts
    CmdAst        opts -> astCmd        opts
    CmdBatch      opts -> batchCmd      opts
    CmdTrace      opts -> traceCmd      opts
    CmdStateGraph opts -> stateGraphCmd opts

-- Silence unused-imports warning when we only import Options for types
-- indirectly via re-exports.
_unusedOptions :: Options.Parser ()
_unusedOptions = pure ()
