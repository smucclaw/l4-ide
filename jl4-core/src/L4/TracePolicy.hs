{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Unified Trace Policy Architecture

This module provides a unified abstraction for controlling trace collection and output
across all L4 tools (CLI, REPL, Decision Service API, LSP).

Based on: doc/todo/TRACE-GRAPHVIZ-ARCHITECTURE.md

= Design Principles

The architecture separates three orthogonal concerns:

1. __Directive Semantics__ (file-level, author's hint):
     * @#EVAL expr@ - "Evaluate this and show the result"
     * @#EVALTRACE expr@ - "Evaluate this AND I expect the trace to be interesting"

2. __Trace Collection__ (runtime, tool decides):
     * Whether to collect traces for each directive type
     * Controlled by TracePolicy

3. __Output Control__ (format and destination):
     * Text, DOT, PNG, SVG, JSON
     * Stdout, files, API response

= Usage Pattern

Each tool creates a TracePolicy based on its context:

@
-- CLI: Default no traces (clean output), enable with flags
cliPolicy = cliDefaultPolicy

-- REPL: Full traces for exploration
replPolicy = replDefaultPolicy graphVizOptions

-- API: Controlled by request parameters
apiPolicy = apiDefaultPolicy  -- Modified per request
@

= Current Status

__Infrastructure Complete:__ Types defined, default policies available, CLI creates TracePolicy

__Future Work:__ Thread TracePolicy through evaluation pipeline so it actually controls
behavior (currently tools still use direct option fields).

-}
module L4.TracePolicy where

import L4.EvaluateLazy.GraphVizOptions (GraphVizOptions, defaultGraphVizOptions)

-- | Unified trace policy that determines when and how to collect/display traces
data TracePolicy = TracePolicy
  { evalDirectiveTrace :: TraceLevel      -- ^ What to do with #EVAL directives
  , evaltraceDirectiveTrace :: TraceLevel -- ^ What to do with #EVALTRACE directives
  }
  deriving (Eq, Show)

-- | Whether to collect traces and how to output them
data TraceLevel
  = NoTrace                    -- ^ Don't collect trace (result only)
  | CollectTrace TraceOptions  -- ^ Collect trace with options
  deriving (Eq, Show)

-- | Options for trace collection and output
data TraceOptions = TraceOptions
  { outputFormat :: TraceFormat         -- ^ How to format the trace
  , outputDest :: TraceDestination      -- ^ Where to send the output
  , graphVizOpts :: GraphVizOptions     -- ^ GraphViz-specific options
  }
  deriving (Eq, Show)

-- | Format for trace output
data TraceFormat
  = TextTrace      -- ^ Human-readable text trace
  | DotFormat      -- ^ GraphViz DOT format
  | PngFormat      -- ^ Rendered PNG image
  | SvgFormat      -- ^ Rendered SVG image
  | JsonTrace      -- ^ Structured JSON (for API)
  deriving (Eq, Show)

-- | Where to send trace output
data TraceDestination
  = Stdout                      -- ^ Print to stdout
  | StdoutAndFiles FilePath     -- ^ Print to stdout AND write files to directory
  | FilesOnly FilePath          -- ^ Only write files to directory (no stdout)
  | ApiResponse                 -- ^ Include in API response
  deriving (Eq, Show)

-- | Default policies for different tools
-- These follow the design principles from TRACE-GRAPHVIZ-ARCHITECTURE.md

-- | CLI default: No trace collection (clean output)
cliDefaultPolicy :: TracePolicy
cliDefaultPolicy = TracePolicy
  { evalDirectiveTrace = NoTrace
  , evaltraceDirectiveTrace = NoTrace
  }

-- | REPL default: Full trace collection for exploration
replDefaultPolicy :: GraphVizOptions -> TracePolicy
replDefaultPolicy gvOpts = TracePolicy
  { evalDirectiveTrace = CollectTrace (TraceOptions TextTrace Stdout gvOpts)
  , evaltraceDirectiveTrace = CollectTrace (TraceOptions TextTrace Stdout gvOpts)
  }

-- | API default: Collect traces for EVALTRACE directives
-- The decision service uses #EVALTRACE when trace=full is requested,
-- so we need to collect traces for those directives to enable GraphViz output.
apiDefaultPolicy :: TracePolicy
apiDefaultPolicy = TracePolicy
  { evalDirectiveTrace = NoTrace
  , evaltraceDirectiveTrace = CollectTrace (TraceOptions TextTrace ApiResponse defaultGraphVizOptions)
  }

-- | LSP default: Avoid editor noise
lspDefaultPolicy :: GraphVizOptions -> TracePolicy
lspDefaultPolicy gvOpts = TracePolicy
  { evalDirectiveTrace = NoTrace
  , evaltraceDirectiveTrace = CollectTrace (TraceOptions TextTrace ApiResponse gvOpts)
  }

-- | Helper: Enable text tracing for EVALTRACE directives
withTextTrace :: GraphVizOptions -> TracePolicy
withTextTrace gvOpts = TracePolicy
  { evalDirectiveTrace = NoTrace
  , evaltraceDirectiveTrace = CollectTrace (TraceOptions TextTrace Stdout gvOpts)
  }

-- | Helper: Enable GraphViz output for EVALTRACE directives
withGraphViz :: TraceFormat -> Maybe FilePath -> GraphVizOptions -> TracePolicy
withGraphViz format mOutputDir gvOpts = TracePolicy
  { evalDirectiveTrace = NoTrace
  , evaltraceDirectiveTrace = CollectTrace (TraceOptions format dest gvOpts)
  }
  where
    dest = case mOutputDir of
      Nothing -> Stdout
      Just dir -> FilesOnly dir

-- | Helper: Trace everything (for debugging)
traceAll :: TraceFormat -> TraceDestination -> GraphVizOptions -> TracePolicy
traceAll format dest gvOpts =
  let traceOpts = TraceOptions format dest gvOpts
  in TracePolicy
       { evalDirectiveTrace = CollectTrace traceOpts
       , evaltraceDirectiveTrace = CollectTrace traceOpts
       }
