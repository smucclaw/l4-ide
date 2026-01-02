module L4.Repl.Types where

import Base.Text (Text)
import Data.Map.Strict (Map)
import Language.LSP.Protocol.Types (NormalizedFilePath)

import L4.Syntax (Name, Expr, Resolved)
import L4.TypeCheck.Types (Environment, EntityInfo)
import L4.EvaluateLazy (EvalConfig)
import LSP.Core.Shake (IdeState)

-- | State maintained across REPL iterations
data ReplState = ReplState
  { ideState        :: IdeState
  , loadedFiles     :: [NormalizedFilePath]
  , sessionBindings :: Map Name (Expr Resolved)
  , evalConfig      :: EvalConfig
  , options         :: ReplOptions
  }

-- | REPL configuration options
data ReplOptions = ReplOptions
  { showTrace :: Bool      -- ^ Show evaluation trace
  , showType  :: Bool      -- ^ Show type with each result
  , prompt    :: Text      -- ^ REPL prompt string
  }

defaultOptions :: ReplOptions
defaultOptions = ReplOptions
  { showTrace = False
  , showType  = False
  , prompt    = "jl4> "
  }

-- | Result of processing REPL input
data ReplResult
  = ContinueRepl Text ReplState  -- ^ Output text and new state
  | QuitRepl                      -- ^ Exit the REPL
  | ReplError ReplError           -- ^ An error occurred

-- | REPL errors
data ReplError
  = ParseError Text
  | TypeCheckError Text
  | EvalError Text
  | FileNotFound FilePath
  | CommandError Text
  deriving (Show)

-- | Merged context for evaluation
data EvalContext = EvalContext
  { environment :: Environment
  , entityInfo  :: EntityInfo
  }
