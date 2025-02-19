{-# LANGUAGE DataKinds #-}

module JLServer where

import Control.Monad.Extra
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as J
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified L4.Lexer as Lexer
import LSP.Core.Shake hiding (Log)
import LSP.L4.Config
import LSP.L4.Handlers hiding (Log)
import qualified LSP.L4.Handlers as Handlers
import LSP.L4.Rules hiding (Log)
import qualified LSP.L4.Rules as Rules
import LSP.LanguageServer (ServerM, setupLSP)
import qualified LSP.LanguageServer as LanguageServer
import LSP.Logger
import LSP.Server hiding (Log)
import qualified LSP.Server as Server
import Language.LSP.Server (Options(..))

data Log
  = LogRules Rules.Log
  | LogHandlers Handlers.Log
  | LogServer Server.Log
  | LogLanguageServer LanguageServer.Log

instance Pretty Log where
  pretty = \case
    LogRules msg -> pretty msg
    LogHandlers msg -> pretty msg
    LogServer msg -> pretty msg
    LogLanguageServer msg -> pretty msg

jl4ServerConfig :: Recorder (WithPriority Log) -> ServerConfig ServerM Config
jl4ServerConfig recorder =
  ServerConfig
    { config = defConfig
    , parseServerConfig = parseServerConfig
    , onConfigChange = defOnConfigChange serverRecorder
    , rules = jl4Rules rulesLogger
    , handlers = lspHandlers
    , kick = kickRules $ \files -> do
        void $
             uses GetLexTokens files
          *> uses GetParsedAst files
          *> uses TypeCheck files
          *> uses Evaluate files
    , setupLsp = \fp getIdeState ->
        setupLSP
          languageServerRecorder
          fp
          lspHandlers
          getIdeState
    , keywords = jl4Keywords
    , configSection = "jl4"
    , lspOptions = Server.lspOptions
      { optExecuteCommandCommands = Just $ map snd l4CmdNames
      }
    }
  where
    lspHandlers = handlers handlerRecorder
    handlerRecorder = cmapWithPrio LogHandlers recorder
    serverRecorder = cmapWithPrio LogServer recorder
    rulesLogger = cmapWithPrio LogRules recorder
    languageServerRecorder = cmapWithPrio LogLanguageServer recorder

jl4Keywords :: [Text]
jl4Keywords = Map.keys Lexer.keywords

parseServerConfig :: J.FromJSON config => config -> Aeson.Value -> Either Text config
parseServerConfig _ v = do
  case J.fromJSON v of
    J.Error e -> Left (Text.pack e)
    J.Success cfg -> Right cfg

