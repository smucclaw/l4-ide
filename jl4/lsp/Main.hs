{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import L4.Syntax
import L4.Lexer (TokenCategory(..), PosToken(..), SrcPos(..))
import qualified L4.Parser as Parser

import Control.Applicative (Alternative (..))
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import GHC.Stack
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile (..))
import qualified Control.Monad.Extra as Extra
import Control.Monad.Trans.Except
import L4.Annotation
import qualified L4.Lexer as Lexer
import L4.ExactPrint (EPError(..))
import Control.Monad.Trans.Class

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ -- We need these notifications handlers to declare that we handle these requests
      notificationHandler SMethod_Initialized mempty
    , -- Handling of the virtual file system
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , -- Subscribe to notification changes
      notificationHandler SMethod_WorkspaceDidChangeConfiguration mempty
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let
          TRequestMessage _ _ _ (SemanticTokensParams _ _ doc) = req
          uri = doc ^. J.uri

        mVirtFile <- getVirtualFile $ toNormalizedUri uri
        case mVirtFile of
          Nothing -> do
            responder $ Right $ InR Null
          Just (VirtualFile _ _ rope) -> do
            let
              contents = Rope.toText rope

            case parseJL4WithWithDiagnostics uri contents of
              Left _diags -> responder $ Right $ InR Null
              Right ds -> do
                case runExcept $ runReaderT (programToTokens ds) defaultInfo of
                  Left _err -> do
                    -- TODO: log error
                    responder $ Right $ InR Null
                  Right semanticTokenstoks -> do
                    let semanticTokens =  relativizeTokens $ fmap toSemanticTokenAbsolute semanticTokenstoks
                    case encodeTokens defaultSemanticTokensLegend semanticTokens of
                      Left _err -> do
                        responder $ Right $ InR Null
                      Right semanticTokensData -> do
                        responder $
                          Right $
                            InL $
                              SemanticTokens
                                { _resultId = Nothing
                                , _data_ = semanticTokensData
                                }
    ]

-- These settings are important!
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True
    , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions
    , optExecuteCommandCommands = Just []
    }

main :: IO Int
main =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "jl4"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = lspOptions
      }

-- ----------------------------------------------------------------------------
-- LSP Diagnostics
-- ----------------------------------------------------------------------------

sendDiagnostics :: NormalizedUri -> LspM () ()
sendDiagnostics fileUri = do
  mfile <- getVirtualFile fileUri
  case mfile of
    Nothing -> pure ()
    Just (VirtualFile version _ rope) -> do
      let
        contents = Rope.toText rope

      diags <- case parseJL4WithWithDiagnostics (fromNormalizedUri fileUri) contents of
        Left diags -> pure diags
        Right _ds -> pure []
      publishDiagnostics 100 fileUri (Just version) (partitionBySource diags)

-- ----------------------------------------------------------------------------
-- Simala Parser
-- ----------------------------------------------------------------------------

parseJL4WithWithDiagnostics :: Uri -> Text -> Either [Diagnostic] (Program Name)
parseJL4WithWithDiagnostics uri content = case Parser.execParser Parser.program fp content of
  Left err ->
    Left
      [ Diagnostic
          (LSP.Range (LSP.Position 0 0) (LSP.Position 1 0))
          (Just LSP.DiagnosticSeverity_Error) -- severity
          Nothing -- code
          Nothing
          (Just "parser") -- source
          ("Failed to parse program: " <> Text.pack err)
          Nothing -- tags
          (Just [])
          Nothing
      ]
  Right ds ->
    pure ds
 where
  fp = Maybe.fromMaybe "in-memory" $ uriToFilePath uri

-- ----------------------------------------------------------------------------
-- LSP Helpers
-- ----------------------------------------------------------------------------

data SemanticToken = SemanticToken
  { start :: Position
  , length :: UInt
  , category :: SemanticTokenTypes
  , modifiers :: [SemanticTokenModifiers]
  }
  deriving stock (Show, Eq, Ord)

toSemanticTokenAbsolute :: SemanticToken -> SemanticTokenAbsolute
toSemanticTokenAbsolute s =
  SemanticTokenAbsolute
    { _line = s.start ^. J.line
    , _startChar = s.start ^. J.character
    , _length = s.length
    , _tokenType = s.category
    , _tokenModifiers = s.modifiers
    }

standardTokenType :: TokenCategory -> Maybe SemanticTokenTypes
standardTokenType = \case
  CIdentifier -> Just SemanticTokenTypes_Variable
  CStringLit -> Just SemanticTokenTypes_String
  CNumberLit -> Just SemanticTokenTypes_Number
  CSymbol -> Just SemanticTokenTypes_Operator
  COperator -> Just SemanticTokenTypes_Operator
  CKeyword -> Just SemanticTokenTypes_Keyword
  CComment -> Just SemanticTokenTypes_Comment
  CWhitespace -> Nothing
  CDirective -> Just SemanticTokenTypes_Macro
  CEOF -> Nothing

simpleTokenType :: PosToken -> Maybe SemanticTokenTypes
simpleTokenType t = standardTokenType (Lexer.posTokenCategory t.payload)

type HoleFit = [SemanticToken]

data SemanticTokenCtx p = Info
  { toSemanticToken :: p -> Maybe SemanticTokenTypes
  , getModifiers :: p -> Maybe [SemanticTokenModifiers]
  }

defaultInfo :: SemanticTokenCtx PosToken
defaultInfo =
  Info
    { toSemanticToken = simpleTokenType
    , getModifiers = \_ -> pure []
    }

parameterType :: TokenCategory -> Maybe SemanticTokenTypes
parameterType = \case
  CIdentifier -> Just SemanticTokenTypes_Parameter
  _ -> Nothing

nameIsDirective :: TokenCategory -> Maybe SemanticTokenTypes
nameIsDirective = \case
  CIdentifier -> Just SemanticTokenTypes_Macro
  _ -> Nothing

enumType :: TokenCategory -> Maybe SemanticTokenTypes
enumType = \case
  CIdentifier -> Just SemanticTokenTypes_Enum
  _ -> Nothing

defVar :: TokenCategory -> Maybe [SemanticTokenModifiers]
defVar = \case
  CIdentifier -> Just [SemanticTokenModifiers_Declaration, SemanticTokenModifiers_Definition]
  _ -> Nothing

withModifier :: (TokenCategory -> Maybe [SemanticTokenModifiers]) -> SemanticM a -> SemanticM a
withModifier f act = do
  local (\i -> i{getModifiers = \t -> f (Lexer.posTokenCategory t.payload) <|> i.getModifiers t}) act

withTokenType :: (TokenCategory -> Maybe SemanticTokenTypes) -> SemanticM a -> SemanticM a
withTokenType f act = do
  local (\i -> i{toSemanticToken = \t -> f (Lexer.posTokenCategory t.payload) <|> i.toSemanticToken t}) act

-- ----------------------------------------------------------------------------
-- Simala AST to Semantic Tokens
-- ----------------------------------------------------------------------------

traverseCsnWithHoles :: (HasCallStack) => Anno -> [SemanticM HoleFit] -> SemanticM [SemanticToken]
traverseCsnWithHoles (Anno []) _ = pure []
traverseCsnWithHoles (Anno (AnnoHole : cs)) holeFits = case holeFits of
  [] -> lift $ throwE $ InsufficientHoleFit callStack
  (x : xs) -> do
    toks <- x
    restOfTokens <- traverseCsnWithHoles (Anno cs) xs
    pure $ toks <> restOfTokens
traverseCsnWithHoles (Anno (AnnoCsn m : cs)) xs = do
  ctx <- ask
  let
    transformSyntaxNode token = pack token <$> ctx.toSemanticToken token <*> ctx.getModifiers token
    thisSyntaxNode = Maybe.mapMaybe transformSyntaxNode (csnTokens m)

  restOfTokens <- traverseCsnWithHoles (Anno cs) xs
  pure $ thisSyntaxNode <> restOfTokens
 where
  pack :: PosToken -> SemanticTokenTypes -> [SemanticTokenModifiers] -> SemanticToken
  pack token category modifiers =
    SemanticToken
      { start = srcPosToPosition token.range.start
      , length = fromIntegral token.range.length
      , category = category
      , modifiers = modifiers
      }

srcPosToPosition :: SrcPos -> Position
srcPosToPosition s =
  Position
    { _character = fromIntegral s.column - 1
    , _line = fromIntegral s.line - 1
    }

-- ----------------------------------------------------------------------------
-- Simala AST to Semantic Tokens
-- ----------------------------------------------------------------------------


type SemanticM a = ReaderT (SemanticTokenCtx PosToken) (Except EPError) a

programToTokens :: Program Name -> SemanticM HoleFit
programToTokens (MkProgram ann sections) =
  traverseCsnWithHoles ann [Extra.concatMapM sectionToTokens sections]

sectionToTokens :: Section Name -> SemanticM [SemanticToken]
sectionToTokens (MkSection ann _lvl name decls) =
  traverseCsnWithHoles
    ann
    [ withTokenType nameIsDirective $ nameToTokens name
    , Extra.concatMapM declToTokens decls
    ]

declToTokens :: Decl Name -> SemanticM HoleFit
declToTokens = \case
  Declare ann declare ->
    traverseCsnWithHoles ann [declareToTokens declare]
  Decide ann decide ->
    traverseCsnWithHoles ann [decideToTokens decide]

declareToTokens :: Declare Name -> SemanticM [SemanticToken]
declareToTokens (MkDeclare ann name type') =
  traverseCsnWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

typeToTokens :: Type' Name -> SemanticM HoleFit
typeToTokens = \case
  NamedType ann named -> traverseCsnWithHoles ann [nameToTokens named]
  Enum ann e -> traverseCsnWithHoles ann [withTokenType enumType $ Extra.concatMapM nameToTokens e]
  Record ann rcs -> traverseCsnWithHoles ann [Extra.concatMapM typedNameToTokens rcs]
  Boolean ann -> traverseCsnWithHoles ann []

typedNameToTokens :: TypedName Name -> SemanticM [SemanticToken]
typedNameToTokens (MkTypedName ann name type') =
  traverseCsnWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

decideToTokens :: Decide Name -> SemanticM HoleFit
decideToTokens (MkDecide ann typeSig clauses) =
  traverseCsnWithHoles
    ann
    [ typeSigToTokens typeSig
    , Extra.concatMapM clauseToTokens clauses
    ]

clauseToTokens :: Clause Name -> SemanticM HoleFit
clauseToTokens (GuardedClause ann e guard) =
  traverseCsnWithHoles
    ann
    [ exprToTokens e
    , guardToTokens guard
    ]

guardToTokens :: Guard Name -> SemanticM HoleFit
guardToTokens = \case
  PlainGuard ann e -> traverseCsnWithHoles ann [exprToTokens e]
  Otherwise ann -> traverseCsnWithHoles ann []

exprToTokens :: Expr Name -> SemanticM HoleFit
exprToTokens = \case
  And ann e1 e2 ->
    traverseCsnWithHoles
      ann
      [exprToTokens e1, exprToTokens e2]
  Or ann e1 e2 ->
    traverseCsnWithHoles
      ann
      [exprToTokens e1, exprToTokens e2]
  Is ann e1 e2 ->
    traverseCsnWithHoles
      ann
      [exprToTokens e1, exprToTokens e2]
  Not ann e ->
    traverseCsnWithHoles
      ann
      [exprToTokens e]
  Proj ann e lbl ->
    traverseCsnWithHoles
      ann
      [exprToTokens e, nameToTokens lbl]
  Var ann name ->
    traverseCsnWithHoles
      ann
      [nameToTokens name]

typeSigToTokens :: TypeSig Name -> SemanticM HoleFit
typeSigToTokens (MkTypeSig ann given mGiveth) =
  traverseCsnWithHoles
    ann
    [ givenToTokens given
    , maybe (pure []) givethToTokens mGiveth
    ]

givethToTokens :: GivethSig Name -> SemanticM HoleFit
givethToTokens (MkGivethSig ann typedName) =
  traverseCsnWithHoles
    ann
    [ typedNameToTokens typedName
    ]

givenToTokens :: GivenSig Name -> SemanticM HoleFit
givenToTokens (MkGivenSig ann names) =
  traverseCsnWithHoles
    ann
    [ Extra.concatMapM typedNameToTokens names
    ]

nameToTokens :: Name -> SemanticM HoleFit
nameToTokens (Name ann _) = traverseCsnWithHoles ann []
