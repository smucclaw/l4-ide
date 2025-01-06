{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticTokens where

import L4.Annotation
import L4.ExactPrint (AnnoFirst, EPError (..), genericToTokens)
import L4.Lexer (PosToken (..), SrcPos (..), TokenCategory (..))
import qualified L4.Lexer as Lexer
import L4.Syntax

import Control.Applicative (Alternative (..))
import Control.Lens hiding (Iso)
import qualified Control.Monad.Extra as Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.Maybe as Maybe
import GHC.Stack
import Generics.SOP as SOP
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types hiding (Pattern)

-- ----------------------------------------------------------------------------
-- Semantic Tokens Interface
-- ----------------------------------------------------------------------------

type SemanticM a = ReaderT (SemanticTokenCtx PosToken) (Except EPError) a

type HoleFit = SemanticM [SemanticToken]

-- I would prefer to avoid the duplication between this class and
-- the ToTokens class.
--
-- We might want to override some functionality here. We should perhaps
-- try to find another way to do this.
class ToSemTokens a where
  toSemTokens :: a -> HoleFit
  default toSemTokens ::
    (SOP.Generic a, All (AnnoFirst ToSemTokens) (Code a)) =>
    a ->
    HoleFit
  toSemTokens =
    genericToTokens (Proxy @ToSemTokens) toSemTokens traverseCsnWithHoles

instance (ToSemTokens a) => ToSemTokens [a] where
  toSemTokens =
    Extra.concatMapM toSemTokens

instance (ToSemTokens a) => ToSemTokens (Maybe a) where
  toSemTokens =
    maybe (pure []) toSemTokens

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
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
  CAnnotation -> Just SemanticTokenTypes_Decorator
  CEOF -> Nothing

simpleTokenType :: PosToken -> Maybe SemanticTokenTypes
simpleTokenType t = standardTokenType (Lexer.posTokenCategory t.payload)

data SemanticTokenCtx p = SemanticTokenCtx
  { toSemanticToken :: p -> Maybe SemanticTokenTypes
  , getModifiers :: p -> Maybe [SemanticTokenModifiers]
  }

defaultInfo :: SemanticTokenCtx PosToken
defaultInfo =
  SemanticTokenCtx
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

traverseCsnWithHoles :: (HasCallStack) => Anno -> [HoleFit] -> SemanticM [SemanticToken]
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

deriving anyclass instance ToSemTokens (Program Name)

-- Generic instance does not apply because we exclude the level and override
-- the token type for the name.
instance ToSemTokens (Section Name) where
  toSemTokens (MkSection ann _lvl name decls) =
    traverseCsnWithHoles ann [withTokenType nameIsDirective $ toSemTokens name, toSemTokens decls]

deriving anyclass instance ToSemTokens (TopDecl Name)
deriving anyclass instance ToSemTokens (Assume Name)
deriving anyclass instance ToSemTokens (Declare Name)
deriving anyclass instance ToSemTokens (TypeDecl Name)
deriving anyclass instance ToSemTokens (ConDecl Name)
deriving anyclass instance ToSemTokens (Type' Name)
deriving anyclass instance ToSemTokens (TypedName Name)
deriving anyclass instance ToSemTokens (OptionallyTypedName Name)
deriving anyclass instance ToSemTokens (Decide Name)
deriving anyclass instance ToSemTokens (AppForm Name)
deriving anyclass instance ToSemTokens (Expr Name)
deriving anyclass instance ToSemTokens (Branch Name)
deriving anyclass instance ToSemTokens (Pattern Name)
deriving anyclass instance ToSemTokens (TypeSig Name)
deriving anyclass instance ToSemTokens (GivethSig Name)
deriving anyclass instance ToSemTokens (GivenSig Name)
deriving anyclass instance ToSemTokens (Directive Name)

instance ToSemTokens Name where
  toSemTokens (Name ann _) =
    traverseCsnWithHoles ann []
  toSemTokens (PreDef ann _) =
    traverseCsnWithHoles ann []
