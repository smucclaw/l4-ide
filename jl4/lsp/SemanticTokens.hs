{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticTokens where

import L4.Annotation
import L4.ExactPrint (EPError (..), genericToNodes)
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

type SemanticTokensM t = ReaderT (SemanticTokenCtx t) (Except EPError)

type HoleFit_ t = SemanticTokensM t [SemanticToken]

-- I would prefer to avoid the duplication between this class and
-- the ToTokens class.
--
-- We might want to override some functionality here. We should perhaps
-- try to find another way to do this.
class ToSemTokens t a where
  toSemTokens :: a -> HoleFit_ t
  default toSemTokens ::
    (SOP.Generic a, All (AnnoFirst a (ToSemTokens t)) (Code a), HasAnno a, ToSemToken t, AnnoToken a ~ t) =>
    a ->
    HoleFit_ t
  toSemTokens =
    genericToNodes (Proxy @(ToSemTokens t)) toSemTokens traverseCsnWithHoles

traverseCsnWithHoles :: (HasCallStack, ToSemToken t) => Anno_ t e -> [HoleFit_ t] -> SemanticTokensM t [SemanticToken]
traverseCsnWithHoles (Anno _ csns) = go csns
  where
    go [] _ = pure []
    go (AnnoHole : cs) holeFits =
      case holeFits of
        [] -> lift $ throwE $ InsufficientHoleFit callStack
        (x : xs) -> (<>) <$> x <*> go cs xs
    go (AnnoCsn m : cs) holeFits = do
      ctx <- ask
      let
        transformSyntaxNode token = toSemToken token <$> ctx.toSemanticToken token <*> ctx.getModifiers token
        thisSyntaxNode = Maybe.mapMaybe transformSyntaxNode (csnTokens m)
      restOfTokens <- go cs holeFits
      pure $ thisSyntaxNode <> restOfTokens

class ToSemToken t where
  toSemToken :: t -> SemanticTokenTypes -> [SemanticTokenModifiers] -> SemanticToken

instance (ToSemTokens t a) => ToSemTokens t [a] where
  toSemTokens =
    Extra.concatMapM toSemTokens

instance (ToSemTokens t a) => ToSemTokens t (Maybe a) where
  toSemTokens =
    maybe (pure []) toSemTokens

withModifier :: (t -> Maybe [SemanticTokenModifiers]) -> SemanticTokensM t a -> SemanticTokensM t a
withModifier f act = do
  local (\i -> i{getModifiers = \t -> f t <|> i.getModifiers t}) act

withTokenType :: (t -> Maybe SemanticTokenTypes) -> SemanticTokensM t a -> SemanticTokensM t a
withTokenType f act = do
  local (\i -> i{toSemanticToken = \t -> f t <|> i.toSemanticToken t}) act

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
-- ----------------------------------------------------------------------------

type HoleFit = HoleFit_ PosToken

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

defaultSemanticTokenCtx :: SemanticTokenCtx PosToken
defaultSemanticTokenCtx =
  SemanticTokenCtx
    { toSemanticToken = simpleTokenType
    , getModifiers = \_ -> pure []
    }

parameterType :: PosToken -> Maybe SemanticTokenTypes
parameterType t = case Lexer.posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Parameter
  _ -> Nothing

nameIsDirective :: PosToken -> Maybe SemanticTokenTypes
nameIsDirective t = case Lexer.posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Interface
  _ -> Nothing

enumType :: PosToken -> Maybe SemanticTokenTypes
enumType t = case Lexer.posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Enum
  _ -> Nothing

defVar :: PosToken -> Maybe [SemanticTokenModifiers]
defVar t = case Lexer.posTokenCategory t.payload of
  CIdentifier -> Just [SemanticTokenModifiers_Declaration, SemanticTokenModifiers_Definition]
  _ -> Nothing

srcPosToPosition :: SrcPos -> Position
srcPosToPosition s =
  Position
    { _character = fromIntegral s.column - 1
    , _line = fromIntegral s.line - 1
    }

-- ----------------------------------------------------------------------------
-- Simala AST to Semantic Tokens
-- ----------------------------------------------------------------------------

instance ToSemToken PosToken where
  toSemToken :: PosToken -> SemanticTokenTypes -> [SemanticTokenModifiers] -> SemanticToken
  toSemToken token category modifiers =
    SemanticToken
      { start = srcPosToPosition token.range.start
      , length = fromIntegral token.range.length
      , category = category
      , modifiers = modifiers
      }

deriving anyclass instance ToSemTokens PosToken (Program Name)

-- Generic instance does not apply because we exclude the level and override
-- the token type for the name.
instance ToSemTokens PosToken (Section Name) where
  toSemTokens (MkSection ann _lvl name decls) =
    traverseCsnWithHoles ann [withTokenType nameIsDirective $ toSemTokens name, toSemTokens decls]

deriving anyclass instance ToSemTokens PosToken (TopDecl Name)
deriving anyclass instance ToSemTokens PosToken (Assume Name)
deriving anyclass instance ToSemTokens PosToken (Declare Name)
deriving anyclass instance ToSemTokens PosToken (TypeDecl Name)
deriving anyclass instance ToSemTokens PosToken (ConDecl Name)
deriving anyclass instance ToSemTokens PosToken (Type' Name)
deriving anyclass instance ToSemTokens PosToken (TypedName Name)
deriving anyclass instance ToSemTokens PosToken (OptionallyTypedName Name)
deriving anyclass instance ToSemTokens PosToken (OptionallyNamedType Name)
deriving anyclass instance ToSemTokens PosToken (Decide Name)
deriving anyclass instance ToSemTokens PosToken (AppForm Name)
deriving anyclass instance ToSemTokens PosToken (Expr Name)
deriving anyclass instance ToSemTokens PosToken (NamedExpr Name)
deriving anyclass instance ToSemTokens PosToken (Branch Name)
deriving anyclass instance ToSemTokens PosToken (Pattern Name)
deriving anyclass instance ToSemTokens PosToken (TypeSig Name)
deriving anyclass instance ToSemTokens PosToken (GivethSig Name)
deriving anyclass instance ToSemTokens PosToken (GivenSig Name)
deriving anyclass instance ToSemTokens PosToken (Directive Name)

instance ToSemTokens PosToken Int where
  toSemTokens _ = pure []

instance ToSemTokens PosToken Name where
  toSemTokens (MkName ann _) =
    traverseCsnWithHoles ann []

instance ToSemTokens PosToken RawName where
  toSemTokens _ = pure []

instance ToSemTokens PosToken Lit where
  toSemTokens (NumericLit ann _) =
    traverseCsnWithHoles ann []
  toSemTokens (StringLit ann _) =
    traverseCsnWithHoles ann []
