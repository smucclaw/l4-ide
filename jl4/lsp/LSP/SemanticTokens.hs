{-# LANGUAGE DefaultSignatures #-}

module LSP.SemanticTokens where

import L4.Annotation
import Base

import Control.Applicative (Alternative (..))
import Control.Lens hiding (Iso)
import qualified Control.Monad.Extra as Extra
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import GHC.Stack
import Generics.SOP as SOP
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types hiding (Pattern)

-- ----------------------------------------------------------------------------
-- Semantic Tokens Interface
-- ----------------------------------------------------------------------------

type SemanticTokensM t = ReaderT (SemanticTokenCtx t) (Except TraverseAnnoError)

type HoleFit_ t = SemanticTokensM t [SemanticToken]

runSemanticTokensM :: ToSemTokens t a => SemanticTokenCtx t -> a -> Either TraverseAnnoError [SemanticToken]
runSemanticTokensM semTokenCtx a = Except.runExcept $ ReaderT.runReaderT (toSemTokens a) semTokenCtx

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
  toSemTokens = genericToSemTokens

genericToSemTokens :: forall a t .
    (SOP.Generic a, All (AnnoFirst a (ToSemTokens t)) (Code a), HasAnno a, ToSemToken t, AnnoToken a ~ t) =>
    a ->
    HoleFit_ t
genericToSemTokens =
  genericToNodes (Proxy @(ToSemTokens t)) toSemTokens traverseCsnWithHoles

traverseCsnWithHoles :: (HasCallStack, ToSemToken t) => Anno_ t e -> [HoleFit_ t] -> SemanticTokensM t [SemanticToken]
traverseCsnWithHoles (Anno _ _ []) _ = pure []
traverseCsnWithHoles (Anno e mSrcRange (AnnoHole _ : cs)) holeFits = case holeFits of
  [] -> lift $ throwE $ InsufficientHoleFit callStack
  (x : xs) -> do
    toks <- x
    restOfTokens <- traverseCsnWithHoles (Anno e mSrcRange cs) xs
    pure $ toks <> restOfTokens
traverseCsnWithHoles (Anno e mSrcRange (AnnoCsn _ m: cs)) xs = do
  ctx <- ask
  let
    thisSyntaxNode = Maybe.mapMaybe (fromSemanticTokenContext ctx) (allClusterTokens m)

  restOfTokens <- traverseCsnWithHoles (Anno e mSrcRange cs) xs
  pure $ concatMap toList thisSyntaxNode <> restOfTokens

fromSemanticTokenContext :: ToSemToken t => SemanticTokenCtx t -> t -> Maybe (NonEmpty SemanticToken)
fromSemanticTokenContext ctx token = toSemToken token <$> ctx.semanticTokenType token <*> ctx.semanticTokenModifier token

class ToSemToken t where
  -- | 'toSemToken' possibly returns multiple tokens, to accommodate for clients
  -- that do not support tokens that span multiple lines
  toSemToken :: t -> SemanticTokenTypes -> [SemanticTokenModifiers] -> NonEmpty SemanticToken

instance (ToSemTokens t a) => ToSemTokens t [a] where
  toSemTokens =
    Extra.concatMapM toSemTokens

instance (ToSemTokens t a) => ToSemTokens t (Maybe a) where
  toSemTokens =
    maybe (pure []) toSemTokens

data SemanticTokenCtx p = SemanticTokenCtx
  { semanticTokenType :: p -> Maybe SemanticTokenTypes
  , semanticTokenModifier :: p -> Maybe [SemanticTokenModifiers]
  }

withModifier :: (t -> Maybe [SemanticTokenModifiers]) -> SemanticTokensM t a -> SemanticTokensM t a
withModifier f act = do
  local (\i -> i{semanticTokenModifier = \t -> f t <|> i.semanticTokenModifier t}) act

withTokenType :: (t -> Maybe SemanticTokenTypes) -> SemanticTokensM t a -> SemanticTokensM t a
withTokenType f act = do
  local (\i -> i{semanticTokenType = \t -> f t <|> i.semanticTokenType t}) act

data SemanticToken = SemanticToken
  { start :: Position
  , length :: UInt
  , category :: SemanticTokenTypes
  , modifiers :: [SemanticTokenModifiers]
  }
  deriving stock (Show, Eq, Ord, Generics.Generic)
  deriving anyclass (NFData)

toSemanticTokenAbsolute :: SemanticToken -> SemanticTokenAbsolute
toSemanticTokenAbsolute s =
  SemanticTokenAbsolute
    { _line = s.start ^. J.line
    , _startChar = s.start ^. J.character
    , _length = s.length
    , _tokenType = s.category
    , _tokenModifiers = s.modifiers
    }

-- | Split a 'SemanticToken' into multiple 'SemanticToken' based on
-- newlines.
-- Not all LSP clients support 'SemanticToken's spanning over multiple
-- lines. Since we want to display them correctly nevertheless, we split them
-- here into multiple 'SemanticToken's.
splitTokens :: SemanticToken -> Text -> NonEmpty SemanticToken
splitTokens s t =
  case Text.lines t of
    [] -> pure s
    (x:xs) -> snd $ List.mapAccumL go s.start (x :| xs)
  where
    nextLine p =
      p
        { _character = 0
        , _line = p ^. J.line + 1
        }

    mkSemanticTokenForLine startPos line = SemanticToken
        { start = startPos
        , length = fromIntegral $ Text.length line
        , category = s.category
        , modifiers = s.modifiers
        }

    go pos line =
      (nextLine pos, mkSemanticTokenForLine pos line)
