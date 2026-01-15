module L4.Parser.MixfixRegistry
  ( MixfixHintRegistry (..)
  , emptyMixfixHintRegistry
  , registerMixfixInfo
  , buildMixfixHintRegistry
  , hasMixfixHints
  , isKnownMixfixKeyword
  , showKeywords
  ) where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import L4.Mixfix
import L4.Syntax

data MixfixHintRegistry = MkMixfixHintRegistry
  { byFirstKeyword :: Map RawName [MixfixInfo]
  , keywordUniverse :: Set RawName
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Semigroup MixfixHintRegistry where
  MkMixfixHintRegistry m1 s1 <> MkMixfixHintRegistry m2 s2 =
    MkMixfixHintRegistry
      (Map.unionWith (<>) m1 m2)
      (Set.union s1 s2)

instance Monoid MixfixHintRegistry where
  mempty = emptyMixfixHintRegistry

emptyMixfixHintRegistry :: MixfixHintRegistry
emptyMixfixHintRegistry =
  MkMixfixHintRegistry Map.empty Set.empty

registerMixfixInfo :: MixfixInfo -> MixfixHintRegistry
registerMixfixInfo info =
  case firstKeyword info of
    Nothing -> mempty
    Just kw ->
      MkMixfixHintRegistry
        (Map.singleton kw [info])
        (Set.fromList info.keywords)

hasMixfixHints :: MixfixHintRegistry -> Bool
hasMixfixHints hints = not (Set.null hints.keywordUniverse)

isKnownMixfixKeyword :: RawName -> MixfixHintRegistry -> Bool
isKnownMixfixKeyword kw hints =
  kw `Set.member` hints.keywordUniverse

showKeywords :: MixfixHintRegistry -> [Text]
showKeywords hints = map rawNameToText $ Set.toList hints.keywordUniverse

buildMixfixHintRegistry :: Module Name -> MixfixHintRegistry
buildMixfixHintRegistry =
  foldTopDecls gatherTop
  where
    gatherTop :: TopDecl Name -> MixfixHintRegistry
    gatherTop = \case
      Decide _ dec -> foldDecides gatherDecide dec
      Assume _ ass -> gatherAssume ass
      _ -> mempty

    gatherDecide :: Decide Name -> MixfixHintRegistry
    gatherDecide (MkDecide _ tysig appForm _) =
      maybe mempty registerMixfixInfo (extractMixfixInfo tysig appForm)

    gatherAssume :: Assume Name -> MixfixHintRegistry
    gatherAssume (MkAssume _ tysig appForm _) =
      maybe mempty registerMixfixInfo (extractMixfixInfo tysig appForm)
