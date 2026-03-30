module L4.Mixfix
  ( MixfixPatternToken (..)
  , MixfixInfo (..)
  , extractMixfixInfo
  , givenParamNames
  , firstKeyword
  , canonicalMixfixName
  , buildCanonicalNameFromKeywords
  ) where

import Base
import qualified Base.Set as Set
import qualified Base.Text as Text
import L4.Syntax

-- | A token in a mixfix pattern, representing either a keyword (part of the function name)
-- or a parameter slot.
data MixfixPatternToken
  = MixfixKeyword RawName
    -- ^ A keyword part of the function name (e.g., "is eligible for")
  | MixfixParam RawName
    -- ^ A parameter slot, with the original parameter name for documentation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Information about a mixfix function pattern.
-- A mixfix function is one where the function name is interspersed with parameters,
-- like @person `is eligible for` program@ instead of @isEligibleFor person program@.
data MixfixInfo = MkMixfixInfo
  { pattern :: [MixfixPatternToken]
    -- ^ The complete pattern, e.g., [Param "person", Keyword "is eligible for", Param "program"]
  , keywords :: [RawName]
    -- ^ Just the keyword parts, for quick lookup (e.g., ["is eligible for"])
  , arity :: Int
    -- ^ Number of parameters (parameter slots in the pattern)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Extract parameter names from a TypeSig's GIVEN clause.
givenParamNames :: TypeSig Name -> [RawName]
givenParamNames (MkTypeSig _ givenSig _) =
  case givenSig of
    MkGivenSig _ otns -> map optionallyTypedNameToRawName otns
  where
    optionallyTypedNameToRawName :: OptionallyTypedName Name -> RawName
    optionallyTypedNameToRawName (MkOptionallyTypedName _ n _) = rawName n

-- | Extract mixfix pattern information by comparing the AppForm against
-- the GIVEN parameters in the TypeSig.
extractMixfixInfo :: TypeSig Name -> AppForm Name -> Maybe MixfixInfo
extractMixfixInfo tysig appForm =
  let
    givenParams :: Set RawName
    givenParams = Set.fromList $ givenParamNames tysig

    appFormTokens :: [Name]
    appFormTokens = appFormHead' : appFormArgs'
      where
        MkAppForm _ appFormHead' appFormArgs' _ = appForm

    classifyToken :: Name -> MixfixPatternToken
    classifyToken n
      | rawName n `Set.member` givenParams = MixfixParam (rawName n)
      | otherwise                          = MixfixKeyword (rawName n)

    patternTokens :: [MixfixPatternToken]
    patternTokens = map classifyToken appFormTokens

    extractKeyword :: MixfixPatternToken -> Maybe RawName
    extractKeyword (MixfixKeyword k) = Just k
    extractKeyword (MixfixParam _)   = Nothing

    keywordList :: [RawName]
    keywordList = mapMaybe extractKeyword patternTokens

    isParamToken :: MixfixPatternToken -> Bool
    isParamToken (MixfixParam _)   = True
    isParamToken (MixfixKeyword _) = False

    paramCount :: Int
    paramCount = length $ filter isParamToken patternTokens

    -- | Check if there's a keyword that comes AFTER a parameter.
    -- This distinguishes true mixfix patterns (where keywords are interspersed
    -- with parameters) from prefix functions where the user redundantly listed
    -- GIVEN parameter names after the function name.
    --
    -- Examples:
    --   [Keyword, Param]       -> False (just prefix with redundant param listing)
    --   [Keyword, Param, Param] -> False (same - no keyword after any param)
    --   [Param, Keyword]       -> True  (e.g., "person IS ELIGIBLE")
    --   [Param, Keyword, Param] -> True  (e.g., "person IS ELIGIBLE FOR program")
    --   [Keyword, Param, Keyword] -> True (e.g., "IF cond THEN")
    hasKeywordAfterParam :: Bool
    hasKeywordAfterParam =
      let afterFirstParam = dropWhile (not . isParamToken) patternTokens
          afterParam = drop 1 afterFirstParam  -- skip the first param itself
      in any (not . isParamToken) afterParam

  in
    if paramCount > 0 && hasKeywordAfterParam
       then Just MkMixfixInfo
              { pattern = patternTokens
              , keywords = keywordList
              , arity = paramCount
              }
       else Nothing

-- | First keyword appearing in a mixfix pattern, if any.
firstKeyword :: MixfixInfo -> Maybe RawName
firstKeyword MkMixfixInfo {pattern = toks} =
  listToMaybe [kw | MixfixKeyword kw <- toks]

-- | Build the canonical name for a mixfix function from its pattern.
-- Following the Agda/OBJ convention, parameter slots are replaced with @_@.
--
-- Examples:
--   @[Kw "tax on", Param, Kw "item costing", Param, Kw "as GST in", Param]@
--   becomes @"tax on _ item costing _ as GST in _"@
--
--   @[Param, Kw "is eligible for", Param]@
--   becomes @"_ is eligible for _"@
canonicalMixfixName :: MixfixInfo -> RawName
canonicalMixfixName info =
  NormalName $ Text.intercalate " " $ map tokenToText info.pattern
  where
    tokenToText :: MixfixPatternToken -> Text
    tokenToText (MixfixKeyword kw) = rawNameToText kw
    tokenToText (MixfixParam _)    = "_"

-- | Build a canonical name from a list of keywords observed at a call site,
-- inferring the parameter positions from the pattern structure.
-- The first keyword is passed separately (it was used for the initial registry lookup).
--
-- For a keyword-first pattern with keywords [kw1, kw2, kw3]:
--   produces @"kw1 _ kw2 _ kw3 _"@
--
-- For a param-first pattern with keywords [kw1, kw2]:
--   produces @"_ kw1 _ kw2 _"@
--
-- This is used at call sites to reconstruct the canonical name from the
-- keywords found in the expression, enabling direct registry lookup.
buildCanonicalNameFromKeywords :: Bool -> [RawName] -> RawName
buildCanonicalNameFromKeywords paramFirst keywords =
  NormalName $ Text.intercalate " " parts
  where
    parts
      | paramFirst = concatMap (\kw -> ["_", rawNameToText kw]) keywords ++ ["_"]
      | otherwise  = concatMap (\kw -> [rawNameToText kw, "_"]) keywords
