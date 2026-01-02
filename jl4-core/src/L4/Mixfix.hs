module L4.Mixfix
  ( MixfixPatternToken (..)
  , MixfixInfo (..)
  , extractMixfixInfo
  , givenParamNames
  , firstKeyword
  ) where

import Base
import qualified Base.Set as Set
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

  in
    if paramCount > 0 && (maybe False isParamToken (listToMaybe patternTokens) || paramCount < length patternTokens)
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
