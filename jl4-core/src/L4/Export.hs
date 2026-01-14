module L4.Export (
  ExportedFunction (..),
  ExportedParam (..),
  DescFlags (..),
  ParsedDesc (..),
  TypeDescMap,
  parseDescText,
  getExportedFunctions,
  getDefaultFunction,
  buildTypeDescMap,
  assumesFromModule,
  extractAssumeParamTypes,
  extractImplicitAssumeParams,
  hasTypeInferenceVars,
) where

import Base

import Control.Applicative ((<|>))
import qualified Base.Text as Text
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import L4.Annotation (getAnno)
import L4.Syntax
import L4.TypeCheck.Types (CheckErrorWithContext(..), CheckError(..))
import Optics

type TypeDescMap = Map.Map Unique Text

data ExportedFunction = ExportedFunction
  { exportName :: !Text
  , exportDescription :: !Text
  , exportIsDefault :: !Bool
  , exportParams :: ![ExportedParam]
  , exportReturnType :: !(Maybe (Type' Resolved))
  , exportDecide :: !(Decide Resolved)
  }
  deriving stock (Eq, Show)

data ExportedParam = ExportedParam
  { paramName :: !Text
  , paramType :: !(Maybe (Type' Resolved))
  , paramDescription :: !(Maybe Text)
  , paramRequired :: !Bool
  }
  deriving stock (Eq, Show)

data DescFlags = DescFlags
  { isDefault :: !Bool
  , isExport :: !Bool
  }
  deriving stock (Eq, Show)

data ParsedDesc = ParsedDesc
  { flags :: !DescFlags
  , description :: !Text
  }
  deriving stock (Eq, Show)

parseDescText :: Text -> ParsedDesc
parseDescText txt =
  let
    trimmed = Text.strip txt
    (flags', remainder) = consumeKeywords trimmed initialFlags
  in
    ParsedDesc
      { flags = flags'
      , description = Text.strip remainder
      }
 where
  initialFlags =
    DescFlags
      { isDefault = False
      , isExport = False
      }

  consumeKeywords t flagsAcc =
    let current = Text.stripStart t
    in case Text.uncons current of
      Nothing -> (flagsAcc, current)
      Just _ ->
        let (token, rest) = Text.break isSpace current
            restStripped = Text.stripStart rest
        in case Text.toLower token of
          "default" ->
            consumeKeywords
              restStripped
              flagsAcc
                { isDefault = True
                , isExport = True
                }
          "export" ->
            consumeKeywords restStripped flagsAcc{isExport = True}
          _ -> (flagsAcc, current)

getExportedFunctions :: Module Resolved -> [ExportedFunction]
getExportedFunctions mod'@(MkModule _ _ section) =
  let typeDescMap = buildTypeDescMap mod'
      assumes = assumesFromModule mod'
      explicitExports = collectSection typeDescMap assumes section
      allDecides = collectAllDecides section
  in case explicitExports of
    -- No explicit exports: export the topmost function as default
    [] -> case allDecides of
      (firstDecide : _) -> maybeToList (buildImplicitDefaultFunction typeDescMap assumes firstDecide)
      [] -> []
    -- Has explicit exports but no default: mark the topmost as default
    _ | not (any (\ef -> ef.exportIsDefault) explicitExports) ->
        case explicitExports of
          (firstExport : rest) -> firstExport { exportIsDefault = True } : rest
    -- Has explicit exports with default: use as-is
    _ -> explicitExports
 where
  collectSection tdm assumes' (MkSection _ _ _ decls) =
    decls >>= collectDecl tdm assumes'

  collectDecl tdm assumes' = \ case
    Decide _ dec -> maybeToList (buildExportedFunction tdm assumes' dec)
    Section _ sub -> collectSection tdm assumes' sub
    _ -> []

  -- Collect all Decide declarations in source order
  collectAllDecides (MkSection _ _ _ decls) =
    decls >>= \ case
      Decide _ dec -> [dec]
      Section _ sub -> collectAllDecides sub
      _ -> []

  -- Build an implicit default export for a function without @export
  buildImplicitDefaultFunction tdm assumes' decide@(MkDecide _ tySig appForm _) =
    let desc = getAnno decide ^. annDesc
        description = maybe "" getDesc desc
        givenParams = extractParams tdm tySig
        assumedParams = extractAssumedDependencies tdm assumes' decide
    in Just ExportedFunction
        { exportName = resolvedToText (extractAppFormName appForm)
        , exportDescription = description
        , exportIsDefault = True
        , exportParams = givenParams <> assumedParams
        , exportReturnType = extractReturnType tySig
        , exportDecide = decide
        }

getDefaultFunction :: Module Resolved -> Maybe ExportedFunction
getDefaultFunction =
  List.find isDefaultExport . getExportedFunctions
 where
  isDefaultExport ExportedFunction{exportIsDefault = flag} = flag

buildExportedFunction
  :: TypeDescMap
  -> Map.Map Unique (Assume Resolved)
  -> Decide Resolved
  -> Maybe ExportedFunction
buildExportedFunction typeDescMap assumes decide@(MkDecide _ tySig appForm _) = do
  desc <- getAnno decide ^. annDesc
  let parsed = parseDescText (getDesc desc)
  guard (parsed.flags.isExport)
  let givenParams = extractParams typeDescMap tySig
      assumedParams = extractAssumedDependencies typeDescMap assumes decide
  pure
    ExportedFunction
      { exportName = resolvedToText (extractAppFormName appForm)
      , exportDescription = parsed.description
      , exportIsDefault = parsed.flags.isDefault
      , exportParams = givenParams <> assumedParams
      , exportReturnType = extractReturnType tySig
      , exportDecide = decide
      }

extractAppFormName :: AppForm Resolved -> Resolved
extractAppFormName (MkAppForm _ name _ _) = name

extractParams :: TypeDescMap -> TypeSig Resolved -> [ExportedParam]
extractParams typeDescMap (MkTypeSig _ (MkGivenSig _ names) _) =
  fmap toParam names
 where
  toParam (MkOptionallyTypedName ann resolved mType) =
    let paramDesc = fmap getDesc (ann ^. annDesc)
        fallbackDesc = mType >>= getTypeDesc typeDescMap
    in ExportedParam
      { paramName = resolvedToText resolved
      , paramType = mType
      , paramDescription = paramDesc <|> fallbackDesc
      , paramRequired = True
      }

extractReturnType :: TypeSig Resolved -> Maybe (Type' Resolved)
extractReturnType (MkTypeSig _ _ giveth) =
  (\(MkGivethSig _ ty) -> ty) <$> giveth

resolvedToText :: Resolved -> Text
resolvedToText =
  rawNameToText . rawName . getActual

buildTypeDescMap :: Module Resolved -> TypeDescMap
buildTypeDescMap (MkModule _ _ section) =
  Map.fromList (collectSection section)
 where
  collectSection (MkSection _ _ _ decls) =
    decls >>= collectDecl

  collectDecl = \ case
    Declare _ (MkDeclare ann _ (MkAppForm _ name _ _) _) ->
      case ann ^. annDesc of
        Just desc -> [(getUnique name, getDesc desc)]
        Nothing -> []
    Section _ sub -> collectSection sub
    _ -> []

getTypeDesc :: TypeDescMap -> Type' Resolved -> Maybe Text
getTypeDesc typeDescMap = \ case
  TyApp _ name _ -> Map.lookup (getUnique name) typeDescMap
  _ -> Nothing

-- | Collect all non-function ASSUME declarations from a module.
-- Function-typed ASSUMEs are filtered out per design decision.
assumesFromModule :: Module Resolved -> Map.Map Unique (Assume Resolved)
assumesFromModule (MkModule _ _ section) =
  Map.fromList (collectSection section)
 where
  collectSection (MkSection _ _ _ decls) =
    decls >>= collectDecl

  collectDecl = \case
    Assume _ assume@(MkAssume _ _ (MkAppForm _ name _ _) mType) ->
      case mType of
        Just (Fun {}) -> []  -- Skip function-typed ASSUMEs
        _ -> [(getUnique name, assume)]
    Section _ sub -> collectSection sub
    _ -> []

-- | Collect all free variable references (by Unique) from an expression.
-- Uses cosmosOf gplate for recursive traversal of the expression AST.
collectFreeRefs :: Expr Resolved -> Set.Set Unique
collectFreeRefs =
  foldMapOf (cosmosOf (gplate @(Expr Resolved))) $ \case
    App _ (Ref _ uniq _) [] -> Set.singleton uniq
    _ -> Set.empty

-- | Extract ASSUME declarations that are referenced by a DECIDE body.
-- Returns ExportedParams for each ASSUME that the function depends on.
extractAssumedDependencies
  :: TypeDescMap
  -> Map.Map Unique (Assume Resolved)
  -> Decide Resolved
  -> [ExportedParam]
extractAssumedDependencies typeDescMap assumes (MkDecide _ _ _ body) =
  let
    referencedUniques = collectFreeRefs body
    matchingAssumes =
      [ assume
      | (uniq, assume) <- Map.toList assumes
      , Set.member uniq referencedUniques
      ]
  in
    map (assumeToParam typeDescMap) matchingAssumes

-- | Convert an ASSUME declaration to an ExportedParam
assumeToParam :: TypeDescMap -> Assume Resolved -> ExportedParam
assumeToParam typeDescMap (MkAssume ann _ (MkAppForm _ name _ _) mType) =
  let
    paramDesc = fmap getDesc (ann ^. annDesc)
    fallbackDesc = mType >>= getTypeDesc typeDescMap
  in
    ExportedParam
      { paramName = resolvedToText name
      , paramType = mType
      , paramDescription = paramDesc <|> fallbackDesc
      , paramRequired = True
      }

-- | Extract (name, type) pairs for ASSUME declarations referenced by a DECIDE body.
-- Used by CodeGen to generate LET bindings for ASSUME values.
extractAssumeParamTypes
  :: Module Resolved
  -> Decide Resolved
  -> [(Text, Type' Resolved)]
extractAssumeParamTypes mod' (MkDecide _ _ _ body) =
  let
    assumes = assumesFromModule mod'
    referencedUniques = collectFreeRefs body
    matchingAssumes =
      [ assume
      | (uniq, assume) <- Map.toList assumes
      , Set.member uniq referencedUniques
      ]
  in
    mapMaybe assumeToTypeInfo matchingAssumes
 where
  assumeToTypeInfo :: Assume Resolved -> Maybe (Text, Type' Resolved)
  assumeToTypeInfo (MkAssume _ _ (MkAppForm _ name _ _) (Just ty)) =
    Just (resolvedToText name, ty)
  assumeToTypeInfo _ = Nothing

-- | Check if a type contains any unresolved inference variables.
-- Types with inference variables cannot be used for implicit ASSUMEs
-- because their concrete type is not yet known.
hasTypeInferenceVars :: Type' Resolved -> Bool
hasTypeInferenceVars = \case
  Type   _ -> False
  TyApp  _ _n ns -> any hasTypeInferenceVars ns
  Fun    _ opts ty -> any hasNamedTypeInferenceVars opts || hasTypeInferenceVars ty
  Forall _ _ ty -> hasTypeInferenceVars ty
  InfVar {} -> True
 where
  hasNamedTypeInferenceVars :: OptionallyNamedType Resolved -> Bool
  hasNamedTypeInferenceVars (MkOptionallyNamedType _ _ ty) = hasTypeInferenceVars ty

-- | Extract implicit ASSUME parameters from type check errors.
-- When a variable is used without declaration, the type checker records an
-- OutOfScopeError with the inferred type. If the type is fully resolved
-- (no inference variables), we can treat this as an implicit ASSUME.
--
-- This enables programs to omit explicit ASSUME declarations when the type
-- can be inferred from usage context.
--
-- Example: `temperature + 0` forces `temperature :: NUMBER`
-- The OutOfScopeError for `temperature` will have type NUMBER, which we
-- can extract as an implicit ASSUME.
extractImplicitAssumeParams
  :: [CheckErrorWithContext]
  -> [(Text, Type' Resolved)]
extractImplicitAssumeParams errors =
  [ (nameToText name, ty)
  | MkCheckErrorWithContext{kind = OutOfScopeError name ty} <- errors
  , not (hasTypeInferenceVars ty)
  ]
