{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.FunctionSchema (
  Parameters (..),
  Parameter (..),
  declaresFromModule,
  typeToParameter,
  parametersFromDecide,
  parametersFromDecideWithErrors,
) where

import Base
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import L4.Export (extractAssumeParamTypes, extractImplicitAssumeParams)
import L4.Syntax
import L4.TypeCheck.Types (CheckErrorWithContext)
import qualified Optics

data Parameters = MkParameters
  { parameterMap :: Map Text Parameter
  , required :: [Text]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data Parameter = Parameter
  { parameterType :: !Text
  , parameterAlias :: !(Maybe Text)
  , parameterFormat :: !(Maybe Text) -- Format hint (e.g., "date" for YYYY-MM-DD, "date-time" for ISO 8601)
  , parameterEnum :: ![Text]
  , parameterDescription :: !Text
  , parameterProperties :: !(Maybe (Map Text Parameter)) -- Nested properties for object types
  , parameterPropertyOrder :: !(Maybe [Text]) -- Field order for object types (declaration order when available)
  , parameterItems :: !(Maybe Parameter) -- Array items schema for array types
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

instance ToJSON Parameters where
  toJSON (MkParameters props reqProps) =
    Aeson.object
      [ "type" .= Aeson.String "object"
      , "properties" .= props
      , "required" .= reqProps
      ]

instance FromJSON Parameters where
  parseJSON = Aeson.withObject "Parameters" $ \o -> do
    _ :: Text <- o .: "type"
    props <- o .: "properties"
    reqProps <- o .: "required"
    pure $ MkParameters props reqProps

instance ToJSON Parameter where
  toJSON p =
    Aeson.object $
      [ "type" .= p.parameterType
      , "alias" .= p.parameterAlias
      , "enum" .= p.parameterEnum
      , "description" .= p.parameterDescription
      ]
        ++ case p.parameterFormat of
          Nothing -> []
          Just fmt -> ["format" .= fmt]
        ++ case p.parameterProperties of
          Nothing -> []
          Just props -> ["properties" .= props]
        ++ case p.parameterPropertyOrder of
          Nothing -> []
          Just ord -> ["propertyOrder" .= ord]
        ++ case p.parameterItems of
          Nothing -> []
          Just items -> ["items" .= items]

instance FromJSON Parameter where
  parseJSON = Aeson.withObject "Parameter" $ \p ->
    Parameter
      <$> p .: "type"
      <*> p .:? "alias"
      <*> p .:? "format"
      <*> p .:? "enum" .!= []
      <*> p .: "description"
      <*> p .:? "properties"
      <*> p .:? "propertyOrder"
      <*> p .:? "items"

declaresFromModule :: Module Resolved -> Map Text (Declare Resolved)
declaresFromModule (MkModule _ _ section) =
  Map.fromList (collectSection section)
 where
  collectSection (MkSection _ _ _ decls) =
    decls >>= collectDecl

  collectDecl = \case
    Declare _ decl@(MkDeclare _ _ (MkAppForm _ name _ _) _) ->
      [(resolvedNameText name, decl)]
    Section _ sub ->
      collectSection sub
    _ ->
      []

typeToParameter ::
  Map Text (Declare Resolved) ->
  Set.Set Text ->
  Type' Resolved ->
  Parameter
typeToParameter declares visited ty =
  case ty of
    Type _ -> emptyParam "object"
    TyApp _ name [] ->
      typeNameToParameter name
    TyApp _ name [inner] ->
      let lowered = Text.toLower (resolvedNameText name)
       in
        if lowered `elem` ["list", "listof"]
          then
            (emptyParam "array")
              { parameterItems = Just (typeToParameter declares visited inner)
              }
          else
            if lowered `elem` ["maybe", "optional"]
              then typeToParameter declares visited inner
              else typeNameToParameter name
    TyApp _ name _ ->
      typeNameToParameter name
    Fun{} -> emptyParam "object"
    Forall _ _ inner -> typeToParameter declares visited inner
    InfVar{} -> emptyParam "object"
 where
  emptyParam :: Text -> Parameter
  emptyParam t =
    Parameter
      { parameterType = t
      , parameterAlias = Nothing
      , parameterFormat = Nothing
      , parameterEnum = []
      , parameterDescription = ""
      , parameterProperties = Nothing
      , parameterPropertyOrder = Nothing
      , parameterItems = Nothing
      }

  typeNameToParameter :: Resolved -> Parameter
  typeNameToParameter name =
    case primitiveJsonType name of
      Just (t, fmt) -> (emptyParam t) { parameterFormat = fmt }
      Nothing ->
        case Map.lookup (resolvedNameText name) declares of
          Nothing -> emptyParam "object"
          Just decl -> declareToParameter (resolvedNameText name) decl

  -- | Map L4 primitive type names to JSON Schema type and optional format.
  primitiveJsonType :: Resolved -> Maybe (Text, Maybe Text)
  primitiveJsonType name =
    case Text.toLower (resolvedNameText name) of
      "number" -> Just ("number", Nothing)
      "int" -> Just ("number", Nothing)
      "integer" -> Just ("number", Nothing)
      "float" -> Just ("number", Nothing)
      "double" -> Just ("number", Nothing)
      "boolean" -> Just ("boolean", Nothing)
      "bool" -> Just ("boolean", Nothing)
      "string" -> Just ("string", Nothing)
      "text" -> Just ("string", Nothing)
      "date" -> Just ("string", Just "date")
      _ -> Nothing

  declareToParameter :: Text -> Declare Resolved -> Parameter
  declareToParameter typeName (MkDeclare declAnn _ _ typeDecl)
    | typeName `Set.member` visited =
        emptyParam "object"
    | otherwise =
        case typeDecl of
          RecordDecl _ _ fields ->
            let
              visited' = Set.insert typeName visited
              fieldOrder = [resolvedNameText fieldName | MkTypedName _ fieldName _ <- fields]
              props =
                Map.fromList
                  [ (resolvedNameText fieldName, addDesc fieldDesc (typeToParameter declares visited' fieldTy))
                  | MkTypedName fieldAnn fieldName fieldTy <- fields
                  , let fieldDesc = fmap getDesc (fieldAnn Optics.^. annDesc)
                  ]
             in
              (emptyParam "object")
                { parameterDescription = Maybe.fromMaybe "" (fmap getDesc (declAnn Optics.^. annDesc))
                , parameterProperties = Just props
                , parameterPropertyOrder = Just fieldOrder
                }
          EnumDecl _ constructors ->
            (emptyParam "string")
              { parameterDescription = Maybe.fromMaybe "" (fmap getDesc (declAnn Optics.^. annDesc))
              , parameterEnum = [resolvedNameText c | MkConDecl _ c _ <- constructors]
              }
          SynonymDecl _ inner ->
            typeToParameter declares (Set.insert typeName visited) inner
   where
    addDesc :: Maybe Text -> Parameter -> Parameter
    addDesc Nothing p = p
    addDesc (Just d) p = p {parameterDescription = d}

parametersFromDecide :: Module Resolved -> Decide Resolved -> Parameters
parametersFromDecide resolvedModule decide =
  parametersFromDecideWithErrors resolvedModule decide []

-- | Extract parameters from a DECIDE, including implicit ASSUMEs from type errors.
-- When a variable is used without an explicit ASSUME declaration, but its type
-- can be inferred (e.g., from usage like `temperature + 0`), the type checker
-- records an OutOfScopeError with the inferred type. If that type is fully
-- resolved (no inference variables), we treat it as an implicit ASSUME.
parametersFromDecideWithErrors
  :: Module Resolved
  -> Decide Resolved
  -> [CheckErrorWithContext]
  -> Parameters
parametersFromDecideWithErrors resolvedModule decide@(MkDecide _ (MkTypeSig _ (MkGivenSig _ names) _) _ _) errors =
  let
    declares = declaresFromModule resolvedModule
    mkOne (MkOptionallyTypedName ann resolved mType) =
      let
        base =
          Maybe.fromMaybe (emptyParam "object") (typeToParameter declares Set.empty <$> mType)
        desc = Maybe.fromMaybe "" (fmap getDesc (ann Optics.^. annDesc))
       in
        (resolvedNameText resolved, base {parameterDescription = desc, parameterAlias = Nothing})

    -- Extract explicit ASSUME params that this function references
    assumeParams = extractAssumeParamTypes resolvedModule decide

    -- Extract implicit ASSUME params from OutOfScopeError with resolved types
    implicitParams = extractImplicitAssumeParams errors

    mkAssumeParam (name, ty) =
      let base = typeToParameter declares Set.empty ty
      in (name, base {parameterDescription = "", parameterAlias = Nothing})

    givenParamList = map mkOne names
    assumeParamList = map mkAssumeParam assumeParams
    implicitParamList = map mkAssumeParam implicitParams

    -- Combine all params, avoiding duplicates (explicit ASSUMEs take precedence)
    allAssumeParams = assumeParamList <> filter (\(n, _) -> n `notElem` map fst assumeParamList) implicitParamList
   in
    MkParameters
      { parameterMap = Map.fromList (givenParamList <> allAssumeParams)
      , required = map fst givenParamList <> map fst allAssumeParams
      }
 where
  emptyParam :: Text -> Parameter
  emptyParam t =
    Parameter
      { parameterType = t
      , parameterAlias = Nothing
      , parameterFormat = Nothing
      , parameterEnum = []
      , parameterDescription = ""
      , parameterProperties = Nothing
      , parameterPropertyOrder = Nothing
      , parameterItems = Nothing
      }

resolvedNameText :: Resolved -> Text
resolvedNameText =
  rawNameToText . rawName . getActual
