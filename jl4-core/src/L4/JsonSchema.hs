module L4.JsonSchema (
  generateJsonSchema,
  typeToJsonSchema,
  JsonSchema (..),
  SchemaType (..),
  SchemaContext (..),
  emptyContext,
) where

import Base
import qualified Base.Text as Text
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map

import L4.Export (ExportedFunction (..), ExportedParam (..))
import L4.Syntax
import Optics

data JsonSchema = JsonSchema
  { schemaVersion :: !Text
  , schemaTitle :: !Text
  , schemaDescription :: !(Maybe Text)
  , schemaType :: !SchemaType
  , schemaDefs :: !(Map Text SchemaType)
  }
  deriving stock (Eq, Show)

data SchemaType
  = SString !(Maybe Text)
  | SNumber !(Maybe Text)
  | SInteger !(Maybe Text)
  | SBoolean !(Maybe Text)
  | SNull
  | SArray !SchemaType !(Maybe Text)
  | SObject ![(Text, SchemaType, Bool)] !(Maybe Text)
  | SRef !Text !(Maybe Text)
  | SOneOf ![SchemaType] !(Maybe Text)
  | SEnum ![Text] !(Maybe Text)
  | SConst !Text !(Maybe Text)
  | SAnyOf ![SchemaType] !(Maybe Text)
  | SStringFormat !Text !(Maybe Text)
  deriving stock (Eq, Show)

data SchemaContext = SchemaContext
  { ctxDeclares :: !(Map Text (Declare Resolved))
  , ctxCollectedDefs :: !(Map Text SchemaType)
  }
  deriving stock (Show)

emptyContext :: SchemaContext
emptyContext =
  SchemaContext
    { ctxDeclares = Map.empty
    , ctxCollectedDefs = Map.empty
    }

instance ToJSON JsonSchema where
  toJSON schema =
    Aeson.object $
      [ "$schema" .= schema.schemaVersion
      , "title" .= schema.schemaTitle
      ]
        <> maybe [] (\d -> ["description" .= d]) schema.schemaDescription
        <> schemaTypeToFields schema.schemaType
        <> ( if Map.null schema.schemaDefs
              then []
              else ["$defs" .= Map.mapWithKey (\_ v -> schemaTypeToValue v) schema.schemaDefs]
           )

schemaTypeToValue :: SchemaType -> Aeson.Value
schemaTypeToValue st =
  Aeson.object $ schemaTypeToFields st

schemaTypeToFields :: SchemaType -> [(Aeson.Key, Aeson.Value)]
schemaTypeToFields = \case
  SString desc -> typeField "string" <> descField desc
  SNumber desc -> typeField "number" <> descField desc
  SInteger desc -> typeField "integer" <> descField desc
  SBoolean desc -> typeField "boolean" <> descField desc
  SNull -> typeField "null"
  SArray items desc ->
    typeField "array"
      <> ["items" .= schemaTypeToValue items]
      <> descField desc
  SObject props desc ->
    let (propFields, reqFields) =
          foldr
            ( \(name, ty, req) (ps, rs) ->
                ( (Key.fromText name, schemaTypeToValue ty) : ps
                , if req then name : rs else rs
                )
            )
            ([], [])
            props
     in typeField "object"
          <> ["properties" .= Aeson.Object (KeyMap.fromList propFields)]
          <> (if null reqFields then [] else ["required" .= reqFields])
          <> descField desc
  SRef refName desc ->
    ["$ref" .= ("#/$defs/" <> refName)]
      <> descField desc
  SOneOf options desc ->
    ["oneOf" .= map schemaTypeToValue options]
      <> descField desc
  SEnum values desc ->
    typeField "string"
      <> ["enum" .= values]
      <> descField desc
  SConst value desc ->
    ["const" .= value]
      <> descField desc
  SAnyOf options desc ->
    ["anyOf" .= map schemaTypeToValue options]
      <> descField desc
  SStringFormat fmt desc ->
    typeField "string"
      <> ["format" .= fmt]
      <> descField desc
 where
  typeField :: Text -> [(Aeson.Key, Aeson.Value)]
  typeField t = ["type" .= (t :: Text)]

  descField :: Maybe Text -> [(Aeson.Key, Aeson.Value)]
  descField = maybe [] (\d -> ["description" .= d])

generateJsonSchema :: SchemaContext -> ExportedFunction -> JsonSchema
generateJsonSchema ctx export =
  let (paramSchema, finalDefs) = buildParamsSchema ctx export.exportParams
   in JsonSchema
        { schemaVersion = "https://json-schema.org/draft/2020-12/schema"
        , schemaTitle = export.exportName
        , schemaDescription =
            if Text.null export.exportDescription
              then Nothing
              else Just export.exportDescription
        , schemaType = paramSchema
        , schemaDefs = finalDefs
        }

buildParamsSchema :: SchemaContext -> [ExportedParam] -> (SchemaType, Map Text SchemaType)
buildParamsSchema ctx params =
  let (props, defs) = foldr addParam ([], ctx.ctxCollectedDefs) params
   in (SObject props Nothing, defs)
 where
  addParam :: ExportedParam -> ([(Text, SchemaType, Bool)], Map Text SchemaType) -> ([(Text, SchemaType, Bool)], Map Text SchemaType)
  addParam param (acc, defs) =
    case param.paramType of
      Nothing ->
        ((param.paramName, SString param.paramDescription, param.paramRequired) : acc, defs)
      Just ty ->
        let (schema, newDefs) = typeToJsonSchema ctx{ctxCollectedDefs = defs} ty
            schemaWithDesc = addDescription schema param.paramDescription
         in ((param.paramName, schemaWithDesc, param.paramRequired) : acc, newDefs)

  addDescription :: SchemaType -> Maybe Text -> SchemaType
  addDescription st Nothing = st
  addDescription st (Just desc) = case st of
    SRef refName _ -> SRef refName (Just desc)
    SString _ -> SString (Just desc)
    SNumber _ -> SNumber (Just desc)
    SInteger _ -> SInteger (Just desc)
    SBoolean _ -> SBoolean (Just desc)
    SArray items _ -> SArray items (Just desc)
    SObject props _ -> SObject props (Just desc)
    SOneOf opts _ -> SOneOf opts (Just desc)
    SEnum vals _ -> SEnum vals (Just desc)
    SConst val _ -> SConst val (Just desc)
    SAnyOf opts _ -> SAnyOf opts (Just desc)
    SStringFormat fmt _ -> SStringFormat fmt (Just desc)
    SNull -> SNull

typeToJsonSchema :: SchemaContext -> Type' Resolved -> (SchemaType, Map Text SchemaType)
typeToJsonSchema ctx = \case
  Type _ -> (SString Nothing, ctx.ctxCollectedDefs)
  TyApp _ name args ->
    let typeName = resolvedToText name
     in case (typeName, args) of
          ("Number", []) -> (SNumber Nothing, ctx.ctxCollectedDefs)
          ("String", []) -> (SString Nothing, ctx.ctxCollectedDefs)
          ("Boolean", []) -> (SBoolean Nothing, ctx.ctxCollectedDefs)
          ("Integer", []) -> (SInteger Nothing, ctx.ctxCollectedDefs)
          ("Date", []) -> (SStringFormat "date" Nothing, ctx.ctxCollectedDefs)
          ("DateTime", []) -> (SStringFormat "date-time" Nothing, ctx.ctxCollectedDefs)
          ("List", [inner]) ->
            let (innerSchema, defs) = typeToJsonSchema ctx inner
             in (SArray innerSchema Nothing, defs)
          ("Optional", [inner]) ->
            let (innerSchema, defs) = typeToJsonSchema ctx inner
             in (SAnyOf [innerSchema, SNull] Nothing, defs)
          ("Maybe", [inner]) ->
            let (innerSchema, defs) = typeToJsonSchema ctx inner
             in (SAnyOf [innerSchema, SNull] Nothing, defs)
          _ ->
            case Map.lookup typeName ctx.ctxDeclares of
              Just decl ->
                if Map.member typeName ctx.ctxCollectedDefs
                  then (SRef typeName Nothing, ctx.ctxCollectedDefs)
                  else
                    let placeholder = Map.insert typeName (SString Nothing) ctx.ctxCollectedDefs
                        ctx' = ctx{ctxCollectedDefs = placeholder}
                        (declSchema, defs) = declareToJsonSchema ctx' decl
                        finalDefs = Map.insert typeName declSchema defs
                     in (SRef typeName Nothing, finalDefs)
              Nothing ->
                (SRef typeName Nothing, ctx.ctxCollectedDefs)
  Fun _ _ retTy ->
    typeToJsonSchema ctx retTy
  Forall _ _ inner ->
    typeToJsonSchema ctx inner
  InfVar _ _ _ ->
    (SString Nothing, ctx.ctxCollectedDefs)

declareToJsonSchema :: SchemaContext -> Declare Resolved -> (SchemaType, Map Text SchemaType)
declareToJsonSchema ctx (MkDeclare ann _ _ typeDecl) =
  let desc = fmap getDesc (ann ^. annDesc)
   in case typeDecl of
        RecordDecl _ _ fields ->
          let (props, defs) = foldr (addField ctx) ([], ctx.ctxCollectedDefs) fields
           in (SObject props desc, defs)
        EnumDecl _ constructors ->
          enumToJsonSchema ctx desc constructors
        SynonymDecl _ ty ->
          let (schema, defs) = typeToJsonSchema ctx ty
           in (addDescToSchema schema desc, defs)
 where
  addField :: SchemaContext -> TypedName Resolved -> ([(Text, SchemaType, Bool)], Map Text SchemaType) -> ([(Text, SchemaType, Bool)], Map Text SchemaType)
  addField ctx' (MkTypedName fieldAnn fieldName fieldTy) (acc, defs) =
    let fieldNameText = resolvedToText fieldName
        fieldDesc = fmap getDesc (fieldAnn ^. annDesc)
        (fieldSchema, newDefs) = typeToJsonSchema ctx'{ctxCollectedDefs = defs} fieldTy
        schemaWithDesc = addDescToSchema fieldSchema fieldDesc
     in ((fieldNameText, schemaWithDesc, True) : acc, newDefs)

  addDescToSchema :: SchemaType -> Maybe Text -> SchemaType
  addDescToSchema st Nothing = st
  addDescToSchema st (Just d) = case st of
    SRef refName _ -> SRef refName (Just d)
    SString _ -> SString (Just d)
    SNumber _ -> SNumber (Just d)
    SInteger _ -> SInteger (Just d)
    SBoolean _ -> SBoolean (Just d)
    SArray items _ -> SArray items (Just d)
    SObject props _ -> SObject props (Just d)
    SOneOf opts _ -> SOneOf opts (Just d)
    SEnum vals _ -> SEnum vals (Just d)
    SConst val _ -> SConst val (Just d)
    SAnyOf opts _ -> SAnyOf opts (Just d)
    SStringFormat fmt _ -> SStringFormat fmt (Just d)
    SNull -> SNull

enumToJsonSchema :: SchemaContext -> Maybe Text -> [ConDecl Resolved] -> (SchemaType, Map Text SchemaType)
enumToJsonSchema ctx desc constructors =
  let hasPayloads = any hasFields constructors
   in if hasPayloads
        then enumWithPayloads ctx desc constructors
        else enumSimple ctx desc constructors
 where
  hasFields (MkConDecl _ _ fields) = not (null fields)

enumSimple :: SchemaContext -> Maybe Text -> [ConDecl Resolved] -> (SchemaType, Map Text SchemaType)
enumSimple ctx desc constructors =
  let values = map (\(MkConDecl _ name _) -> resolvedToText name) constructors
   in (SEnum values desc, ctx.ctxCollectedDefs)

enumWithPayloads :: SchemaContext -> Maybe Text -> [ConDecl Resolved] -> (SchemaType, Map Text SchemaType)
enumWithPayloads ctx' desc constructors =
  let (options, defs) = foldr buildOption ([], ctx'.ctxCollectedDefs) constructors
   in (SOneOf options desc, defs)
 where
  buildOption :: ConDecl Resolved -> ([SchemaType], Map Text SchemaType) -> ([SchemaType], Map Text SchemaType)
  buildOption (MkConDecl _ name fields) (acc, defs) =
    let tagName = resolvedToText name
        tagProp = ("tag", SConst tagName Nothing, True)
        (fieldProps, newDefs) = foldr (addConField ctx'{ctxCollectedDefs = defs}) ([], defs) fields
        allProps = tagProp : fieldProps
        objSchema = SObject allProps Nothing
     in (objSchema : acc, newDefs)

  addConField :: SchemaContext -> TypedName Resolved -> ([(Text, SchemaType, Bool)], Map Text SchemaType) -> ([(Text, SchemaType, Bool)], Map Text SchemaType)
  addConField ctx'' (MkTypedName fieldAnn fieldName fieldTy) (acc, defs) =
    let fieldNameText = resolvedToText fieldName
        fieldDesc = fmap getDesc (fieldAnn ^. annDesc)
        (fieldSchema, newDefs) = typeToJsonSchema ctx''{ctxCollectedDefs = defs} fieldTy
        schemaWithDesc = case fieldDesc of
          Nothing -> fieldSchema
          Just d -> case fieldSchema of
            SRef refName _ -> SRef refName (Just d)
            SString _ -> SString (Just d)
            SNumber _ -> SNumber (Just d)
            SInteger _ -> SInteger (Just d)
            SBoolean _ -> SBoolean (Just d)
            SArray items _ -> SArray items (Just d)
            SObject props _ -> SObject props (Just d)
            SOneOf opts _ -> SOneOf opts (Just d)
            SEnum vals _ -> SEnum vals (Just d)
            SConst val _ -> SConst val (Just d)
            SAnyOf opts _ -> SAnyOf opts (Just d)
            SStringFormat fmt _ -> SStringFormat fmt (Just d)
            SNull -> SNull
     in ((fieldNameText, schemaWithDesc, True) : acc, newDefs)

resolvedToText :: Resolved -> Text
resolvedToText =
  rawNameToText . rawName . getActual
