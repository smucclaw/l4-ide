{-# LANGUAGE OverloadedStrings #-}

-- | Function schema emission for compiled L4 WASM modules.
--
-- A compiled @.wasm@ bundle is only useful to an external caller if
-- they know (a) what L4 functions are exported, (b) what parameter
-- names/types each function expects, and (c) how those JSON property
-- names map back to the WASM symbol table.
--
-- This module produces a sidecar @.schema.json@ file with *exactly*
-- the same parameter schema format the jl4-service HTTP API uses. A
-- compiled WASM module can therefore be dropped into any system that
-- already talks to jl4-service — the input/output JSON format is
-- identical.
--
-- Canonical sanitization and DEONTIC parameter injection match
-- @jl4-service/src/Shared.hs@ and @jl4-service/src/Compiler.hs@.
module L4.MLIR.Schema
  ( -- * Schema bundle
    WasmBundle (..)
  , FunctionExport (..)
  , bundleExports

    -- * Name sanitization (must match jl4-service)
  , sanitizePropertyName
  , sanitizeFunctionName
  , sanitizeWasmSymbol

    -- * JSON emission
  , encodeBundle
  , writeBundleFile
  ) where

import Data.Aeson ((.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import L4.Syntax
  ( Module, Resolved, Type'(..), Declare
  , rawName, rawNameToText, getActual, getUnique
  )
import L4.Export (ExportedFunction(..), ExportedParam(..), getExportedFunctions)
import L4.FunctionSchema
  ( Parameters(..), Parameter(..), declaresFromModule, typeToParameter
  )
import L4.TypeCheck.Environment (contractUnique)

-- ---------------------------------------------------------------------------
-- Bundle types
-- ---------------------------------------------------------------------------

-- | One exported L4 function in the compiled WASM module.
--
-- @apiName@ is the sanitized name that external callers use in JSON
-- (spaces → hyphens, collapsed, stripped). @wasmSymbol@ is the actual
-- C-style symbol exported from the @.wasm@ binary (alphanumeric +
-- underscore only, since wasm-ld names must be valid linker symbols).
data FunctionExport = FunctionExport
  { apiName     :: !Text
  , wasmSymbol  :: !Text
  , description :: !Text
  , parameters  :: !Parameters
  , returnType  :: !Text           -- ^ Display string (e.g., "NUMBER", "BOOLEAN", "DEONTIC")
  , isDeontic   :: !Bool
  , paramOrder  :: ![Text]         -- ^ API names in declaration order (for positional marshal)
  }

-- | Complete schema for a compiled @.wasm@ module.
data WasmBundle = WasmBundle
  { bundleWasmFile :: !Text        -- ^ Relative path to the .wasm binary
  , bundleVersion  :: !Text        -- ^ Content-derived version hash
  , bundleExports  :: ![FunctionExport]
  }

-- ---------------------------------------------------------------------------
-- Name sanitization — must match jl4-service exactly
-- ---------------------------------------------------------------------------

-- | Sanitize a property name for use as a JSON property key.
-- Matches jl4-service\/src\/Shared.hs exactly.
sanitizePropertyName :: Text -> Text
sanitizePropertyName name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '.' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
  in if Text.null s' then "_unnamed" else s'
 where
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

-- | Sanitize a function name for use as an API/HTTP identifier.
-- Same rule as property names (spaces → hyphens, collapsed).
sanitizeFunctionName :: Text -> Text
sanitizeFunctionName = sanitizePropertyName

-- | Sanitize a name for use as a WASM export symbol (@func.func \@name@).
-- WASM/LLVM linker symbols must be C identifiers: alphanumeric + underscore,
-- not starting with a digit. We replace the sanitized property name's
-- hyphens with underscores to produce a valid symbol.
sanitizeWasmSymbol :: Text -> Text
sanitizeWasmSymbol name =
  let sanitized = sanitizePropertyName name
      withUnderscores = Text.map (\c -> if c == '-' || c == '.' then '_' else c) sanitized
  in if Text.null withUnderscores
       then "_unnamed"
       else case Text.uncons withUnderscores of
         Just (c, _) | not (isAlphaNum c) && c /= '_' -> "_" <> withUnderscores
         Just (c, _) | c >= '0' && c <= '9' -> "_" <> withUnderscores
         _ -> withUnderscores

-- ---------------------------------------------------------------------------
-- Bundle construction from a resolved L4 module
-- ---------------------------------------------------------------------------

-- | Build the schema bundle for all @\@export@\-ed functions in a module.
-- The bundle is then serialized to disk as @.schema.json@.
bundleExports :: Text -> Text -> Module Resolved -> WasmBundle
bundleExports wasmPath version resolvedModule =
  let declares = declaresFromModule resolvedModule
      exports  = getExportedFunctions resolvedModule
  in WasmBundle
       { bundleWasmFile = wasmPath
       , bundleVersion  = version
       , bundleExports  = map (buildExport declares) exports
       }

buildExport :: Map Text (Declare Resolved) -> ExportedFunction -> FunctionExport
buildExport declares ef =
  let name        = ef.exportName
      isDeonticFn = case ef.exportReturnType of
        Just ty -> isDeonticType ty
        Nothing -> False

      -- Build the parameter map. Keys stay in their original L4 form
      -- (spaces, backticks, etc.) exactly like jl4-service does, so
      -- @remapArguments@ can translate sanitized incoming keys back to
      -- original ones at request time.
      baseParams  = buildParamsFromExported declares ef.exportParams
      paramsWithDeontic = if isDeonticFn
        then addDeonticParameters ef.exportReturnType baseParams
        else baseParams

      -- Parameter order in declaration order, using *original* L4 names.
      paramOrder_ = map (.paramName) ef.exportParams
                 ++ (if isDeonticFn then ["startTime", "events"] else [])
  in FunctionExport
       { apiName     = sanitizeFunctionName name
       , wasmSymbol  = sanitizeWasmSymbol name
       , description = Text.strip ef.exportDescription
       , parameters  = paramsWithDeontic
       , returnType  = returnTypeDisplay ef.exportReturnType
       , isDeontic   = isDeonticFn
       , paramOrder  = paramOrder_
       }

-- | Convert an ExportedParam to a FunctionSchema Parameter entry.
buildParamsFromExported :: Map Text (Declare Resolved) -> [ExportedParam] -> Parameters
buildParamsFromExported declares params =
  let entries =
        [ (p.paramName, paramToParameter declares p)
        | p <- params
        ]
      required = [p.paramName | p <- params, p.paramRequired]
  in MkParameters
       { parameterMap = Map.fromList entries
       , required     = required
       }

paramToParameter :: Map Text (Declare Resolved) -> ExportedParam -> Parameter
paramToParameter declares p =
  let base = case p.paramType of
        Nothing -> emptyParam "object"
        Just ty -> typeToParameter declares Set.empty ty
  in base
       { parameterAlias       = Nothing
       , parameterDescription = maybe "" Text.strip p.paramDescription
       }

emptyParam :: Text -> Parameter
emptyParam t = Parameter
  { parameterType          = t
  , parameterAlias         = Nothing
  , parameterFormat        = Nothing
  , parameterEnum          = []
  , parameterDescription   = ""
  , parameterProperties    = Nothing
  , parameterPropertyOrder = Nothing
  , parameterItems         = Nothing
  , parameterRequired      = Nothing
  }


-- ---------------------------------------------------------------------------
-- DEONTIC parameter injection — mirrors jl4-service/src/Compiler.hs
-- ---------------------------------------------------------------------------

-- | Is this a DEONTIC (contract) return type?
isDeonticType :: Type' Resolved -> Bool
isDeonticType (TyApp _ name [_, _]) = getUnique name == contractUnique
isDeonticType _ = False

-- | For DEONTIC-returning functions, the HTTP API injects two extra
-- parameters: @startTime@ (number) and @events@ (array of
-- @{party, action, at}@ objects). We must add these to the schema
-- for compatibility with the service.
addDeonticParameters :: Maybe (Type' Resolved) -> Parameters -> Parameters
addDeonticParameters mReturnType (MkParameters props req) =
  let (partyParam, actionParam) = case mReturnType of
        Just (TyApp _ _ [partyTy, actionTy]) ->
          ( (genericDeonticParam "The party performing the action") { parameterType = paramTypeOf partyTy }
          , (genericDeonticParam "The action performed")             { parameterType = paramTypeOf actionTy }
          )
        _ ->
          ( genericDeonticParam "The party performing the action"
          , genericDeonticParam "The action performed"
          )
      startTimeParam = (emptyParam "number")
        { parameterDescription = "Start time for contract simulation" }
      eventsParam = (emptyParam "array")
        { parameterDescription = "Events for contract simulation (each: {party, action, at})"
        , parameterItems = Just $ (emptyParam "object")
            { parameterDescription = "A trace event"
            , parameterProperties = Just $ Map.fromList
                [ ("party", partyParam)
                , ("action", actionParam)
                , ("at", (emptyParam "number") { parameterDescription = "Timestamp" })
                ]
            , parameterPropertyOrder = Just ["party", "action", "at"]
            , parameterRequired = Just ["party", "action", "at"]
            }
        }
  in MkParameters
       { parameterMap = props
           <> Map.fromList [("startTime", startTimeParam), ("events", eventsParam)]
       , required = req <> ["startTime", "events"]
       }
  where
    paramTypeOf ty = case ty of
      TyApp _ n _ -> case Text.toLower (rawNameToText (rawName (getActual n))) of
        "number"  -> "number"
        "boolean" -> "boolean"
        "bool"    -> "boolean"
        "string"  -> "string"
        "text"    -> "string"
        _         -> "object"
      _ -> "object"

genericDeonticParam :: Text -> Parameter
genericDeonticParam desc = (emptyParam "object") { parameterDescription = desc }

-- ---------------------------------------------------------------------------
-- Return-type display
-- ---------------------------------------------------------------------------

returnTypeDisplay :: Maybe (Type' Resolved) -> Text
returnTypeDisplay Nothing = "unknown"
returnTypeDisplay (Just ty)
  | isDeonticType ty = "DEONTIC"
  | otherwise = case ty of
      TyApp _ n args ->
        let nameText = Text.toUpper (rawNameToText (rawName (getActual n)))
         in if null args
              then nameText
              else nameText <> " " <> Text.intercalate " " (map (returnTypeDisplay . Just) args)
      _ -> "TYPE"

-- ---------------------------------------------------------------------------
-- JSON serialization
-- ---------------------------------------------------------------------------

instance Aeson.ToJSON FunctionExport where
  toJSON fe = Aeson.object
    [ "name"        .= fe.apiName
    , "wasmSymbol"  .= fe.wasmSymbol
    , "description" .= fe.description
    , "parameters"  .= fe.parameters
    , "returnType"  .= fe.returnType
    , "isDeontic"   .= fe.isDeontic
    , "paramOrder"  .= fe.paramOrder
    ]

instance Aeson.FromJSON FunctionExport where
  parseJSON = Aeson.withObject "FunctionExport" $ \o -> FunctionExport
    <$> o .: "name"
    <*> o .: "wasmSymbol"
    <*> o .:? "description" .!= ""
    <*> o .: "parameters"
    <*> o .:? "returnType"  .!= "unknown"
    <*> o .:? "isDeontic"   .!= False
    <*> o .:? "paramOrder"  .!= []

instance Aeson.ToJSON WasmBundle where
  toJSON wb = Aeson.object
    [ "wasmFile" .= wb.bundleWasmFile
    , "version"  .= wb.bundleVersion
    , "functions" .= Aeson.object
        [ (Aeson.Key.fromText fe.apiName, Aeson.toJSON fe)
        | fe <- wb.bundleExports
        ]
    ]

encodeBundle :: WasmBundle -> LBS.ByteString
encodeBundle = Aeson.encode

writeBundleFile :: FilePath -> WasmBundle -> IO ()
writeBundleFile path bundle = LBS.writeFile path (encodeBundle bundle)
