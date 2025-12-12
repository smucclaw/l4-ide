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
) where

import Base

import Control.Applicative ((<|>))
import qualified Base.Text as Text
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import L4.Annotation (getAnno)
import L4.Syntax
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
  in collectSection typeDescMap section
 where
  collectSection tdm (MkSection _ _ _ decls) =
    decls >>= collectDecl tdm

  collectDecl tdm = \ case
    Decide _ dec -> maybeToList (buildExportedFunction tdm dec)
    Section _ sub -> collectSection tdm sub
    _ -> []

getDefaultFunction :: Module Resolved -> Maybe ExportedFunction
getDefaultFunction =
  List.find isDefaultExport . getExportedFunctions
 where
  isDefaultExport ExportedFunction{exportIsDefault = flag} = flag

buildExportedFunction :: TypeDescMap -> Decide Resolved -> Maybe ExportedFunction
buildExportedFunction typeDescMap decide@(MkDecide _ tySig appForm _) = do
  desc <- getAnno decide ^. annDesc
  let parsed = parseDescText (getDesc desc)
  guard (parsed.flags.isExport)
  pure
    ExportedFunction
      { exportName = resolvedToText (extractAppFormName appForm)
      , exportDescription = parsed.description
      , exportIsDefault = parsed.flags.isDefault
      , exportParams = extractParams typeDescMap tySig
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
