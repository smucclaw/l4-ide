{-# LANGUAGE DataKinds #-}

{-| Custom LSP protocol types and helpers for the L4 Results Inspector.

The inspector provides structured evaluation results for #EVAL, #EVALTRACE,
#CHECK, #ASSERT directives, enabling rich rendering in the IDE.
-}
module LSP.L4.Inspector where

import Base
import qualified Data.Text as Text

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.:), (.=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import Data.Aeson.Types (Parser)
import Data.Ratio (numerator, denominator)
import GHC.TypeLits (Symbol)
import Language.LSP.Protocol.Types as LSP
import L4.Parser.SrcSpan (SrcPos(..))
import L4.Print (prettyLayout)
import L4.Syntax (getActual)

import qualified L4.EvaluateLazy as EL
import qualified L4.Evaluate.ValueLazy as Val

------------------------------------------------------
-- l4/evalDirectiveResult method name
------------------------------------------------------

type EvalDirectiveResultMethodName :: Symbol
type EvalDirectiveResultMethodName = "l4/evalDirectiveResult"

------------------------------------------------------
-- Request params
------------------------------------------------------

data EvalDirectiveResultParams = EvalDirectiveResultParams
  { verDocId :: LSP.VersionedTextDocumentIdentifier
  , srcPos :: SrcPos
  , directiveType :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EvalDirectiveResultParams where
  toJSON p = object
    [ "verDocId" .= p.verDocId
    , "srcPos" .= srcPosToJson p.srcPos
    , "directiveType" .= p.directiveType
    ]

instance FromJSON EvalDirectiveResultParams where
  parseJSON = withObject "EvalDirectiveResultParams" $ \obj -> do
    verDocId' <- obj .: "verDocId"
    srcPosVal <- obj .: "srcPos"
    srcPos' <- parseSrcPos srcPosVal
    directiveType' <- obj .: "directiveType"
    pure $ EvalDirectiveResultParams verDocId' srcPos' directiveType'

srcPosToJson :: SrcPos -> Aeson.Value
srcPosToJson (MkSrcPos l c) = object ["line" .= l, "column" .= c]

parseSrcPos :: Aeson.Value -> Parser SrcPos
parseSrcPos = withObject "SrcPos" $ \obj ->
  MkSrcPos <$> obj .: "line" <*> obj .: "column"

------------------------------------------------------
-- Response
------------------------------------------------------

data DirectiveResult = DirectiveResult
  { directiveType :: Text
  , prettyText :: Text
  , success :: Maybe Bool
  , structuredValue :: Maybe Aeson.Value
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DirectiveResult where
  toJSON r = object
    [ "directiveType" .= r.directiveType
    , "prettyText" .= r.prettyText
    , "success" .= r.success
    , "structuredValue" .= r.structuredValue
    ]

instance FromJSON DirectiveResult where
  parseJSON = withObject "DirectiveResult" $ \obj ->
    DirectiveResult
      <$> obj .: "directiveType"
      <*> obj .: "prettyText"
      <*> obj .: "success"
      <*> obj .: "structuredValue"

------------------------------------------------------
-- Converting EvalDirectiveResult to DirectiveResult
------------------------------------------------------

evalDirectiveToResult :: Text -> EL.EvalDirectiveResult -> DirectiveResult
evalDirectiveToResult dirType (EL.MkEvalDirectiveResult _range res mtrace) =
  DirectiveResult
    { directiveType = dirType
    , prettyText = EL.prettyEvalDirectiveResult (EL.MkEvalDirectiveResult _range res mtrace)
    , success = case res of
        EL.Assertion b        -> Just b
        EL.Reduction (Right _) -> Just True
        EL.Reduction (Left _)  -> Just False
    , structuredValue = case res of
        EL.Assertion b -> Just (Aeson.toJSON b)
        EL.Reduction (Left _) -> Nothing
        EL.Reduction (Right nf) -> Just (nfToJson nf)
    }

------------------------------------------------------
-- Simple NF-to-JSON converter
------------------------------------------------------

nfToJson :: Val.NF -> Aeson.Value
nfToJson (Val.MkNF v) = valueToJson v
nfToJson Val.Omitted   = Aeson.Null

valueToJson :: Val.Value Val.NF -> Aeson.Value
valueToJson = \case
  Val.ValNumber r ->
    case isInteger r of
      Just i  -> Aeson.toJSON i
      Nothing -> Aeson.toJSON (fromRational r :: Double)
  Val.ValString t -> Aeson.toJSON t
  Val.ValDate day -> Aeson.object ["$type" .= ("Date" :: Text), "value" .= show day]
  Val.ValTime tod -> Aeson.object ["$type" .= ("Time" :: Text), "value" .= show tod]
  Val.ValDateTime utc tz -> Aeson.object ["$type" .= ("DateTime" :: Text), "value" .= show utc, "timezone" .= tz]
  Val.ValNil -> Aeson.toJSON ([] :: [Aeson.Value])
  Val.ValCons hd tl -> Aeson.toJSON (consToList hd tl)
  Val.ValConstructor resolved args ->
    let name = prettyLayout (getActual resolved)
    in case args of
      [] -> case Text.toUpper name of
        "TRUE"  -> Aeson.toJSON True
        "FALSE" -> Aeson.toJSON False
        _       -> Aeson.String name
      _  -> Aeson.object [Aeson.fromText name .= Aeson.toJSON (map nfToJson args)]
  Val.ValUnappliedConstructor resolved -> Aeson.String (prettyLayout (getActual resolved))
  Val.ValAssumed resolved -> Aeson.object ["$assumed" .= prettyLayout (getActual resolved)]
  -- Closures and builtins cannot be meaningfully serialized
  Val.ValClosure{} -> Aeson.String "<function>"
  Val.ValNullaryBuiltinFun{} -> Aeson.String "<builtin>"
  Val.ValUnaryBuiltinFun{} -> Aeson.String "<builtin>"
  Val.ValBinaryBuiltinFun{} -> Aeson.String "<builtin>"
  Val.ValTernaryBuiltinFun{} -> Aeson.String "<builtin>"
  Val.ValPartialTernary{} -> Aeson.String "<partial>"
  Val.ValPartialTernary2{} -> Aeson.String "<partial>"
  Val.ValEnvironment{} -> Aeson.String "<environment>"
  Val.ValObligation{} -> Aeson.String "<obligation>"
  Val.ValROp{} -> Aeson.String "<deferred-op>"
  Val.ValBreached{} -> Aeson.object ["$type" .= ("Breach" :: Text)]

consToList :: Val.NF -> Val.NF -> [Aeson.Value]
consToList hd tl = nfToJson hd : case tl of
  Val.MkNF Val.ValNil       -> []
  Val.MkNF (Val.ValCons h t) -> consToList h t
  other                      -> [nfToJson other]

isInteger :: Rational -> Maybe Integer
isInteger r
  | denominator r == 1 = Just (numerator r)
  | otherwise = Nothing
