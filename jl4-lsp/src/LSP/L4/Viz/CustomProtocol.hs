{-# LANGUAGE DataKinds #-}

{-| Custom extensions to the LSP and payload types for JL4
  See also 
  - @ts-shared/jl4-client-rpc/custom-protocol.ts@ for RPC-related TS types
  - @ts-shared/viz-expr/eval-on-backend.ts@ for the params/response payload TS types
-}
module LSP.L4.Viz.CustomProtocol where

import qualified Base.Text as T
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.:), (.:?), (.!=), (.=), withObject)
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import GHC.TypeLits  (Symbol, KnownSymbol, symbolVal)
import Language.LSP.Protocol.Types as LSP
import LSP.L4.Viz.VizExpr as V
import GHC.Records (HasField)

------------------------------------------------------
-- l4/evalApp Request Params and Response Payload
------------------------------------------------------

-- | Payload / params for EvalAppRequest
data EvalAppRequestParams = EvalAppRequestParams
  { appExpr  :: V.ID
    {- | I don't want to bother defining a BoolValue just for this;
        can be cleaned up in the future.
        Also, prob better to make the @args@ UBoolLit exprs or smtg
    -}
  , args     :: [V.UBoolValue]
  , verDocId :: LSP.VersionedTextDocumentIdentifier
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | l4/evalApp response payload
newtype EvalAppResult = EvalAppResult
  { value :: V.UBoolValue }
  deriving stock (Eq, Show)

instance ToJSON EvalAppResult where
  toJSON (EvalAppResult val) = object [
    "$type" .= ("EvalAppResult" :: T.Text),
    "value" .= val
    ]

instance FromJSON EvalAppResult where
  parseJSON = withObject "EvalAppResult" $ \obj -> do
    typeTag <- obj .: "$type"
    if typeTag == ("EvalAppResult" :: T.Text)
      then EvalAppResult <$> obj .: "value"
      else fail $ "Expected $type field to be 'EvalAppResult' but got: " <> T.unpack typeTag

------------------------------------------------------
-- l4/inlineExprs Request Params and Response Payload
------------------------------------------------------

-- | Payload / params for InlineExprsRequest
data InlineExprsRequestParams = InlineExprsRequestParams
  { uniques  :: [Int]
  , verDocId :: LSP.VersionedTextDocumentIdentifier
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype InlineExprsResult = MkInlineExprsResult RenderAsLadderInfo
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

------------------------------------------------------
-- l4/queryPlan Request Params
------------------------------------------------------

-- | Payload / params for QueryPlanRequest.
--   The response uses 'L4.Decision.QueryPlan.QueryPlanResponse' directly.
data QueryPlanRequestParams = QueryPlanRequestParams
  { fnName   :: T.Text
  , bindings :: Map T.Text Bool
  , verDocId :: LSP.VersionedTextDocumentIdentifier
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

------------------------------------------------------
-- l4/exportDocument and l4/exportPlan Request Params
------------------------------------------------------

-- | Params for the deterministic document export. All fields except
-- @verDocId@ are optional and default sensibly.
data ExportDocumentParams = ExportDocumentParams
  { verDocId       :: LSP.VersionedTextDocumentIdentifier
  , format         :: T.Text   -- ^ "html" (default) | "text" | "akn" | "json"
  , includeUnused  :: Bool     -- ^ render unreachable imported material too
  , numberSections :: Bool
  , numberClauses  :: Bool
  , toc            :: Bool     -- ^ prepend a table of contents (HTML)
  , excludeModules :: [T.Text] -- ^ module URIs to exclude (deselected imports)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ExportDocumentParams where
  toJSON p = object
    [ "verDocId" .= p.verDocId
    , "format" .= p.format
    , "includeUnused" .= p.includeUnused
    , "numberSections" .= p.numberSections
    , "numberClauses" .= p.numberClauses
    , "toc" .= p.toc
    , "excludeModules" .= p.excludeModules
    ]

instance FromJSON ExportDocumentParams where
  parseJSON = withObject "ExportDocumentParams" $ \o ->
    ExportDocumentParams
      <$> o .:  "verDocId"
      <*> o .:? "format"         .!= "html"
      <*> o .:? "includeUnused"  .!= False
      <*> o .:? "numberSections" .!= False
      <*> o .:? "numberClauses"  .!= False
      <*> o .:? "toc"            .!= False
      <*> o .:? "excludeModules" .!= []

-- | Params for the export plan (imports/rules tree + reachability).
newtype ExportPlanParams = ExportPlanParams
  { verDocId :: LSP.VersionedTextDocumentIdentifier
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ExportPlanParams where
  toJSON p = object ["verDocId" .= p.verDocId]

instance FromJSON ExportPlanParams where
  parseJSON = withObject "ExportPlanParams" $ \o ->
    ExportPlanParams <$> o .: "verDocId"

------------------------------------------------------
--  Custom methods for LSP
------------------------------------------------------

type EvalAppMethodName :: Symbol
type EvalAppMethodName = "l4/evalApp"

type InlineExprsMethodName :: Symbol
type InlineExprsMethodName = "l4/inlineExprs"

type QueryPlanMethodName :: Symbol
type QueryPlanMethodName = "l4/queryPlan"

type ExportDocumentMethodName :: Symbol
type ExportDocumentMethodName = "l4/exportDocument"

type ExportPlanMethodName :: Symbol
type ExportPlanMethodName = "l4/exportPlan"

------------------------------------------------------
--  CustomMethod typeclass
------------------------------------------------------

class KnownSymbol a => CustomMethod (a :: Symbol) where
  getMethodName :: Proxy a -> T.Text
  getMethodName = T.pack . symbolVal

instance CustomMethod EvalAppMethodName
instance CustomMethod InlineExprsMethodName
instance CustomMethod QueryPlanMethodName
instance CustomMethod ExportDocumentMethodName
instance CustomMethod ExportPlanMethodName

------------------------------------------------------
--  LadderRequestParams typeclass
------------------------------------------------------

class (GHC.Records.HasField "verDocId" params VersionedTextDocumentIdentifier,
       FromJSON params)
      => LadderRequestParams params

instance LadderRequestParams EvalAppRequestParams
instance LadderRequestParams InlineExprsRequestParams
instance LadderRequestParams QueryPlanRequestParams
