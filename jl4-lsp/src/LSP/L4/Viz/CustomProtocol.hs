{-# LANGUAGE DataKinds #-}

{-| Custom extensions to the LSP and payload types for JL4
  See also 
  - @ts-shared/jl4-client-rpc/custom-protocol.ts@ for RPC-related TS types
  - @ts-shared/viz-expr/eval-on-backend.ts@ for the params/response payload TS types
-}
module LSP.L4.Viz.CustomProtocol where

import qualified Base.Text as T
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.:), (.=), withObject)
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
--  Custom methods for LSP
------------------------------------------------------

type EvalAppMethodName :: Symbol
type EvalAppMethodName = "l4/evalApp"

type InlineExprsMethodName :: Symbol
type InlineExprsMethodName = "l4/inlineExprs"

------------------------------------------------------
--  IsCustomMethod typeclass
------------------------------------------------------

class KnownSymbol a => IsCustomMethod (a :: Symbol) where
  getMethodName :: Proxy a -> T.Text
  getMethodName = T.pack . symbolVal

instance IsCustomMethod EvalAppMethodName
instance IsCustomMethod InlineExprsMethodName

------------------------------------------------------
--  IsLadderRequestParams typeclass
------------------------------------------------------

class (GHC.Records.HasField "verDocId" params VersionedTextDocumentIdentifier, 
       FromJSON params) 
      => IsLadderRequestParams params where
  
instance IsLadderRequestParams EvalAppRequestParams
instance IsLadderRequestParams InlineExprsRequestParams
