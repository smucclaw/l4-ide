{-# LANGUAGE DataKinds #-}

{-| Custom extensions to the LSP and payload types for JL4
    See also @ts-shared/jl4-client-rpc/custom-protocol.ts@
-}
module LSP.L4.Viz.CustomProtocol where

import qualified Base.Text as T
import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import GHC.TypeLits  (Symbol, KnownSymbol, symbolVal)
import Language.LSP.Protocol.Types as LSP
import LSP.L4.Viz.VizExpr as V

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

------------------------------------------------------
--  l4/evalApp custom method for LSP
------------------------------------------------------

type EvalAppMethodName :: Symbol
type EvalAppMethodName = "l4/evalApp"

------------------------------------------------------
--  IsCustomMethod typeclass
------------------------------------------------------

class KnownSymbol a => IsCustomMethod (a :: Symbol) where
  getMethodName :: Proxy a -> T.Text
  getMethodName = T.pack . symbolVal

instance IsCustomMethod EvalAppMethodName