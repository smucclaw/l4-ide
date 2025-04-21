module LSP.L4.Viz.VizExpr where

import Autodocodec
import Autodocodec.Aeson ()
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Tuple.Optics
import GHC.Generics (Generic)
import Optics

newtype RenderAsLadderInfo = MkRenderAsLadderInfo
  { funDecl :: FunDecl -- TODO: change the fieldname once this becomes more stable
  }
  deriving newtype (Eq)
  deriving stock (Show, Generic)

type Unique = Int
-- | TODO: Consider renaming this to something other than `Name`
data Name = MkName
  { unique :: Unique   -- ^ for checking whether, e.g., two UBoolVar IRNodes actually refer to the same proposition.
  , label  :: Text     -- ^ Label to be displayed in the visualizer.
  }
  deriving (Show, Eq, Generic)

-- TODO: Will worry about adding param type info later
data FunDecl = MkFunDecl
  { id     :: ID
  , fnName :: Name
  , params :: [Name]
  , body   :: IRExpr }
  deriving (Show, Eq, Generic)

data IRExpr
  = And ID [IRExpr]
  | Or ID [IRExpr]
  | Not ID IRExpr
  | UBoolVar ID Name UBoolValue
  deriving (Show, Eq, Generic)

newtype ID = MkID
  { id :: Int
  }
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

data UBoolValue = FalseV | TrueV | UnknownV
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- HasCodec instances
--------------------------------------------------------------------------------

instance HasCodec Name where
  codec = object "Name" $
    MkName
      <$> requiredField "unique" "Unique identifier for the name (for equality of expressions)" .= view #unique
      <*> requiredField "label" "Label to be displayed" .= view #label

instance HasCodec ID where
  codec =
    object "ID" $
      MkID
        <$> requiredField "id" "Unique, stable identifier" .= view #id

-- | Corresponds to the Typescript `'False' | 'True' | 'Unknown'`
instance HasCodec UBoolValue where
  codec = stringConstCodec $ NE.fromList [(FalseV, "False"), (TrueV, "True"), (UnknownV, "Unknown")]

-- Related examples
-- https://github.com/NorfairKing/autodocodec/blob/e939442995debec6d0e014bfcc45449b3a2cb6e6/autodocodec-api-usage/src/Autodocodec/Usage.hs#L688
-- https://github.com/NorfairKing/autodocodec/blob/e939442995debec6d0e014bfcc45449b3a2cb6e6/autodocodec-api-usage/src/Autodocodec/Usage.hs#L740

instance HasCodec FunDecl where
  codec = object "FunDecl" $
    typeField "FunDecl" MkFunDecl
      <*> requiredField' "id"     .= view #id
      <*> requiredField' "name"   .= view #fnName
      <*> requiredField' "params" .= view #params
      <*> requiredField' "body"   .= view #body

instance HasCodec IRExpr where
  codec = object "IRExpr" $ discriminatedUnionCodec "$type" enc dec
    where
      -- Encoder: maps Haskell constructors to (tag, codec)
      enc = \case
        And uid args -> ("And", mapToEncoder (uid, args) naryExprCodec)
        Or uid args -> ("Or", mapToEncoder (uid, args) naryExprCodec)
        Not uid expr -> ("Not", mapToEncoder (uid, expr) notExprCodec)
        UBoolVar uid name value -> ("UBoolVar", mapToEncoder (uid, name, value) uBoolVarCodec)

      -- Decoder: maps tag to (constructor name, codec)
      dec =
        HashMap.fromList
          [ ("And", ("And", mapToDecoder (uncurry And) naryExprCodec)),
            ("Or", ("Or", mapToDecoder (uncurry Or) naryExprCodec)),
            ("Not", ("Not", mapToDecoder (uncurry Not) notExprCodec)),
            ("UBoolVar", ("UBoolVar", mapToDecoder mkUBoolVar uBoolVarCodec))
          ]
      mkUBoolVar (uid, name, value) = UBoolVar uid name value

      -- Codec for 'And' and 'Or' expressions.
      naryExprCodec =
        (,)
          <$> requiredField' "id" .= fst
          <*> requiredField' "args" .= snd

      notExprCodec =
        (,)
          <$> requiredField' "id" .= fst
          <*> requiredField' "negand" .= snd

      uBoolVarCodec =
        (,,)
          <$> requiredField' "id" .= view _1
          <*> requiredField' "name" .= view _2
          <*> requiredField' "value" .= view _3

instance HasCodec RenderAsLadderInfo where
  codec =
    named "RenderAsLadderInfo" $
      object "RenderAsLadderInfo" $
        MkRenderAsLadderInfo
          <$> requiredField' "funDecl" .= view #funDecl

-------------------------------------------------------------
-- To/FromJSON Instances via Autodocodec
-------------------------------------------------------------

deriving via (Autodocodec ID) instance ToJSON ID
deriving via (Autodocodec ID) instance FromJSON ID

deriving via (Autodocodec UBoolValue) instance ToJSON UBoolValue
deriving via (Autodocodec UBoolValue) instance FromJSON UBoolValue

deriving via (Autodocodec IRExpr) instance ToJSON IRExpr
deriving via (Autodocodec IRExpr) instance FromJSON IRExpr

deriving via (Autodocodec RenderAsLadderInfo) instance ToJSON RenderAsLadderInfo
deriving via (Autodocodec RenderAsLadderInfo) instance FromJSON RenderAsLadderInfo

{-
Am trying out autodocodec because I wanted to see if
being able to create a json schema might help with automatedly checking that
the Ladder backend VizExpr types are in sync with the corresponding types on the frontend
(since the lib I use for the types on the frontend can also automatically generate a json schema).

I haven't actually tried setting up such a automated test though.

If that doesn't end up panning out, I will switch back to aeson.
-}

-------------------------------------------------------------
-- Utils
-------------------------------------------------------------
{- | Adapted from
https://github.com/NorfairKing/autodocodec/blob/e939442995debec6d0e014bfcc45449b3a2cb6e6/autodocodec-api-usage/src/Autodocodec/Usage.hs#L827
-}
typeField :: Text -> a -> ObjectCodec b a
typeField typeName a =
  a <$ requiredFieldWith' "$type" (literalTextCodec typeName) .= const typeName