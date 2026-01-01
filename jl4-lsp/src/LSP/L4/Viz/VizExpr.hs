{-# OPTIONS_GHC -Wno-orphans #-}
module LSP.L4.Viz.VizExpr where

import Autodocodec
import Autodocodec.Aeson ()
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Base.Text (Text)
import Data.Tuple.Optics
import GHC.Generics (Generic)
import Optics
import qualified Language.LSP.Protocol.Types as LSP

data RenderAsLadderInfo = MkRenderAsLadderInfo
  { verDocId  :: LSP.VersionedTextDocumentIdentifier
  , funDecl   :: FunDecl
  }
  deriving stock (Show, Generic, Eq)

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
  | UBoolVar ID Name UBoolValue Bool Text -- ^ id name ubvalue canInline atomId
  | App ID Name [IRExpr] Text
  | TrueE ID Name 
  | FalseE ID Name
  deriving (Show, Eq, Generic)

{- | See  viz-expr-to-lir.ts and ladder.svelte.ts for examples of how the IRIds get used -}
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
  codec = stringConstCodec $ NE.fromList [(FalseV, "FalseV"), (TrueV, "TrueV"), (UnknownV, "UnknownV")]

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
  -- TODO: Look into whether discriminatedUnionCodec really is better than the typeField approach here
  codec = object "IRExpr" $ discriminatedUnionCodec "$type" enc dec
    where
      -- Encoder: maps Haskell constructors to (tag, codec)
      enc = \ case
        And uid args -> ("And", mapToEncoder (uid, args) naryExprCodec)
        Or uid args -> ("Or", mapToEncoder (uid, args) naryExprCodec)
        Not uid expr -> ("Not", mapToEncoder (uid, expr) notExprCodec)
        UBoolVar uid name value canInline atomId -> ("UBoolVar", mapToEncoder (uid, name, value, canInline, atomId) uBoolVarCodec)
        App uid name args atomId -> ("App", mapToEncoder (uid, name, args, atomId) appExprCodec)
        TrueE uid name -> ("TrueE", mapToEncoder (uid, name) boolLitCodec)
        FalseE uid name -> ("FalseE", mapToEncoder (uid, name) boolLitCodec)

      -- Decoder: maps tag to (constructor name, codec)
      dec =
        HashMap.fromList
          [ ("And", ("And", mapToDecoder (uncurry And) naryExprCodec)),
            ("Or", ("Or", mapToDecoder (uncurry Or) naryExprCodec)),
            ("Not", ("Not", mapToDecoder (uncurry Not) notExprCodec)),
            ("UBoolVar", ("UBoolVar", mapToDecoder mkUBoolVar uBoolVarCodec)),
            ("App", ("App", mapToDecoder mkAppExpr appExprCodec)),
            ("TrueE", ("TrueE", mapToDecoder (uncurry TrueE) boolLitCodec)),
            ("FalseE", ("FalseE", mapToDecoder (uncurry FalseE) boolLitCodec))
          ]

      mkUBoolVar (uid, name, value, canInline, atomId) = UBoolVar uid name value canInline atomId
      mkAppExpr (uid, name, args, atomId) = App uid name args atomId

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
        (,,,,)
          <$> requiredField' "id" .= view _1
          <*> requiredField' "name" .= view _2
          <*> requiredField' "value" .= view _3
          <*> requiredField' "canInline" .= view _4
          <*> requiredField' "atomId" .= view _5

      appExprCodec =
        (,,,)
          <$> requiredField' "id" .= view _1
          <*> requiredField' "fnName" .= view _2
          <*> requiredField' "args" .= view _3
          <*> requiredField' "atomId" .= view _4

      boolLitCodec =
        (,)
          <$> requiredField' "id"   .= fst
          <*> requiredField' "name" .= snd

instance HasCodec RenderAsLadderInfo where
  codec =
    named "RenderAsLadderInfo" $
      object "RenderAsLadderInfo" $
        MkRenderAsLadderInfo
          <$> requiredField' "verDocId" .= view #verDocId
          <*> requiredField' "funDecl"  .= view #funDecl

instance HasCodec LSP.VersionedTextDocumentIdentifier where
  codec = codecViaAeson "VersionedTextDocumentIdentifier"

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
