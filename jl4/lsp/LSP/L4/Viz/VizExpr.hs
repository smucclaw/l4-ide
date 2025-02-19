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

newtype VisualizeDecisionLogicIRInfo = MkVisualizeDecisionLogicIRInfo
  { program :: IRDecl -- TODO: change the fieldname once this becomes more stable
  }
  deriving newtype (Eq)
  deriving stock (Show, Generic)

type Unique = Int
-- | Analogous to, but much simpler than, L4.Syntax's Name
data Name = MkName
  { unique :: Unique   -- ^ for checking whether, e.g., two BoolVar IRNodes actually refer to the same proposition.
  , label  :: Text     -- ^ Label to be displayed in the visualizer.
  }
  deriving (Show, Eq, Generic)

type IRDecl = FunDecl

-- TODO: Will worry about adding param type info later
data FunDecl = MkFunDecl ID Name [Name] IRExpr
  deriving (Show, Eq, Generic)

data IRExpr
  = And ID [IRExpr]
  | Or ID [IRExpr]
  | Not ID IRExpr
  | BoolVar ID Name BoolValue
  deriving (Show, Eq, Generic)

newtype ID = MkID
  { id :: Int
  }
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

data BoolValue = FalseV | TrueV | UnknownV
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
instance HasCodec BoolValue where
  codec = stringConstCodec $ NE.fromList [(FalseV, "False"), (TrueV, "True"), (UnknownV, "Unknown")]

-- Related examples
-- https://github.com/NorfairKing/autodocodec/blob/e939442995debec6d0e014bfcc45449b3a2cb6e6/autodocodec-api-usage/src/Autodocodec/Usage.hs#L688
-- https://github.com/NorfairKing/autodocodec/blob/e939442995debec6d0e014bfcc45449b3a2cb6e6/autodocodec-api-usage/src/Autodocodec/Usage.hs#L740

instance HasCodec IRDecl where
  codec = object "IRDecl" $ discriminatedUnionCodec "$type" enc dec
    where
      enc (MkFunDecl uid name params body) = ("FunDecl", mapToEncoder (uid, name, params, body) funDeclCodec)
      dec = HashMap.fromList
        [ ("FunDecl", ("FunDecl", mapToDecoder (\(uid, name, params, body) -> MkFunDecl uid name params body) funDeclCodec))
        ]
      funDeclCodec =
        (,,,)
          <$> requiredField' "id" .= view _1
          <*> requiredField' "name" .= view _2
          <*> requiredField' "params" .= view _3
          <*> requiredField' "body" .= view _4

instance HasCodec IRExpr where
  codec = object "IRExpr" $ discriminatedUnionCodec "$type" enc dec
    where
      -- Encoder: maps Haskell constructors to (tag, codec)
      enc = \case
        And uid args -> ("And", mapToEncoder (uid, args) naryExprCodec)
        Or uid args -> ("Or", mapToEncoder (uid, args) naryExprCodec)
        Not uid expr -> ("Not", mapToEncoder (uid, expr) notExprCodec)
        BoolVar uid name value -> ("BoolVar", mapToEncoder (uid, name, value) boolVarCodec)

      -- Decoder: maps tag to (constructor name, codec)
      dec =
        HashMap.fromList
          [ ("And", ("And", mapToDecoder (uncurry And) naryExprCodec)),
            ("Or", ("Or", mapToDecoder (uncurry Or) naryExprCodec)),
            ("Not", ("Not", mapToDecoder (uncurry Not) notExprCodec)),
            ("BoolVar", ("BoolVar", mapToDecoder (\(uid, name, value) -> BoolVar uid name value) boolVarCodec))
          ]

      -- Codec for 'And' and 'Or' expressions.
      naryExprCodec =
        (,)
          <$> requiredField' "id" .= fst
          <*> requiredField' "args" .= snd

      notExprCodec =
        (,)
          <$> requiredField' "id" .= fst
          <*> requiredField' "negand" .= snd

      boolVarCodec =
        (,,)
          <$> requiredField' "id" .= view _1
          <*> requiredField' "name" .= view _2
          <*> requiredField' "value" .= view _3

instance HasCodec VisualizeDecisionLogicIRInfo where
  codec =
    named "VisualizeDecisionLogicIRInfo" $
      object "VisualizeDecisionLogicIRInfo" $
        MkVisualizeDecisionLogicIRInfo
          <$> requiredField' "program" .= view #program

-------------------------------------------------------------
-- To/FromJSON Instances via Autodocodec
-------------------------------------------------------------

deriving via (Autodocodec ID) instance ToJSON ID
deriving via (Autodocodec ID) instance FromJSON ID

deriving via (Autodocodec BoolValue) instance ToJSON BoolValue
deriving via (Autodocodec BoolValue) instance FromJSON BoolValue

deriving via (Autodocodec IRExpr) instance ToJSON IRExpr
deriving via (Autodocodec IRExpr) instance FromJSON IRExpr

deriving via (Autodocodec VisualizeDecisionLogicIRInfo) instance ToJSON VisualizeDecisionLogicIRInfo
deriving via (Autodocodec VisualizeDecisionLogicIRInfo) instance FromJSON VisualizeDecisionLogicIRInfo

{-
Am trying out autodocodec because I wanted to see if
being able to create a json schema might help with automatedly checking that
the Ladder backend VizExpr types are in sync with the corresponding types on the frontend
(since the lib I use for the types on the frontend can also automatically generate a json schema).

I haven't actually tried setting up such a automated test though.

If that doesn't end up panning out, I will switch back to aeson.
-}
