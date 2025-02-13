module LSP.L4.Viz.VizExpr where

import Autodocodec
import Autodocodec.Aeson ()
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Tuple.Optics
import GHC.Generics (Generic)
import Optics
import qualified Data.List.NonEmpty as NE

newtype VisualizeDecisionLogicIRInfo = MkVisualizeDecisionLogicIRInfo
  { program :: IRExpr
  }
  deriving newtype (Eq)
  deriving stock (Show, Generic)

data IRExpr
  = And ID [IRExpr]
  | Or ID [IRExpr]
  | BoolVar ID Text BoolValue
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

instance HasCodec IRExpr where
  codec = object "IRExpr" $ discriminatedUnionCodec "$type" enc dec
    where
      -- Encoder: maps Haskell constructors to (tag, codec)
      enc = \case
        And uid args -> ("And", mapToEncoder (uid, args) naryExprCodec)
        Or uid args -> ("Or", mapToEncoder (uid, args) naryExprCodec)
        BoolVar uid name value -> ("BoolVar", mapToEncoder (uid, name, value) boolVarCodec)

      -- Decoder: maps tag to (constructor name, codec)
      dec =
        HashMap.fromList
          [ ("And", ("And", mapToDecoder (uncurry And) naryExprCodec)),
            ("Or", ("Or", mapToDecoder (uncurry Or) naryExprCodec)),
            ("BoolVar", ("BoolVar", mapToDecoder (\(uid, value, name) -> BoolVar uid value name) boolVarCodec))
          ]

      -- Codec for 'And' and 'Or' expressions.
      naryExprCodec =
        (,)
          <$> requiredField' "id" .= fst
          <*> requiredField' "args" .= snd

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