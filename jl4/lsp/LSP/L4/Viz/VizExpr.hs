{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.L4.Viz.VizExpr where

import Autodocodec
import Autodocodec.Aeson ()
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Tuple.Optics
import GHC.Generics (Generic)
import Optics

newtype VisualizeDecisionLogicIRInfo = MkVisualizeDecisionLogicIRInfo
  { program :: IRExpr
  }
  deriving newtype (Eq)
  deriving stock (Show, Generic)

data IRExpr
  = And ID [IRExpr]
  | Or ID [IRExpr]
  | BoolVar ID BoolValue Text
  deriving (Show, Eq, Generic)

newtype ID = MkID
  { id :: Int
  }
  deriving newtype (Eq)
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
  codec = stringConstCodec [(FalseV, "False"), (TrueV, "True"), (UnknownV, "Unknown")]

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

      -- -- Individual codecs without the "$type" discriminator
      -- -- TODO: There must be a nicer way to do this with record syntax instead of tuples?!
      -- binExprCodec =
      --   (,,)
      --     <$> requiredField' "id" .= view _1
      --     <*> requiredField' "op" .= view _2
      --     <*> requiredField' "args" .= view _3

      boolVarCodec =
        (,,)
          <$> requiredField' "id" .= view _1
          <*> requiredField' "name" .= view _2
          <*> requiredField' "value" .= view _3

-- -- Helper functions
-- uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
-- uncurry3 f (a, b, c) = f a b c

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
