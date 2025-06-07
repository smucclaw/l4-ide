{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Api (
  module Backend.Api,
) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Types as Aeson
import Data.Bifunctor (bimap)
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Scientific as Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextReader
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Optics.Cons
import Servant.API

type FunctionName = Text

data FnLiteral
  = FnLitInt !Integer
  | FnLitDouble !Double
  | FnLitBool !Bool
  | FnLitString !Text
  | FnArray [FnLiteral]
  | FnObject [(Text, FnLiteral)]
  | FnUncertain
  | FnUnknown
  deriving (Show, Read, Ord, Eq, Generic)

instance ToJSON FnLiteral where
  toJSON = \ case
    FnLitInt val -> String $ tshow val
    FnLitDouble val -> String $ tshow val
    FnLitBool val -> String $ tshow val
    FnLitString val -> String val
    FnArray vals -> Array $ V.fromList $ fmap toJSON vals
    FnObject ps -> Object $ Aeson.fromList $ fmap (bimap Aeson.fromText toJSON) ps
    FnUncertain -> Object $ Aeson.fromList []
    FnUnknown -> Null
   where
    tshow :: forall a. (Show a) => a -> Text
    tshow = Text.pack . show

instance FromJSON FnLiteral where
  parseJSON = \ case
    String val -> pure $ parseTextAsFnLiteral val
    Bool val -> pure $ FnLitBool val
    Number val
      | Just (i :: Int) <- Scientific.toBoundedInteger val -> pure $ FnLitInt $ fromIntegral i
      | Right d <- Scientific.toBoundedRealFloat val -> pure $ FnLitDouble d
      | otherwise -> Aeson.typeMismatch "Failed to parse number into bounded real or integer" (Number val)
    Null -> pure FnUnknown
    Array vals -> FnArray <$> traverse parseJSON (Foldable.toList vals)
    Object o
      | [] <- Aeson.toList o -> pure FnUncertain
      | otherwise -> do
          ps <- traverse (\(k, v) -> fmap (Aeson.toText k,) (parseJSON v)) (Aeson.toList o)
          pure $ FnObject ps

data RunFunction = RunFunction
  { -- | Run a function with parameters
    runFunction ::
      [(Text, Maybe FnLiteral)] ->
      -- ^ Parameters to the function
      Maybe (Set Text) ->
      -- ^ Output filter, as the function may return a record of
      -- outputs.
      -- If this filter is 'Nothing', we do not filter anything.
      ExceptT EvaluatorError IO ResponseWithReason
  }

-- | A Function declaration is the definition of a function from the
-- perspective of this decision REST API.
data FunctionDeclaration = FunctionDeclaration
  { name :: !Text
  -- ^ Name of the function.
  -- Used in the interpreter of the various backends.
  , description :: !Text
  -- ^ A description of the function.
  , longNames :: !(Set Text)
  -- ^ A set of parameter names which the function can be called with.
  , nameMapping :: !(Map Text Text)
  -- ^ How to translate 'short' names to their 'long' counter part.
  }

data ResponseWithReason = ResponseWithReason
  { values :: [(Text, FnLiteral)]
  , reasoning :: Reasoning
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Wrap our reasoning into a top-level field.
data Reasoning = Reasoning
  { payload :: ReasoningTree
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

emptyTree :: Reasoning
emptyTree =
  Reasoning
    { payload =
        ReasoningTree
          { payload =
              ReasonNode
                { exampleCode = []
                , explanation = []
                }
          , children = []
          }
    }

-- | Basically a rose tree, but serialisable to json and specialised to our purposes.
data ReasoningTree = ReasoningTree
  { payload :: ReasonNode
  , children :: [ReasoningTree]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReasonNode = ReasonNode
  { exampleCode :: [Text]
  , explanation :: [Text]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | An 'EvaluatorError' is some form of panic thrown by an evaluator.
-- The execution of a function had to be interrupted for /some/ reason.
-- Such an exception is unrecoverable.
-- The error message may contain hints of what might have gone wrong.
data EvaluatorError
  = InterpreterError !Text
  | RequiredParameterMissing !ParameterMismatch
  | UnknownArguments ![Text]
  | CannotHandleParameterType !FnLiteral
  | CannotHandleUnknownVars
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ParameterMismatch = ParameterMismatch
  { expected :: !Int
  , actual :: !Int
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData FnLiteral where
  parseQueryParam t = Right $ parseTextAsFnLiteral t

parseTextAsFnLiteral :: Text -> FnLiteral
parseTextAsFnLiteral t
  | Right (d, "") <- TextReader.signed TextReader.decimal t = FnLitInt d
  | Right (d, "") <- TextReader.double t = FnLitDouble d
  | Just b <- parseAsBool = FnLitBool b
  | Just st <- stripQuotes = FnLitString st
  | otherwise = FnLitString t
 where
  parseAsBool = case Text.toLower t of
    "true" -> Just True
    "false" -> Just False
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

  stripQuotes = do
    ('\"', t') <- uncons t
    (t'', '\"') <- unsnoc t'
    pure t''
