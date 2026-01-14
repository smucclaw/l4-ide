{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Api (
  module Backend.Api,
  TraceLevel (..),
  StateGraphFormat (..),
  StateGraphInfo (..),
  StateGraphListResponse (..),
) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
import Data.OpenApi (ToSchema)

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
    -- Use proper JSON types for symmetric ToJSON/FromJSON roundtripping
    FnLitInt val -> Number $ fromIntegral val
    FnLitDouble val -> Number $ Scientific.fromFloatDigits val
    FnLitBool val -> Bool val
    FnLitString val -> String val
    FnArray vals -> Array $ V.fromList $ fmap toJSON vals
    FnObject ps -> Object $ Aeson.fromList $ fmap (bimap Aeson.fromText toJSON) ps
    FnUncertain -> Object $ Aeson.fromList []
    FnUnknown -> Null

instance FromJSON FnLiteral where
  parseJSON = \ case
    -- JSON Strings should remain strings, not be coerced to numbers
    -- (Type coercion is only for query parameters via FromHttpApiData)
    String val -> pure $ FnLitString val
    Bool val -> pure $ FnLitBool val
    Number val
      -- Check if it's an integer (no fractional part) - supports arbitrary precision
      | Scientific.isInteger val -> pure $ FnLitInt $ Scientific.coefficient val * (10 ^ Scientific.base10Exponent val)
      -- Otherwise it's a floating point number
      | Right d <- Scientific.toBoundedRealFloat val -> pure $ FnLitDouble d
      | otherwise -> Aeson.typeMismatch "Failed to parse number into bounded real" (Number val)
    Null -> pure FnUnknown
    Array vals -> FnArray <$> traverse parseJSON (Foldable.toList vals)
    Object o
      | [] <- Aeson.toList o -> pure FnUncertain
      | otherwise -> do
          ps <- traverse (\(k, v) -> fmap (Aeson.toText k,) (parseJSON v)) (Aeson.toList o)
          pure $ FnObject ps

-- | Control how much trace information to return
data TraceLevel
  = TraceNone  -- ^ No trace, result only
  | TraceFull  -- ^ Full evaluation trace
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromHttpApiData TraceLevel where
  parseQueryParam t = case Text.toLower t of
    "none" -> Right TraceNone
    "full" -> Right TraceFull
    _ -> Left $ "Invalid trace level: " <> t <> ". Expected: none, full"

instance ToHttpApiData TraceLevel where
  toQueryParam TraceNone = "none"
  toQueryParam TraceFull = "full"

data EvalBackend
  = JL4
  deriving ()
  deriving stock (Show, Eq, Ord, Enum, Read, Bounded, Generic)

instance FromHttpApiData EvalBackend where
  parseQueryParam = \case
    "jl4" -> Right JL4
    "JL4" -> Right JL4
    other -> Left $ "Invalid eval backend: " <> other <> ". Expected: JL4"

instance ToJSON EvalBackend where
  toJSON = Aeson.String . \case
    JL4 -> "jl4"

instance FromJSON EvalBackend where
  parseJSON (Aeson.String s) =
    case Text.toLower s of
      "jl4" -> pure JL4
      o -> Aeson.prependFailure "EvalBackend" (Aeson.typeMismatch "String" $ Aeson.String o)
  parseJSON o = Aeson.prependFailure "EvalBackend" (Aeson.typeMismatch "String" o)

instance ToJSONKey EvalBackend

instance FromJSONKey EvalBackend

data FnArguments = FnArguments
  { fnEvalBackend :: Maybe EvalBackend
  , fnArguments :: Map Text (Maybe FnLiteral)
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype RunFunction = RunFunction
  { -- | Run a function with parameters
    runFunction ::
      [(Text, Maybe FnLiteral)] ->
      -- ^ Parameters to the function
      Maybe (Set Text) ->
      -- ^ Output filter, as the function may return a record of
      -- outputs.
      -- If this filter is 'Nothing', we do not filter anything.
      TraceLevel ->
      -- ^ Control whether to return full trace or just result
      Bool ->
      -- ^ Include GraphViz DOT output (only meaningful when trace level is full)
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

data GraphVizResponse = GraphVizResponse
  { dot :: Text
    -- ^ Raw DOT text for callers who want to render or post-process traces themselves.
  , png :: Maybe Text
    -- ^ Relative URL to the PNG endpoint (e.g. @/functions/foo/evaluation/trace.png@).
    --   The server regenerates the image on every request so evaluation responses stay pure.
  , svg :: Maybe Text
    -- ^ Relative URL to the SVG endpoint (same contract as 'png').
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ResponseWithReason = ResponseWithReason
  { values :: [(Text, FnLiteral)]
  , reasoning :: Reasoning
  , graphviz :: Maybe GraphVizResponse
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype PngImage = PngImage {unPngImage :: BS.ByteString}

instance MimeRender OctetStream PngImage where
  mimeRender _ (PngImage bs) = BL.fromStrict bs

instance MimeUnrender OctetStream PngImage where
  mimeUnrender _ lbs = Right $ PngImage (BL.toStrict lbs)

-- | Wrap our reasoning into a top-level field.
newtype Reasoning = Reasoning
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

-- ----------------------------------------------------------------------------
-- State Graph Types
-- ----------------------------------------------------------------------------

-- | Format for state graph output
data StateGraphFormat
  = StateGraphDot  -- ^ GraphViz DOT text
  | StateGraphSvg  -- ^ SVG image
  | StateGraphPng  -- ^ PNG image
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromHttpApiData StateGraphFormat where
  parseQueryParam t = case Text.toLower t of
    "dot" -> Right StateGraphDot
    "svg" -> Right StateGraphSvg
    "png" -> Right StateGraphPng
    _ -> Left $ "Invalid state graph format: " <> t <> ". Expected: dot, svg, png"

instance ToHttpApiData StateGraphFormat where
  toQueryParam StateGraphDot = "dot"
  toQueryParam StateGraphSvg = "svg"
  toQueryParam StateGraphPng = "png"

-- | Basic information about a state graph
data StateGraphInfo = StateGraphInfo
  { graphName :: Text
  , graphDescription :: Maybe Text
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Response for listing all state graphs in a module
newtype StateGraphListResponse = StateGraphListResponse
  { graphs :: [StateGraphInfo]
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
