{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Server (
  -- * AppM
  AppM,
  AppEnv (..),
  ValidatedFunction (..),

  -- * Servant
  OperationId,

  -- * REST API
  Api,
  FunctionApi,
  FunctionApi' (..),
  SingleFunctionApi,
  SingleFunctionApi' (..),
  FunctionCrud,
  FunctionCrud' (..),
  handler,

  -- * API json types
  Parameters (..),
  Parameter (..),
  Function (..),
  SimpleFunction (..),
  SimpleResponse (..),
  Reasoning (..),
  ReasoningTree (..),
  ResponseWithReason (..),
  EvaluatorError (..),
  FnLiteral (..),
  EvalBackend (..),
  FunctionImplementation (..),
  FnArguments (..),
  Outcomes (..),
  OutcomeObject (..),
  OutcomeStyle (..),
  BatchRequest (..),
  InputCase (..),
  OutputCase (..),
  BatchResponse (..),
  OutputSummary (..),

  -- * utilities
  toDecl,
) where

import Base
import Backend.Api as Api
import qualified Backend.Jl4 as Jl4

import qualified Chronos
import Control.Applicative
import Control.Concurrent.STM
import Control.Exception (evaluate, displayException)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.:), (.:?), (.=), (.!=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Combinators.Decode (Decoder)
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import Data.Fixed
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Tuple.Extra as Tuple
import qualified GHC.Clock as Clock
import GHC.TypeLits
import Servant
import System.Timeout (timeout)
import Servant.Client.Core.HasClient

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant.Client (BaseUrl, ClientError, ClientM, runClientM, mkClientEnv)
import qualified L4.CRUD as CRUD
import Servant.Client.Generic (genericClient)
import Network.HTTP.Client (Manager)
import qualified Base.Text as T
import L4.Syntax
import L4.Print (prettyLayout)
import Data.Function
import qualified Optics
import L4.Lexer


-- ----------------------------------------------------------------------------
-- Servant API
-- ----------------------------------------------------------------------------

data AppEnv = MkAppEnv
  { functionDatabase :: TVar (Map Text ValidatedFunction)
  , baseUrl :: BaseUrl
  , manager :: Manager
  }
  deriving stock (Generic)

data ValidatedFunction = ValidatedFunction
  { fnImpl :: !Function
  , fnEvaluator :: !(Map EvalBackend RunFunction)
  }
  deriving stock (Generic)

type AppM = ReaderT AppEnv Handler
type Api = NamedRoutes FunctionApi'
type FunctionApi = NamedRoutes FunctionApi'

-- | API that can be invoked by a custom gpt.
--
-- See https://openai.com/index/introducing-gpts/
data FunctionApi' mode = FunctionApi
  { functionRoutes :: mode :- "functions" :> FunctionCrud
  }
  deriving stock (Generic)

type FunctionCrud = NamedRoutes FunctionCrud'

-- | API for interacting with the 'function' resource.
data FunctionCrud' mode = FunctionCrud
  { batchEntities ::
      mode
        :- Summary "Shortened descriptions of all available functions and their parameters"
          :> OperationId "getAllFunctions"
          :> Get '[JSON] [SimpleFunction]
  , singleEntity ::
      mode
        :- Capture "name" String
          :> SingleFunctionApi
  }
  deriving stock (Generic)

type SingleFunctionApi = NamedRoutes SingleFunctionApi'
data SingleFunctionApi' mode = SingleFunctionApi
  { getFunction ::
      mode
        :- Summary "Get a detailed description of the function and its parameters"
          :> OperationId "getFunction"
          :> Get '[JSON] Function
  , postFunction ::
      mode
        :- Summary "Add a function resource that can be evaluated."
          :> ReqBody '[JSON] FunctionImplementation
          :> OperationId "createFunction"
          :> Post '[JSON] ()
  , putFunction ::
      mode
        :- Summary "Update a function resource"
          :> ReqBody '[JSON] FunctionImplementation
          :> OperationId "updateFunction"
          :> Put '[JSON] ()
  , deleteFunction ::
      mode
        :- Summary "Delete the function"
          :> OperationId "deleteFunction"
          :> Delete '[JSON] ()
  , evalFunction ::
      mode
        :- "evaluation"
          :> Summary "Evaluate a function with arguments"
          :> ReqBody '[JSON] FnArguments
          :> OperationId "evalFunction"
          :> Post '[JSON] SimpleResponse
  , batchFunction ::
      mode
        :- "batch"
          :> Summary "Run a function using a batch of arguments"
          :> Description "Evaluate a function with a batch of arguments, conforming to Oracle Intelligent Advisor Batch API"
          :> ReqBody '[JSON] BatchRequest
          :> Post '[JSON] BatchResponse
  -- ^ Run a function with a "batch" of parameters.
  -- This API aims to be consistent with
  -- https://docs.oracle.com/en/cloud/saas/b2c-service/opawx/using-batch-assess-rest-api.html
  }
  deriving stock (Generic)

data SimpleFunction = SimpleFunction
  { simpleName :: Text
  , simpleDescription :: Text
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data Function = Function
  { name :: !Text
  , description :: !Text
  , parameters :: !Parameters
  , supportedEvalBackend :: [EvalBackend]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data FunctionImplementation = FunctionImplementation
  { declaration :: !Function
  , implementation :: !(Map EvalBackend Text)
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data Parameters = MkParameters
  { parameterMap :: Map Text Parameter
  , required :: [Text]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data Parameter = Parameter
  { parameterType :: !Text
  , parameterAlias :: !(Maybe Text)
  , parameterEnum :: ![Text]
  , parameterDescription :: !Text
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data SimpleResponse
  = SimpleResponse !ResponseWithReason
  | SimpleError !EvaluatorError
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FnArguments = FnArguments
  { fnEvalBackend :: Maybe EvalBackend
  , fnArguments :: Map Text (Maybe FnLiteral)
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ----------------------------------------------------------------------------
-- Servant Combinators
-- ----------------------------------------------------------------------------

data OperationId (symbol :: Symbol)

instance (HasLink sub) => HasLink (OperationId s :> sub) where
  type MkLink (OperationId s :> sub) a = MkLink sub a
  toLink = simpleToLink (Proxy :: Proxy sub)

simpleToLink ::
  forall sub a combinator.
  (HasLink sub, MkLink sub a ~ MkLink (combinator :> sub) a) =>
  Proxy sub ->
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkLink (combinator :> sub) a
simpleToLink _ toA _ = toLink toA (Proxy :: Proxy sub)

-- | Ignore @'OperationId'@ in server handlers.
instance (HasServer api ctx) => HasServer (OperationId desc :> api) ctx where
  type ServerT (OperationId desc :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance HasClient m api => HasClient m (OperationId desc :> api) where
  type Client m (OperationId desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

-- ----------------------------------------------------------------------------
-- Web Service Handlers
-- ----------------------------------------------------------------------------

-- TODO: this has become redundant
data EvalBackend
  = JL4
  deriving ()
  deriving stock (Show, Eq, Ord, Enum, Read, Bounded, Generic)

handler :: ServerT Api AppM
handler =
  FunctionApi
    { functionRoutes =
        FunctionCrud
          { batchEntities = getAllFunctions
          , singleEntity = \name ->
              SingleFunctionApi
                { getFunction =
                    getFunctionHandler name
                , putFunction =
                    putFunctionHandler name
                , postFunction =
                    postFunctionHandler name
                , deleteFunction =
                    deleteFunctionHandler name
                , evalFunction =
                    evalFunctionHandler name
                , batchFunction =
                    batchFunctionHandler name
                }
          }
    }

evalFunctionHandler :: String -> FnArguments -> AppM SimpleResponse
evalFunctionHandler name' args = do
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  let fnArgs = Map.assocs args.fnArguments
      eval fnImpl = runEvaluatorFor args.fnEvalBackend fnImpl fnArgs Nothing
  case Map.lookup name functions of
    Nothing -> withUUIDFunction
        name
        eval
        (\k -> throwError (k err404))
    Just fnImpl -> eval fnImpl
 where
  name = Text.pack name'

batchFunctionHandler :: String -> BatchRequest -> AppM BatchResponse
batchFunctionHandler name' batchArgs = do
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  case Map.lookup name functions of
    Nothing -> throwError err404
    Just fnImpl -> do
      (execTime, responses) <- stopwatchM $ forM batchArgs.cases $ \inputCase -> do
        let
          args = Map.assocs $ fmap Just inputCase.attributes

        r <- runEvaluatorFor Nothing fnImpl args outputFilter
        pure (inputCase.id, r)

      let
        nCases = length responses

        successfulRuns =
          Maybe.mapMaybe
            ( \(rid, simpleRes) -> case simpleRes of
                SimpleResponse r -> Just (rid, r)
                SimpleError _ -> Nothing
            )
            responses

        nSuccessful = length successfulRuns
        nIgnored = nCases - nSuccessful

      pure $
        BatchResponse
          { cases =
              [ OutputCase
                { id = rid
                , attributes = Map.fromList response.values
                }
              | (rid, response) <- successfulRuns
              ]
          , summary =
              OutputSummary
                { casesRead = nCases
                , casesProcessed = nSuccessful
                , casesIgnored = nIgnored
                , processorDurationSec = nsToS execTime
                , casesPerSec = nsToS execTime / realToFrac nCases
                , processorQueuedSec = 0
                }
          }
 where
  outputFilter = if null outputFilter' then Nothing else Just outputFilter'
  outputFilter' =
    Set.fromList $
      Maybe.mapMaybe
        ( \ case
            OutcomeAttribute t -> Just t
            OutcomePropertyObject _ -> Nothing
        )
        batchArgs.outcomes
  name = Text.pack name'

  nsToS :: Chronos.Timespan -> Centi
  nsToS n = (realToFrac @Int @Centi $ fromIntegral @Int64 @Int (Chronos.getTimespan n)) / 10e9

runEvaluatorFor :: Maybe EvalBackend -> ValidatedFunction -> [(Text, Maybe FnLiteral)] -> Maybe (Set Text) -> AppM SimpleResponse
runEvaluatorFor engine validatedFunc args outputFilter = do
  eval <- evaluationEngine evalBackend validatedFunc
  evaluationResult <-
    timeoutAction $
      runExceptT
        ( eval.runFunction
            args
            outputFilter
        )

  case evaluationResult of
    Left err -> pure $ SimpleError err
    Right r -> pure $ SimpleResponse r
 where
  evalBackend = Maybe.fromMaybe JL4 engine

deleteFunctionHandler :: String -> AppM ()
deleteFunctionHandler name' = do
  functionsTVar <- asks (.functionDatabase)
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, Map.delete name functions)
      False ->
        (False, functions)

  when (not exists) $
    throwError
      err404
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " does not exist"
        }
 where
  name = Text.pack name'

putFunctionHandler :: String -> FunctionImplementation -> AppM ()
putFunctionHandler name' updatedFunctionImpl = do
  validatedFunction <- validateFunction updatedFunctionImpl
  functionsTVar <- asks (.functionDatabase)
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, Map.insert name validatedFunction functions)
      False ->
        (False, functions)

  when (not exists) $
    -- Error code has been chosen in accordance with
    -- https://stackoverflow.com/a/70371989
    throwError
      err404
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " does not exist"
        }
 where
  name = Text.pack name'

postFunctionHandler :: String -> FunctionImplementation -> AppM ()
postFunctionHandler name' newFunctionImpl = do
  validatedFunction <- validateFunction newFunctionImpl
  functionsTVar <- asks (.functionDatabase)
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, functions)
      False ->
        (False, Map.insert name validatedFunction functions)

  when exists $
    -- Error code has been chosen in accordance with
    -- https://stackoverflow.com/a/70371989
    throwError
      err409
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " already exists"
        }
 where
  name = Text.pack name'

validateFunction :: FunctionImplementation -> AppM ValidatedFunction
validateFunction fn = do
  evaluators <- Map.traverseWithKey validateImplementation fn.implementation
  pure
    ValidatedFunction
      { fnImpl = fn.declaration
      , fnEvaluator = evaluators
      }
 where
  validateImplementation :: EvalBackend -> Text -> AppM RunFunction
  validateImplementation JL4 program =  pure $ Jl4.createFunction (toDecl fn.declaration) program

getAllFunctions :: AppM [SimpleFunction]
getAllFunctions = do
  functions <- liftIO . readTVarIO =<< asks (.functionDatabase)
  pure $ fmap (toSimpleFunction . (.fnImpl)) $ Map.elems functions
 where
  toSimpleFunction s =
    SimpleFunction
      { simpleName = s.name
      , simpleDescription = s.description
      }

getFunctionHandler :: String -> AppM Function
getFunctionHandler name = do
  let tname = Text.pack name
  functions <- liftIO . readTVarIO =<< asks (.functionDatabase)
  case Map.lookup tname functions of
    Nothing ->
      withUUIDFunction
        tname
        (pure . (.fnImpl))
        (\k -> throwError (k err404))
    Just function -> pure function.fnImpl

withUUIDFunction :: Text -> (ValidatedFunction -> AppM a) -> ((ServerError -> ServerError) -> AppM a) -> AppM a
withUUIDFunction uuidAndFun k err = case UUID.fromText muuid of
  Nothing -> err id
  Just uuid -> do
    MkAppEnv {baseUrl, manager} <- ask
    eprog <- liftIO $ runExceptT $ readCrudUUID uuid baseUrl manager
    case eprog of
      Left err' -> do
        liftIO do
          hPutStrLn stderr "failed to retrieve function from CRUD backend"
          hPutStrLn stderr $ displayException err'
        err (\e -> e {errBody = "uuid not present on remote backend: " <> UUID.toLazyASCIIBytes uuid})
      Right prog -> do
        let fnImpl = mkSessionFunction funName MkParameters {parameterMap = Map.empty, required = []} prog
            fnDecl = toDecl fnImpl

        decide <- liftIO (runExceptT (Jl4.buildFunDecide prog fnDecl))
          >>= either (\e -> do
            liftIO $ hPutStrLn stderr "the evaluator failed with error:"
            liftIO $ hPrint stderr e
            throwError err500 {errBody = "evaluator failed"}
            ) pure

        k ValidatedFunction
          { fnImpl = fnImpl { parameters = parametersOfDecide decide }
          , fnEvaluator = Map.singleton JL4 $ Jl4.createFunction fnDecl prog
          }
  where
   (muuid, funName) = T.drop 1 <$> T.breakOn ":" uuidAndFun

parametersOfDecide :: Decide Resolved -> Parameters
parametersOfDecide (MkDecide _ (MkTypeSig _ (MkGivenSig _ typedNames) _) (MkAppForm _ _ args _) _)  =
  -- TODO:
  -- need to change the description of the parameters as soon as we have it in the Decide
  MkParameters {parameterMap = Map.fromList $ map (\x -> (x ,
    let argInfo = lookup x bestEffortArgInfo
     in Parameter (maybe "object" fst argInfo) Nothing [] (maybe "" snd argInfo))) argList, required = argList}
 where
  bestEffortArgInfo = foldr fn [] args
  fn r acc = case find (\(MkOptionallyTypedName _ r' _) -> r `sameResolved` r') typedNames of
    Just tn@(MkOptionallyTypedName _ r' mt)
      | Just t <- mt
      , let descriptions = mconcat $ nubOrd $ Optics.toListOf (Optics.gplate @TAnnotations Optics.% #_TDesc) tn
      -> (prettyLayout r', (prettyLayout t, descriptions)) : acc
    _ -> acc
  sameResolved = (==) `on` getUnique
  argList = map prettyLayout args

timeoutAction :: IO b -> AppM b
timeoutAction act =
  liftIO (timeout (seconds 60) act) >>= \ case
    Nothing -> throwError err500
    Just r -> pure r
 where
  seconds n = 1_000_000 * n

stopwatchM :: AppM a -> AppM (Chronos.Timespan, a)
stopwatchM action = do
  start <- liftIO Clock.getMonotonicTimeNSec
  a' <- action
  a <- liftIO $ evaluate a'
  end <- liftIO Clock.getMonotonicTimeNSec
  pure ((Chronos.Timespan (fromIntegral (end - start))), a)

-- ----------------------------------------------------------------------------
-- "Database" layer
-- ----------------------------------------------------------------------------

evaluationEngine :: EvalBackend -> ValidatedFunction -> AppM RunFunction
evaluationEngine b valFn = do
  case Map.lookup b valFn.fnEvaluator of
    Nothing -> throwError err404
    Just eval -> pure eval

-- ----------------------------------------------------------------------------
-- Json encoders and decoders that are not derived.
-- We often need custom instances, as we want to be more lenient in what we accept
-- than what aeson does by default. Further, we try to provide a specific json schema.
--
-- ----------------------------------------------------------------------------

instance ToJSON SimpleFunction where
  toJSON (SimpleFunction n desc) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            ]
      ]

instance FromJSON SimpleFunction where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    "function" :: Text <- o .: "type"
    props <- o .: "function"
    simpleFn <-
      Aeson.withObject
        "function body"
        ( \p -> do
            SimpleFunction
              <$> p .: "name"
              <*> p .: "description"
        )
        props
    pure simpleFn

instance ToJSON Function where
  toJSON (Function n desc params backends) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            , "parameters" .= params
            , "supportedBackends" .= backends
            ]
      ]

instance FromJSON Function where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    fnType <- o .: "type"
    case fnType :: Text of
      "function" -> pure ()
      e -> fail $ "Expected \"function\" but got" <> Text.unpack e
    props <- o .: "function"
    Aeson.withObject
      "function body"
      ( \p -> do
          Function
            <$> p .: "name"
            <*> p .: "description"
            <*> p .: "parameters"
            <*> p .: "supportedBackends"
      )
      props

instance ToJSON FunctionImplementation where
  toJSON fnImpl =
    Aeson.object
      [ "declaration" .= fnImpl.declaration
      , "implementation" .= fnImpl.implementation
      ]

instance FromJSON FunctionImplementation where
  parseJSON = Aeson.withObject "Function Implementation" $ \o -> do
    FunctionImplementation
      <$> o .: "declaration"
      <*> o .: "implementation"

instance ToJSON Parameters where
  toJSON (MkParameters props reqProps) =
    Aeson.object
      [ "type" .= Aeson.String "object"
      , "properties" .= props
      , "required" .= reqProps
      ]

instance FromJSON Parameters where
  parseJSON = Aeson.withObject "Parameters" $ \o -> do
    _ :: Text <- o .: "type"
    props <- o .: "properties"
    reqProps <- o .: "required"
    pure $ MkParameters props reqProps

instance ToJSON Parameter where
  toJSON p =
    Aeson.object
      [ "type" .= p.parameterType
      , "alias" .= p.parameterAlias -- omitNothingFields?
      , "enum" .= p.parameterEnum
      , "description" .= p.parameterDescription
      ]

instance FromJSON Parameter where
  parseJSON = Aeson.withObject "Parameter" $ \p ->
    Parameter
      <$> p .: "type"
      <*> p .:? "alias"
      <*> p .:? "enum" .!= []
      <*> p .: "description"

instance FromHttpApiData EvalBackend where
  parseQueryParam t = case Text.toLower t of
    "jl4" -> Right JL4
    _ -> Left $ "Invalid evaluation backend: " <> t

instance ToJSON EvalBackend where
  toJSON = \ case
    JL4 -> Aeson.String "jl4"

instance FromJSON EvalBackend where
  parseJSON (Aeson.String s) = case Text.toLower s of
    "jl4" -> pure JL4
    o -> Aeson.prependFailure "EvalBackend" (Aeson.typeMismatch "String" $ Aeson.String o)
  parseJSON o = Aeson.prependFailure "EvalBackend" (Aeson.typeMismatch "String" o)

instance ToJSONKey EvalBackend

instance FromJSONKey EvalBackend

toDecl :: Function -> Api.FunctionDeclaration
toDecl fn =
  Api.FunctionDeclaration
    { Api.name = fn.name
    , Api.description = fn.description
    , Api.longNames = Map.keysSet $ fn.parameters.parameterMap
    , Api.nameMapping = shortToLongNameMapping
    }
 where
  shortToLongNameMapping :: Map Text Text
  shortToLongNameMapping =
    Map.fromList $
      Maybe.mapMaybe (fmap Tuple.swap . Tuple.secondM (.parameterAlias)) $
        Map.assocs fn.parameters.parameterMap

-- ----------------------------------------------------------------------------
-- Oracle DB
-- ----------------------------------------------------------------------------

type Id = Int

data Outcomes
  = OutcomeAttribute Text
  | OutcomePropertyObject OutcomeObject
  deriving stock (Show, Eq, Ord)

data OutcomeObject = OutcomeObject
  { id :: Text
  , showSilent :: Maybe Bool
  , showInvisible :: Maybe Bool
  , resolveIndecisionRelationships :: Maybe Bool
  , knownOutcomeStyle :: Maybe OutcomeStyle
  , unknownOutcomeStyle :: Maybe OutcomeStyle
  }
  deriving stock (Show, Eq, Ord)

data OutcomeStyle
  = ValueOnly
  | DecisionReport
  | BaseAttributes
  deriving stock (Show, Ord, Eq, Enum, Bounded)

data BatchRequest = BatchRequest
  { outcomes :: [Outcomes]
  , cases :: [InputCase]
  }
  deriving stock (Show, Eq, Ord)

data InputCase = InputCase
  { id :: Id
  , attributes :: Map Text FnLiteral
  }
  deriving stock (Show, Eq, Ord)

data OutputCase = OutputCase
  { id :: Id
  , attributes :: Map Text FnLiteral
  }
  deriving stock (Show, Eq, Ord)

data BatchResponse = BatchResponse
  { cases :: [OutputCase]
  , summary :: OutputSummary
  }
  deriving stock (Show, Eq, Ord)

data OutputSummary = OutputSummary
  { casesRead :: Int
  , casesProcessed :: Int
  , casesIgnored :: Int
  , processorDurationSec :: Fixed E2
  , casesPerSec :: Fixed E2
  , processorQueuedSec :: Fixed E2
  }
  deriving stock (Show, Eq, Ord)

-- ----------------------------------------------------------------------------
-- Batch request json
-- ----------------------------------------------------------------------------

batchRequestDecoder :: Decoder BatchRequest
batchRequestDecoder =
  BatchRequest
    <$> ACD.key "outcomes" (ACD.list outcomesDecoder)
    <*> ACD.key "cases" (ACD.list inputCaseDecoder)

outcomeStyleDecoder :: Decoder OutcomeStyle
outcomeStyleDecoder =
  ACD.text >>= \ case
    "value-only" -> pure ValueOnly
    "decision-report" -> pure DecisionReport
    "base-attributes" -> pure BaseAttributes
    val -> fail $ "Unknown value " <> show val

outcomeObjectDecoder :: Decoder OutcomeObject
outcomeObjectDecoder =
  OutcomeObject
    <$> ACD.key "id" ACD.text
    <*> ACD.maybeKey "showSilent" ACD.bool
    <*> ACD.maybeKey "showInvisible" ACD.bool
    <*> ACD.maybeKey "resolveIndecisionRelationships" ACD.bool
    <*> ACD.maybeKey "knownOutcomeStyle" outcomeStyleDecoder
    <*> ACD.maybeKey "unknownOutcomeStyle" outcomeStyleDecoder

outcomesDecoder :: Decoder Outcomes
outcomesDecoder =
  (OutcomeAttribute <$> ACD.text)
    <|> (OutcomePropertyObject <$> outcomeObjectDecoder)

inputCaseDecoder :: Decoder InputCase
inputCaseDecoder = do
  caseId <- ACD.key "@id" ACD.int
  attributes <- ACD.mapStrict fnLiteralDecoder
  let
    attributes' = Map.delete "@id" attributes
  pure $ InputCase caseId attributes'

fnLiteralDecoder :: Decoder FnLiteral
fnLiteralDecoder =
  (FnLitInt <$> ACD.integer)
    <|> (FnLitDouble <$> ACD.double)
    <|> (FnLitBool <$> ACD.bool)
    <|> (parseTextAsFnLiteral <$> ACD.text)
    <|> (FnArray <$> ACD.list fnLiteralDecoder)
    <|> (FnObject <$> ACD.list (liftA2 (,) ACD.text fnLiteralDecoder))

batchRequestEncoder :: BatchRequest -> Aeson.Value
batchRequestEncoder br =
  Aeson.object
    [ "outcomes" .= fmap outcomesEncoder br.outcomes
    ]

outcomesEncoder :: Outcomes -> Aeson.Value
outcomesEncoder (OutcomeAttribute t) = Aeson.String t
outcomesEncoder (OutcomePropertyObject o) =
  Aeson.object
    [ "@id" .= o.id
    , "showSilent" .= o.showSilent
    , "showInvisible" .= o.showInvisible
    , "resolveIndecisionRelationships" .= o.resolveIndecisionRelationships
    , "knownOutcomeStyle" .= fmap outcomeStyleEncoder o.knownOutcomeStyle
    , "unknownOutcomeStyle" .= fmap outcomeStyleEncoder o.unknownOutcomeStyle
    ]

outcomeStyleEncoder :: OutcomeStyle -> Aeson.Value
outcomeStyleEncoder = \ case
  ValueOnly -> Aeson.String "value-only"
  DecisionReport -> Aeson.String "decision-report"
  BaseAttributes -> Aeson.String "base-attributes"

-- ----------------------------------------------------------------------------
-- Batch response json
-- ----------------------------------------------------------------------------

batchResponseDecoder :: Decoder BatchResponse
batchResponseDecoder =
  BatchResponse
    <$> ACD.key "cases" (ACD.list casesDecoder)
    <*> ACD.key "summary" summaryDecoder

casesDecoder :: Decoder OutputCase
casesDecoder = do
  caseId <- ACD.key "@id" ACD.int
  attributes <- ACD.mapStrict fnLiteralDecoder
  let
    attributes' = Map.delete "@id" attributes
  pure $ OutputCase caseId attributes'

summaryDecoder :: Decoder OutputSummary
summaryDecoder =
  OutputSummary
    <$> ACD.key "casesRead" ACD.int
    <*> ACD.key "casesProcessed" ACD.int
    <*> ACD.key "casesIgnored" ACD.int
    <*> ACD.key "processorDurationSec" (fmap toFixed ACD.scientific)
    <*> ACD.key "processorCasesPerSec" (fmap toFixed ACD.scientific)
    <*> ACD.key "processorQueuedSec" (fmap toFixed ACD.scientific)
 where
  toFixed = realToFrac @Scientific @Centi

batchResponseEncoder :: BatchResponse -> Aeson.Value
batchResponseEncoder br =
  Aeson.object $
    [ "cases" .= fmap outputCaseEncoder br.cases
    , "summary" .= outputSummaryEncoder br.summary
    ]

outputSummaryEncoder :: OutputSummary -> Aeson.Value
outputSummaryEncoder os =
  Aeson.object
    [ "casesRead" .= os.casesRead
    , "casesProcessed" .= os.casesProcessed
    , "casesIgnored" .= os.casesIgnored
    , "processorDurationSec" .= toSci os.processorDurationSec
    , "processorCasesPerSec" .= toSci os.casesPerSec
    , "processorQueuedSec" .= toSci os.processorQueuedSec
    ]
 where
  toSci = realToFrac @Centi @Scientific

outputCaseEncoder :: OutputCase -> Aeson.Value
outputCaseEncoder oc =
  Aeson.object $
    [ "@id" .= oc.id
    ]
      <> [ (Aeson.fromText k .= v)
         | (k, v) <- Map.assocs oc.attributes
         ]

-- ----------------------------------------------------------------------------
-- Final FromJSON/ToJSON instances
-- ----------------------------------------------------------------------------

instance FromJSON BatchRequest where
  parseJSON = ACD.fromDecoder batchRequestDecoder

instance ToJSON BatchRequest where
  toJSON = batchRequestEncoder

instance ToJSON BatchResponse where
  toJSON = batchResponseEncoder

instance FromJSON BatchResponse where
  parseJSON = ACD.fromDecoder batchResponseDecoder

crudAPIClient :: CRUD.Api (AsClientT ClientM)
crudAPIClient = genericClient

runCrudClient :: ClientM a -> BaseUrl -> Manager -> ExceptT ClientError IO a
runCrudClient m crudBaseUrl mgr = ExceptT $ runClientM m $ mkClientEnv mgr crudBaseUrl

readCrudUUID :: UUID -> BaseUrl -> Manager -> ExceptT ClientError IO Text
readCrudUUID = runCrudClient . crudAPIClient.readSession

mkSessionFunction :: Text -> Parameters -> Text -> Function
mkSessionFunction name parameters description =
  Function
    { name
    , description
    , parameters
    , supportedEvalBackend = [JL4]
    }
