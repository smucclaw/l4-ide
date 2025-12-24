module IntegrationSpec (spec) where

import Test.Hspec

import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
import Servant.Client.Generic (genericClient)

import Application (app)
import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Examples
import Server
import qualified TestData

type AppClient = FunctionApi' (AsClientT ClientM)

spec :: SpecWith ()
spec = describe "integration" do
  it "contains example functions" do
    runDecisionService \api -> do
      funs <- api.functionRoutes.batchEntities
      liftIO $ do
        fmap (.simpleName) funs `shouldContain` ["vermin_and_rodent"]
        fmap (.simpleName) funs `shouldContain` ["compute_qualifies"]
  it "details of compute_qualifies" do
    runDecisionService \api -> do
      fun <- (api.functionRoutes.singleEntity "compute_qualifies").getFunction
      liftIO do
        fun.name `shouldBe` "compute_qualifies"
        fun.description `shouldSatisfy` (not . Text.null)
        fun.supportedEvalBackend `shouldContain` [JL4]
        let
          params = fun.parameters.parameterMap
        Map.keys params `shouldMatchList` ["walks", "eats", "drinks"]
  it "add new function" do
    runDecisionService \api -> do
      rodentSpec <- liftIO TestData.rodentAndVerminFunctionSpec
      oldFuns <- api.functionRoutes.batchEntities
      () <- (api.functionRoutes.singleEntity "newRodentAndVermin").postFunction
        FunctionImplementation
          { declaration = rodentSpec.fnImpl
          , implementation = Map.singleton JL4 TestData.rodentAndVerminJL4
          }
      newFuns <- api.functionRoutes.batchEntities
      liftIO do
        length newFuns `shouldBe` length oldFuns + 1
  it "update function" do
    runDecisionService \api -> do
      rodentSpec <- liftIO TestData.rodentAndVerminFunctionSpec
      oldFun <- (api.functionRoutes.singleEntity "vermin_and_rodent").getFunction
      () <- (api.functionRoutes.singleEntity "vermin_and_rodent").putFunction
        FunctionImplementation
          { declaration = rodentSpec.fnImpl
          , implementation = Map.singleton JL4 TestData.rodentAndVerminJL4
          }
      newFun <- (api.functionRoutes.singleEntity "vermin_and_rodent").getFunction
      liftIO do
        newFun.description `shouldNotBe` oldFun.description
        newFun.name `shouldBe` oldFun.name
  it "fills metadata from annotations when omitted by client" do
    runDecisionService \api -> do
      let
        emptyDeclaration =
          Function
            { name = ""
            , description = ""
            , parameters = MkParameters {parameterMap = Map.empty, required = []}
            , supportedEvalBackend = [JL4]
            }
        impl =
          FunctionImplementation
            { declaration = emptyDeclaration
            , implementation = Map.singleton JL4 TestData.annotationFallbackJL4
            }
      () <- (api.functionRoutes.singleEntity "annotatedUpload").postFunction impl
      fun <- (api.functionRoutes.singleEntity "annotatedUpload").getFunction
      liftIO do
        fun.name `shouldBe` "annotatedEntry"
        fun.description `shouldBe` "Complex metadata demo"
        let params = fun.parameters.parameterMap
            expectedNames =
              fmap Text.pack
                [ "numberParam"
                , "textParam"
                , "boolParam"
                , "recordParam"
                , "listParam"
                , "listOfListsParam"
                , "nestedRecordParam"
                , "listOfRecordsParam"
                , "recordOfListsParam"
                ]
        Map.keys params `shouldMatchList` expectedNames
        mapM_ (uncurry3 (assertParam params))
          [ (Text.pack "numberParam", "number", "Numeric input")
          , (Text.pack "textParam", "string", "Text input")
          , (Text.pack "boolParam", "boolean", "Flag input")
          , (Text.pack "recordParam", "object", "Record input")
          , (Text.pack "listParam", "array", "List of strings")
          , (Text.pack "listOfListsParam", "array", "Matrix of numbers")
          , (Text.pack "nestedRecordParam", "object", "Nested record")
          , (Text.pack "listOfRecordsParam", "array", "Crew members")
          , (Text.pack "recordOfListsParam", "object", "Container of lists")
          ]
        fun.parameters.required `shouldBe` expectedNames
  describe "evaluation" do
    it "zero-parameter constant function" do
      runDecisionService $ \api -> do
        r <-
          (api.functionRoutes.singleEntity "the_answer").evalFunction
            Nothing  -- X-L4-Trace header
            Nothing  -- ?trace= query param
            Nothing  -- ?graphviz= query param
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments = Map.empty  -- No parameters needed
              }
        liftIO do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitInt 42)]
    it "person qualifies" do
      runDecisionService $ \api -> do
        r <-
          (api.functionRoutes.singleEntity "compute_qualifies").evalFunction
            Nothing  -- X-L4-Trace header
            Nothing  -- ?trace= query param
            Nothing  -- ?graphviz= query param
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments =
                  Map.fromList
                    [ ("walks", Just $ FnLitBool True)
                    , ("drinks", Just $ FnLitBool True)
                    , ("eats", Just $ FnLitBool True)
                    ]
              }
        liftIO do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool True)]
    it "person does not qualify" do
      runDecisionService $ \api -> do
        r <-
          (api.functionRoutes.singleEntity "compute_qualifies").evalFunction
            Nothing  -- X-L4-Trace header
            Nothing  -- ?trace= query param
            Nothing  -- ?graphviz= query param
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments =
                  Map.fromList
                    [ ("walks", Just $ FnLitBool False)
                    , ("drinks", Just $ FnLitBool False)
                    , ("eats", Just $ FnLitBool False)
                    ]
              }
        liftIO do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool False)]
    it "not covered by insurance" do
      runDecisionService $ \api -> do
        r <-
          (api.functionRoutes.singleEntity "vermin_and_rodent").evalFunction
            Nothing  -- X-L4-Trace header
            Nothing  -- ?trace= query param
            Nothing  -- ?graphviz= query param
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments =
                  Map.fromList
                    [
                      ( "i"
                      , Just $
                          FnObject
                            [ ("Loss or Damage.caused by rodents", FnLitBool True)
                            , ("Loss or Damage.caused by insects", FnLitBool True)
                            , ("Loss or Damage.caused by vermin", FnLitBool True)
                            , ("Loss or Damage.caused by birds", FnLitBool True)
                            , ("Loss or Damage.to Contents", FnLitBool True)
                            , ("any other exclusion applies", FnLitBool True)
                            , ("a household appliance", FnLitBool True)
                            , ("a swimming pool", FnLitBool True)
                            , ("a plumbing, heating, or air conditioning system", FnLitBool True)
                            , ("Loss or Damage.ensuing covered loss", FnLitBool True)
                            ]
                      )
                    ]
              }
        liftIO do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool False)]

    it "covered by insurance" do
      runDecisionService $ \api -> do
        r <-
          (api.functionRoutes.singleEntity "vermin_and_rodent").evalFunction
            Nothing  -- X-L4-Trace header
            Nothing  -- ?trace= query param
            Nothing  -- ?graphviz= query param
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments =
                  Map.fromList
                    [
                      ( "i"
                      , Just $
                          FnObject
                            [ ("Loss or Damage.caused by rodents", FnLitBool True)
                            , ("Loss or Damage.caused by insects", FnLitBool True)
                            , ("Loss or Damage.caused by vermin", FnLitBool True)
                            , ("Loss or Damage.caused by birds", FnLitBool False)
                            , ("Loss or Damage.to Contents", FnLitBool False)
                            , ("any other exclusion applies", FnLitBool False)
                            , ("a household appliance", FnLitBool True)
                            , ("a swimming pool", FnLitBool True)
                            , ("a plumbing, heating, or air conditioning system", FnLitBool True)
                            , ("Loss or Damage.ensuing covered loss", FnLitBool False)
                            ]
                      )
                    ]
              }
        liftIO do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool True)]

  describe "query-plan" do
    it "suggests remaining boolean inputs for compute_qualifies" do
      runDecisionService \api -> do
        qp <-
          (api.functionRoutes.singleEntity "compute_qualifies").queryPlan
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments = Map.empty
              }
        liftIO do
          qp.determined `shouldBe` Nothing
          fmap (.inputLabel) qp.inputs `shouldContain` ["walks"]

    it "accepts bindings by atom unique (string key)" do
      runDecisionService \api -> do
        qp0 <-
          (api.functionRoutes.singleEntity "compute_qualifies").queryPlan
            FnArguments
              { fnEvalBackend = Just JL4
              , fnArguments = Map.empty
              }
        let
          mWalks = List.find (\i -> i.inputLabel == "walks") qp0.inputs
        case mWalks of
          Nothing -> liftIO $ expectationFailure "expected an input named walks"
          Just walksInput -> do
            let
              walkAtomUniques = fmap (.unique) walksInput.atoms
            case walkAtomUniques of
              [] -> liftIO $ expectationFailure "expected walks to affect at least one atom"
              (u : _) -> do
                qp1 <-
                  (api.functionRoutes.singleEntity "compute_qualifies").queryPlan
                    FnArguments
                      { fnEvalBackend = Just JL4
                      , fnArguments = Map.singleton (Text.pack (show u)) (Just (FnLitBool True))
                      }
                liftIO do
                  qp1.determined `shouldBe` Nothing
                  -- Some progress: either walks is no longer needed, or it's still needed but in a reduced form.
                  fmap (.inputLabel) qp1.inputs `shouldSatisfy` (not . ("walks" `elem`))

    it "batch evaluation with precompiled module (parallel)" do
      runDecisionService $ \api -> do
        -- Create 100 test cases to demonstrate parallel batch evaluation
        let testCase n = InputCase
              { id = n
              , attributes = Map.fromList
                  [ ("walks", FnLitBool True)
                  , ("eats", FnLitBool True)
                  , ("drinks", FnLitBool True)
                  ]
              }
            cases = map testCase [1..100]

        r <- (api.functionRoutes.singleEntity "compute_qualifies").batchFunction
          Nothing  -- X-L4-Trace header
          Nothing  -- ?trace= query param
          Nothing  -- ?graphviz= query param
          BatchRequest
            { cases = cases
            , outcomes = []
            }

        liftIO $ do
          -- All 100 cases should succeed
          length r.cases `shouldBe` 100
          -- All should return True (person qualifies)
          all (\c -> Map.lookup "result" c.attributes == Just (FnLitBool True)) r.cases `shouldBe` True
          -- Summary should show all cases processed
          r.summary.casesRead `shouldBe` 100
          r.summary.casesProcessed `shouldBe` 100
          r.summary.casesIgnored `shouldBe` 0

requireSuccess :: (MonadIO m) => SimpleResponse -> m ResponseWithReason
requireSuccess resp = case resp of
  SimpleResponse s -> pure s
  SimpleError evalError -> do
    liftIO $ expectationFailure (show evalError)
    error "Impossible"

runDecisionService :: (AppClient -> ClientM a) -> IO a
runDecisionService act = do
  result <- runDecisionService' act
  case result of
    Right a -> pure a
    Left b -> do
      expectationFailure (show b)
      pure undefined

runDecisionService' :: (AppClient -> ClientM a) -> IO (Either ClientError a)
runDecisionService' act = do
  let
    apiClient = genericClient @FunctionApi' @ClientM
    runC port c = do
      mgr <- newManager defaultManagerSettings
      let
        baseUrl = BaseUrl Http "localhost" port ""
      runClientM c $ mkClientEnv mgr baseUrl
  testWithApplication
    ( do
        appEnv <- initExampleAppEnv
        pure $ app appEnv Nothing
    )
    \port -> do
      runC port do
        act apiClient

initExampleAppEnv :: IO AppEnv
initExampleAppEnv = do
  funcs <- Examples.functionSpecs
  MkAppEnv <$> newTVarIO funcs <*> pure (BaseUrl Http "localhost" 5008  "") <*> newManager defaultManagerSettings
assertParam ::
  Map.Map Text Parameter ->
  Text ->
  Text ->
  Text ->
  Expectation
assertParam params paramName expectedType expectedDesc =
  case Map.lookup paramName params of
    Nothing -> expectationFailure ("missing parameter: " <> Text.unpack paramName)
    Just p -> do
      p.parameterType `shouldBe` expectedType
      p.parameterDescription `shouldBe` expectedDesc

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
