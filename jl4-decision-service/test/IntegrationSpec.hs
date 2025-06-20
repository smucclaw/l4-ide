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
          params = fun.parameters.parameters
        Map.keys params `shouldMatchList` ["walks", "eats", "drinks"]
  it "add new function" do
    runDecisionService \api -> do
      oldFuns <- api.functionRoutes.batchEntities
      () <- (api.functionRoutes.singleEntity "newRodentAndVermin").postFunction
        FunctionImplementation
          { declaration = TestData.rodentAndVerminFunctionSpec.fnImpl
          , implementation = Map.singleton JL4 TestData.rodentAndVerminJL4
          }
      newFuns <- api.functionRoutes.batchEntities
      liftIO do
        length newFuns `shouldBe` length oldFuns + 1
  it "update function" do
    runDecisionService \api -> do
      oldFun <- (api.functionRoutes.singleEntity "vermin_and_rodent").getFunction
      () <- (api.functionRoutes.singleEntity "vermin_and_rodent").putFunction
        FunctionImplementation
          { declaration = TestData.rodentAndVerminFunctionSpec.fnImpl
          , implementation = Map.singleton JL4 TestData.rodentAndVerminJL4
          }
      newFun <- (api.functionRoutes.singleEntity "vermin_and_rodent").getFunction
      liftIO do
        newFun.description `shouldNotBe` oldFun.description
        newFun.name `shouldBe` oldFun.name
  describe "evaluation" do
    it "person qualifies" do
      runDecisionService $ \api -> do
        r <-
          (api.functionRoutes.singleEntity "compute_qualifies").evalFunction
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
initExampleAppEnv =
  MkAppEnv <$> newTVarIO Examples.functionSpecs <*> pure (BaseUrl Http "localhost" 5008  "") <*> newManager defaultManagerSettings
