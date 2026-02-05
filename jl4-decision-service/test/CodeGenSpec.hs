{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGenSpec (spec) where

import Test.Hspec

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (try)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
import Servant.Client.Generic (genericClient)
import System.IO.Error (isPermissionError)

import Application (app)
import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import Server
import qualified Examples
import Backend.CodeGen (inputFieldName, transformJsonKeys)

type AppClient = FunctionApi' (AsClientT ClientM)

spec :: SpecWith ()
spec = describe "CodeGen" $ do
  describe "inputFieldName" $ do
    it "adds (input) suffix to simple names" $ do
      inputFieldName "x" `shouldBe` "x (input)"
      inputFieldName "value" `shouldBe` "value (input)"
    
    it "adds (input) suffix to names with spaces" $ do
      inputFieldName "musk service role" `shouldBe` "musk service role (input)"
      inputFieldName "first value" `shouldBe` "first value (input)"
    
    it "handles empty string" $ do
      inputFieldName "" `shouldBe` " (input)"

  describe "transformJsonKeys" $ do
    it "transforms top-level object keys" $ do
      let input = Aeson.object 
            [ ("x", Aeson.Number 1)
            , ("y", Aeson.Number 2)
            ]
          expected = Aeson.object
            [ ("x (input)", Aeson.Number 1)
            , ("y (input)", Aeson.Number 2)
            ]
      transformJsonKeys input `shouldBe` expected
    
    it "transforms keys with spaces" $ do
      let input = Aeson.object
            [ ("musk service role", Aeson.String "CEO")
            , ("board approved", Aeson.Bool True)
            ]
          expected = Aeson.object
            [ ("musk service role (input)", Aeson.String "CEO")
            , ("board approved (input)", Aeson.Bool True)
            ]
      transformJsonKeys input `shouldBe` expected
    
    it "does not transform nested object keys" $ do
      let input = Aeson.object
            [ ("person", Aeson.object
                [ ("name", Aeson.String "Alice")
                , ("age", Aeson.Number 30)
                ])
            ]
          expected = Aeson.object
            [ ("person (input)", Aeson.object
                [ ("name", Aeson.String "Alice")
                , ("age", Aeson.Number 30)
                ])
            ]
      transformJsonKeys input `shouldBe` expected
    
    it "passes through non-object values unchanged" $ do
      transformJsonKeys (Aeson.String "test") `shouldBe` Aeson.String "test"
      transformJsonKeys (Aeson.Number 42) `shouldBe` Aeson.Number 42
      transformJsonKeys (Aeson.Bool True) `shouldBe` Aeson.Bool True
      transformJsonKeys Aeson.Null `shouldBe` Aeson.Null
    
    it "passes through arrays unchanged" $ do
      let input = Aeson.toJSON [1 :: Int, 2, 3]
      transformJsonKeys input `shouldBe` input

  describe "field name collision avoidance" $ do
    it "evaluates function when GIVEN param names match common words" do
      -- This test verifies that functions with parameters that might collide
      -- with generated InputArgs field accessor names still evaluate correctly
      runDecisionService $ \api -> do
        -- Upload a function with parameters that could cause collisions
        let fnDecl = Function
              { name = "collision_test"
              , description = "Test function to verify no name collisions"
              , parameters = MkParameters
                  { parameterMap = Map.fromList
                      [ ("x", Parameter "number" Nothing [] "First number" Nothing Nothing Nothing)
                      , ("y", Parameter "number" Nothing [] "Second number" Nothing Nothing Nothing)
                      ]
                  , required = ["x", "y"]
                  }
              , supportedEvalBackend = [JL4]
              }
            impl = FunctionImplementation
              { declaration = fnDecl
              , implementation = Map.singleton JL4 collisionTestJL4
              }
        () <- (api.functionRoutes.singleEntity "collision_test").postFunction impl
        
        -- Evaluate the function - this should work without collision errors
        r <- (api.functionRoutes.singleEntity "collision_test").evalFunction
          Nothing Nothing Nothing
          FnArguments
            { fnEvalBackend = Just JL4
            , fnArguments = Map.fromList
                [ ("x", Just $ FnLitInt 10)
                , ("y", Just $ FnLitInt 5)
                ]
            }
        liftIO $ do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitInt 15)]

    it "evaluates function when GIVEN param names have spaces (backtick identifiers)" do
      runDecisionService $ \api -> do
        -- Upload a function with backtick-quoted parameter names
        let fnDecl = Function
              { name = "spaced_params"
              , description = "Function with spaced parameter names"
              , parameters = MkParameters
                  { parameterMap = Map.fromList
                      [ ("first value", Parameter "number" Nothing [] "First value" Nothing Nothing Nothing)
                      , ("second value", Parameter "number" Nothing [] "Second value" Nothing Nothing Nothing)
                      ]
                  , required = ["first value", "second value"]
                  }
              , supportedEvalBackend = [JL4]
              }
            impl = FunctionImplementation
              { declaration = fnDecl
              , implementation = Map.singleton JL4 spacedParamsJL4
              }
        () <- (api.functionRoutes.singleEntity "spaced_params").postFunction impl
        
        r <- (api.functionRoutes.singleEntity "spaced_params").evalFunction
          Nothing Nothing Nothing
          FnArguments
            { fnEvalBackend = Just JL4
            , fnArguments = Map.fromList
                [ ("first value", Just $ FnLitInt 100)
                , ("second value", Just $ FnLitInt 50)
                ]
            }
        liftIO $ do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitInt 150)]

    it "evaluates function with enum parameter (Service Role pattern)" do
      -- This pattern is similar to the CEO Award that was causing collisions
      runDecisionService $ \api -> do
        let fnDecl = Function
              { name = "role_check"
              , description = "Check role eligibility"
              , parameters = MkParameters
                  { parameterMap = Map.fromList
                      [ ("employee role", Parameter "string" Nothing ["Manager", "Engineer", "Analyst"] "Employee role" Nothing Nothing Nothing)
                      , ("years of service", Parameter "number" Nothing [] "Years worked" Nothing Nothing Nothing)
                      ]
                  , required = ["employee role", "years of service"]
                  }
              , supportedEvalBackend = [JL4]
              }
            impl = FunctionImplementation
              { declaration = fnDecl
              , implementation = Map.singleton JL4 roleCheckJL4
              }
        () <- (api.functionRoutes.singleEntity "role_check").postFunction impl
        
        r <- (api.functionRoutes.singleEntity "role_check").evalFunction
          Nothing Nothing Nothing
          FnArguments
            { fnEvalBackend = Just JL4
            , fnArguments = Map.fromList
                [ ("employee role", Just $ FnLitString "Manager")
                , ("years of service", Just $ FnLitInt 5)
                ]
            }
        liftIO $ do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool True)]

    it "evaluates function with nested record parameter" do
      runDecisionService $ \api -> do
        let fnDecl = Function
              { name = "nested_record_test"
              , description = "Function with nested record"
              , parameters = MkParameters
                  { parameterMap = Map.fromList
                      [ ("person", Parameter "object" Nothing [] "Person record" Nothing Nothing Nothing)
                      ]
                  , required = ["person"]
                  }
              , supportedEvalBackend = [JL4]
              }
            impl = FunctionImplementation
              { declaration = fnDecl
              , implementation = Map.singleton JL4 nestedRecordJL4
              }
        () <- (api.functionRoutes.singleEntity "nested_record_test").postFunction impl
        
        r <- (api.functionRoutes.singleEntity "nested_record_test").evalFunction
          Nothing Nothing Nothing
          FnArguments
            { fnEvalBackend = Just JL4
            , fnArguments = Map.fromList
                [ ("person", Just $ FnObject
                    [ ("name", FnLitString "Alice")
                    , ("age", FnLitInt 30)
                    ])
                ]
            }
        liftIO $ do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool True)]

    it "evaluates function with mixed boolean and non-boolean parameters" do
      -- This tests the deep Maybe lifting with mixed parameter types
      runDecisionService $ \api -> do
        let fnDecl = Function
              { name = "mixed_params"
              , description = "Function with mixed parameter types"
              , parameters = MkParameters
                  { parameterMap = Map.fromList
                      [ ("is active", Parameter "boolean" Nothing [] "Active flag" Nothing Nothing Nothing)
                      , ("threshold value", Parameter "number" Nothing [] "Threshold" Nothing Nothing Nothing)
                      , ("is verified", Parameter "boolean" Nothing [] "Verified flag" Nothing Nothing Nothing)
                      ]
                  , required = ["is active", "threshold value", "is verified"]
                  }
              , supportedEvalBackend = [JL4]
              }
            impl = FunctionImplementation
              { declaration = fnDecl
              , implementation = Map.singleton JL4 mixedParamsJL4
              }
        () <- (api.functionRoutes.singleEntity "mixed_params").postFunction impl
        
        r <- (api.functionRoutes.singleEntity "mixed_params").evalFunction
          Nothing Nothing Nothing
          FnArguments
            { fnEvalBackend = Just JL4
            , fnArguments = Map.fromList
                [ ("is active", Just $ FnLitBool True)
                , ("threshold value", Just $ FnLitInt 100)
                , ("is verified", Just $ FnLitBool True)
                ]
            }
        liftIO $ do
          s <- requireSuccess r
          s.values `shouldBe` [("result", FnLitBool True)]


-- Test L4 source code
collisionTestJL4 :: Text
collisionTestJL4 = [i|
GIVEN x IS A NUMBER
    , y IS A NUMBER
GIVETH A NUMBER
collision_test x y MEANS x + y
|]

spacedParamsJL4 :: Text
spacedParamsJL4 = [i|
GIVEN `first value` IS A NUMBER
    , `second value` IS A NUMBER
GIVETH A NUMBER
spaced_params `first value` `second value` MEANS `first value` + `second value`
|]

roleCheckJL4 :: Text
roleCheckJL4 = [i|
DECLARE `Employee Role` IS ONE OF
    Manager
    Engineer
    Analyst

GIVEN `employee role` IS AN `Employee Role`
    , `years of service` IS A NUMBER
GIVETH A BOOLEAN
role_check `employee role` `years of service` MEANS
    CONSIDER `employee role`
        WHEN Manager THEN `years of service` >= 3
        WHEN Engineer THEN `years of service` >= 2
        WHEN Analyst THEN `years of service` >= 1
|]

nestedRecordJL4 :: Text
nestedRecordJL4 = [i|
DECLARE Person HAS
    name IS A STRING
  , age IS A NUMBER

GIVEN person IS A Person
GIVETH A BOOLEAN
nested_record_test person MEANS person's age >= 18
|]

mixedParamsJL4 :: Text
mixedParamsJL4 = [i|
GIVEN `is active` IS A BOOLEAN
    , `threshold value` IS A NUMBER
    , `is verified` IS A BOOLEAN
GIVETH A BOOLEAN
mixed_params `is active` `threshold value` `is verified` MEANS
    `is active` AND `is verified` AND `threshold value` > 50
|]


-- Helper functions (copied from IntegrationSpec)
requireSuccess :: (MonadIO m) => SimpleResponse -> m ResponseWithReason
requireSuccess resp = case resp of
  SimpleResponse s -> pure s
  SimpleError evalError -> do
    liftIO $ expectationFailure (show evalError)
    error "Impossible"

runDecisionService :: (AppClient -> ClientM a) -> IO a
runDecisionService act = do
  resOrExc <- try (runDecisionService' act)
  case resOrExc of
    Left ioe ->
      if isPermissionError ioe
        then pendingWith ("Skipping integration test (cannot bind sockets in this environment): " <> show ioe) >> pure undefined
        else do
          expectationFailure (show ioe)
          pure undefined
    Right result ->
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
