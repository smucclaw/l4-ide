module Options (
  Options (..),
  buildOpts,
) where

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { port                  :: !Int
  , storePath             :: !FilePath
  , serverName            :: !(Maybe Text)
  , lazyLoad              :: !Bool
  , debug                 :: !Bool
  , maxZipSize            :: !Int
  , maxFileCount          :: !Int
  , maxDeployments        :: !Int
  , maxConcurrentRequests :: !Int
  , maxEvalMemoryMb       :: !Int
  , evalTimeout           :: !Int
  , compileTimeout        :: !Int
  }

-- | Build the CLI parser with environment variable defaults.
-- Env vars are read first; CLI args override them.
buildOpts :: IO (ParserInfo Options)
buildOpts = do
  envPort       <- readEnvInt "JL4_PORT"
  envStorePath  <- lookupEnv "JL4_STORE_PATH"
  envServerName <- lookupEnv "JL4_SERVER_NAME"
  envLazyLoad   <- readEnvBool "JL4_LAZY_LOAD"
  envDebug      <- readEnvBool "JL4_DEBUG"
  envMaxZip     <- readEnvInt "JL4_MAX_ZIP_SIZE"
  envMaxFiles   <- readEnvInt "JL4_MAX_FILE_COUNT"
  envMaxDeploy  <- readEnvInt "JL4_MAX_DEPLOYMENTS"
  envMaxConc    <- readEnvInt "JL4_MAX_CONCURRENT_REQUESTS"
  envMaxMem     <- readEnvInt "JL4_MAX_EVAL_MEMORY_MB"
  envEvalTO     <- readEnvInt "JL4_EVAL_TIMEOUT"
  envCompTO     <- readEnvInt "JL4_COMPILE_TIMEOUT"

  let parser = Options
        <$> option auto
              ( long "port"
                  <> short 'p'
                  <> metavar "PORT"
                  <> value (maybe 8080 id envPort)
                  <> showDefault
                  <> help "HTTP port to listen on (env: JL4_PORT)"
              )
        <*> strOption
              ( long "store-path"
                  <> metavar "PATH"
                  <> value (maybe "/tmp/jl4-store" id envStorePath)
                  <> showDefault
                  <> help "Directory for persisting deployment bundles (env: JL4_STORE_PATH)"
              )
        <*> optional
              ( strOption
                  ( long "server-name"
                      <> short 's'
                      <> metavar "NAME"
                      <> maybe mempty value (Text.pack <$> envServerName)
                      <> help "Server name exposed in OpenAPI metadata (env: JL4_SERVER_NAME)"
                  )
              )
        <*> ( switch
                ( long "lazy-load"
                    <> help "Compile deployments on first request instead of at startup (env: JL4_LAZY_LOAD)"
                )
              <|> pure (maybe False id envLazyLoad)
            )
        <*> ( switch
                ( long "debug"
                    <> help "Enable debug mode: verbose logging and detailed error responses (env: JL4_DEBUG)"
                )
              <|> pure (maybe False id envDebug)
            )
        <*> option auto
              ( long "max-zip-size"
                  <> metavar "BYTES"
                  <> value (maybe 2097152 id envMaxZip)
                  <> showDefault
                  <> help "Maximum upload zip file size in bytes (env: JL4_MAX_ZIP_SIZE)"
              )
        <*> option auto
              ( long "max-file-count"
                  <> metavar "COUNT"
                  <> value (maybe 5096 id envMaxFiles)
                  <> showDefault
                  <> help "Maximum number of files per deployment zip (env: JL4_MAX_FILE_COUNT)"
              )
        <*> option auto
              ( long "max-deployments"
                  <> metavar "COUNT"
                  <> value (maybe 1024 id envMaxDeploy)
                  <> showDefault
                  <> help "Maximum number of concurrent deployments (env: JL4_MAX_DEPLOYMENTS)"
              )
        <*> option auto
              ( long "max-concurrent-requests"
                  <> metavar "COUNT"
                  <> value (maybe 20 id envMaxConc)
                  <> showDefault
                  <> help "Maximum number of concurrent requests (env: JL4_MAX_CONCURRENT_REQUESTS)"
              )
        <*> option auto
              ( long "max-eval-memory-mb"
                  <> metavar "MB"
                  <> value (maybe 256 id envMaxMem)
                  <> showDefault
                  <> help "Maximum memory allocation per evaluation in MB (env: JL4_MAX_EVAL_MEMORY_MB)"
              )
        <*> option auto
              ( long "eval-timeout"
                  <> metavar "SECONDS"
                  <> value (maybe 60 id envEvalTO)
                  <> showDefault
                  <> help "Evaluation timeout in seconds (env: JL4_EVAL_TIMEOUT)"
              )
        <*> option auto
              ( long "compile-timeout"
                  <> metavar "SECONDS"
                  <> value (maybe 60 id envCompTO)
                  <> showDefault
                  <> help "Compilation timeout in seconds (env: JL4_COMPILE_TIMEOUT)"
              )

  pure $ info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Multi-tenant decision service for L4 rule bundles"
        <> header "jl4-service — persistent, multi-tenant L4 decision service"
    )

-- | Read an optional integer from an environment variable.
readEnvInt :: String -> IO (Maybe Int)
readEnvInt name = do
  mVal <- lookupEnv name
  pure (mVal >>= readMaybe)

-- | Read an optional boolean from an environment variable.
-- Accepts "1", "true", "yes" (case-insensitive) as True.
readEnvBool :: String -> IO (Maybe Bool)
readEnvBool name = do
  mVal <- lookupEnv name
  pure $ mVal >>= \val ->
    case map toLower val of
      "1"    -> Just True
      "true" -> Just True
      "yes"  -> Just True
      "0"     -> Just False
      "false" -> Just False
      "no"    -> Just False
      _      -> Nothing
