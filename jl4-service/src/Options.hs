module Options (
  Options (..),
  optionsParser,
  opts,
) where

import Data.Text (Text)
import Options.Applicative

data Options = Options
  { port       :: !Int
  , storePath  :: !FilePath
  , serverName :: !(Maybe Text)
  , lazyLoad   :: !Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option auto
          ( long "port"
              <> short 'p'
              <> metavar "PORT"
              <> value 8080
              <> showDefault
              <> help "HTTP port to listen on"
          )
    <*> strOption
          ( long "store-path"
              <> metavar "PATH"
              <> value "/tmp/jl4-store"
              <> showDefault
              <> help "Directory for persisting deployment bundles"
          )
    <*> optional
          ( strOption
              ( long "server-name"
                  <> short 's'
                  <> metavar "NAME"
                  <> help "Server name exposed in OpenAPI metadata"
              )
          )
    <*> switch
          ( long "lazy-load"
              <> help "Compile deployments on first request instead of at startup"
          )

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Multi-tenant decision service for L4 rule bundles"
        <> header "jl4-service — persistent, multi-tenant L4 decision service"
    )
