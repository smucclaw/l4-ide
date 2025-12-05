module Options (
  Options (..),
  optionsParser,
) where

import qualified Data.Text as Text
import Options.Applicative
import Servant.Client

data Options = Options
  { port :: Int
  , serverName :: Maybe Text.Text
  , sourcePaths :: [FilePath]  -- New field for source files or directories
  , websessionsUrl :: BaseUrl
  }

optionsParser :: Parser Options
optionsParser = do
  Options
    <$> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 8081
                <> help "HTTP port to use"
            )
    <*> optional
      ( strOption
          ( long "serverName"
              <> short 's'
              <> metavar "URL"
              <> help "Name of the server. Exposed in swagger.json field."
          )
      )
    <*> many
      ( strOption
          ( long "sourcePaths"
              <> short 'f'
              <> metavar "SOURCE"
              <> help "One or more files or directories containing L4 source files to expose"
          )
      )
    <*> do
      BaseUrl
        <$> flag Http Https
              ( long "websessionsSecure"
                  <> help "Use HTTPS to connect to websessions service"
                  <> showDefault
              )
        <*> strOption
              ( long "websessionsHost"
                  <> help "Hostname of websessions service (web IDE backend)"
                  <> value "localhost"
                  <> showDefault
              )
        <*> option auto
              ( long "websessionsPort"
                  <> help "Port of websessions service"
                  <> value 5008
                  <> showDefault
              )
        <*> strOption
              ( long "websessionsPath"
                  <> help "Path prefix for websessions service"
                  <> value mempty
                  <> showDefault
              )
