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
  , crudServerName :: BaseUrl
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
              ( long "crudServerSecure"
                  <> help "Whether or not the server should be securely connected to."
                  <> showDefault
              )
        <*> strOption
              ( long "crudServerName"
                  <> help "Name of the server for sessions"
                  <> value "localhost"
                  <> showDefault
              )
        <*> option auto
              ( long "crudServerPort"
                  <> help "Port of the server for sessions."
                  <> value 5008
                  <> showDefault
              )
        <*> strOption
              ( long "crudServerPath"
                  <> help "path on the server for sessions. When provided, assumes https"
                  <> value mempty
                  <> showDefault
              )
