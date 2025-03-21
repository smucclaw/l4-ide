module LSP.L4.Config where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified GHC.Generics as GHC

data Config = Config
    { serverExecutablePath :: Maybe Text
    }
    deriving (GHC.Generic, J.ToJSON, Show)

instance J.FromJSON Config where
    -- NOTE: monaco currently sends [null] for config, so that
    -- should not result in an error, especially since the executable path
    -- is irrelevant in that case
    parseJSON J.Null = pure defConfig
    parseJSON x = J.genericParseJSON J.defaultOptions x

defConfig :: Config
defConfig =
    Config
        { serverExecutablePath = Nothing
        }
