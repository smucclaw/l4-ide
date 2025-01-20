module LSP.L4.Config where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified GHC.Generics as GHC

data Config = Config
  { serverExecutablePath :: Maybe Text
  }
  deriving (GHC.Generic, J.ToJSON, J.FromJSON, Show)

defConfig :: Config
defConfig = Config
  { serverExecutablePath = Nothing
  }
