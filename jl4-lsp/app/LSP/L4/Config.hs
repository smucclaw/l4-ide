module LSP.L4.Config where

import qualified Data.Aeson as J
import qualified GHC.Generics as GHC

-- | Server-side configuration.
-- Note: Settings like 'serverExecutablePath' are client-side only (used by
-- VS Code to know which binary to launch) and should NOT be included here.
-- Including client-side settings causes unnecessary build session restarts
-- when VS Code sends configuration updates.
data Config = Config
    { -- Currently empty - add server-relevant settings here as needed
      -- e.g., maxCompletions, enableExperimentalFeatures, etc.
    }
    deriving (Eq, GHC.Generic, J.ToJSON, Show)

instance J.FromJSON Config where
    -- NOTE: monaco currently sends [null] for config, so that
    -- should not result in an error
    -- We ignore all fields since Config is currently empty
    parseJSON _ = pure defConfig

defConfig :: Config
defConfig = Config {}
