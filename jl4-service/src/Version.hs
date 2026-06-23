{-# LANGUAGE TemplateHaskell #-}

-- | The jl4-service build version, baked into the binary at compile time.
--
-- The release workflow (@.github/workflows/main-tag.yml@) computes
-- @1.5.<run_number>@ and exports it as the @JL4_SERVICE_VERSION@ environment
-- variable for the @cabal build exe:jl4-service@ step. A Template Haskell
-- splice captures that value into the binary, so the running service can
-- self-report its version (on @\/health@) and derive its major component for
-- per-deployment versioning. Local builds with the variable unset fall back to
-- @"0.0.0-dev"@ (major 0).
module Version
  ( serviceVersion
  , serviceMajor
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (runIO, stringE)
import System.Environment (lookupEnv)

-- | Full service version string (e.g. @"1.5.79"@), or @"0.0.0-dev"@ for a
-- local build where @JL4_SERVICE_VERSION@ was unset at compile time.
serviceVersion :: Text
serviceVersion =
  Text.pack
    $( do
         mv <- runIO (lookupEnv "JL4_SERVICE_VERSION")
         stringE (fromMaybe "0.0.0-dev" mv)
     )

-- | Major component of 'serviceVersion' (the @1@ in @1.5.79@). Falls back to
-- @0@ when the version has no leading integer (e.g. the @"0.0.0-dev"@ default).
serviceMajor :: Int
serviceMajor =
  case TextRead.decimal (Text.takeWhile (/= '.') serviceVersion) of
    Right (n, _) -> n
    Left _       -> 0
