{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Embedded L4 core libraries for WASM builds.
--
-- This module uses Template Haskell to embed the contents of L4 library files
-- at compile time. This allows the WASM module to resolve IMPORT statements
-- for core libraries without needing filesystem access.
--
-- The libraries are read from the @data-files@ specified in @jl4-core.cabal@
-- (i.e., @libraries/*.l4@) during compilation.
--
-- @since 0.1
module L4.Wasm.EmbeddedLibraries
  ( embeddedLibraries
  , embeddedLibraryNames
  , lookupEmbeddedLibrary
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Language.Haskell.TH.Syntax (runIO)

import qualified Paths_jl4_core

-- Import TH helpers from separate module (GHC stage restriction)
import L4.Wasm.EmbeddedLibraries.TH (embedLibrariesFromDir)

-- | List of embedded library names (without .l4 extension).
embeddedLibraryNames :: [Text]
embeddedLibraryNames = Map.keys embeddedLibraries

-- | Look up an embedded library by name.
-- The name should be without the .l4 extension (e.g., "prelude", "math").
lookupEmbeddedLibrary :: Text -> Maybe Text
lookupEmbeddedLibrary = flip Map.lookup embeddedLibraries

-- | Embedded libraries as a Map from name to source code.
-- The name is without the .l4 extension.
-- This is computed at compile time using Template Haskell.
embeddedLibraries :: Map Text Text
embeddedLibraries = Map.fromList $ map (\(n, c) -> (Text.pack n, Text.pack c)) $(do
  -- Get the data directory at compile time
  dataDir <- runIO Paths_jl4_core.getDataDir
  let libDir = dataDir </> "libraries"
  
  -- Check if directory exists by looking for prelude.l4
  libDirExists <- runIO $ doesFileExist (libDir </> "prelude.l4")
  
  if libDirExists
    then embedLibrariesFromDir libDir
    else do
      -- Fallback: try relative path for development
      let devLibDir = "libraries"
      devExists <- runIO $ doesFileExist (devLibDir </> "prelude.l4")
      if devExists
        then embedLibrariesFromDir devLibDir
        else do
          -- Try jl4-core/libraries for building from repo root
          let repoLibDir = "jl4-core" </> "libraries"
          repoExists <- runIO $ doesFileExist (repoLibDir </> "prelude.l4")
          if repoExists
            then embedLibrariesFromDir repoLibDir
            else [| [] |]  -- No libraries found, return empty list
  )
