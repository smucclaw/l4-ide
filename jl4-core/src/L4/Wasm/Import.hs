{-# LANGUAGE OverloadedStrings #-}
-- | Import resolution for WASM builds.
--
-- This module provides import resolution for the WASM backend using the
-- shared resolution logic from 'L4.Import.Resolution'. It uses:
--
-- * Embedded core libraries (bundled at compile time from @libraries/*.l4@)
-- * VFS (Virtual File System) provided by JavaScript via FFI
--
-- The resolution order is: embedded libraries first, then VFS.
--
-- == Usage
--
-- @
-- -- JavaScript provides files via VFS
-- let vfs = vfsFromList [("helper", helperSource)]
-- result <- checkWithImports vfs mainSource
-- @
--
-- @since 0.1
module L4.Wasm.Import
  ( -- * Virtual File System
    VFS
  , emptyVFS
  , vfsLookup
  , vfsInsert
  , vfsFromList
  , vfsKeys
    -- * Embedded Libraries (re-exported)
  , embeddedLibraries
  , embeddedLibraryNames
  , lookupEmbeddedLibrary
    -- * Type Checking with Imports
  , checkWithImports
  , checkWithImportsAndUri
    -- * Re-exports from shared resolution
  , TypeCheckWithDepsResult(..)
  , ResolvedImport(..)
  , extractImportNames
  , moduleNameToProjectUri
  ) where

import Base
import qualified Base.Text as Text
import qualified Data.Map.Strict as Map
import Control.Applicative ((<|>))

-- Re-export embedded libraries
import L4.Wasm.EmbeddedLibraries (embeddedLibraries, embeddedLibraryNames, lookupEmbeddedLibrary)

-- Use shared resolution logic
import L4.Import.Resolution
  ( ModuleLookup
  , extractImportNames
  , moduleNameToProjectUri
  , typecheckWithDependencies
  , TypeCheckWithDepsResult(..)
  , ResolvedImport(..)
  )

-- ----------------------------------------------------------------------------
-- Virtual File System
-- ----------------------------------------------------------------------------

-- | Virtual File System for storing project files.
-- Files are indexed by module name (e.g., "helper" not "helper.l4").
newtype VFS = VFS { unVFS :: Map Text Text }
  deriving stock (Eq, Show)

-- | Empty VFS.
emptyVFS :: VFS
emptyVFS = VFS Map.empty

-- | Look up a file in the VFS by module name.
vfsLookup :: Text -> VFS -> Maybe Text
vfsLookup moduleName (VFS m) =
  -- Try both with and without .l4 extension
  Map.lookup moduleName m
  <|> Map.lookup (moduleName <> ".l4") m
  <|> Map.lookup (fromMaybe moduleName (Text.stripSuffix ".l4" moduleName)) m

-- | Insert or update a file in the VFS.
vfsInsert :: Text -> Text -> VFS -> VFS
vfsInsert moduleName contents (VFS m) = VFS (Map.insert moduleName contents m)

-- | Create a VFS from a list of (moduleName, contents) pairs.
vfsFromList :: [(Text, Text)] -> VFS
vfsFromList = VFS . Map.fromList

-- | Get all module names in the VFS.
vfsKeys :: VFS -> [Text]
vfsKeys (VFS m) = Map.keys m

-- ----------------------------------------------------------------------------
-- Module Lookup for WASM
-- ----------------------------------------------------------------------------

-- | Create a module lookup function for WASM.
-- Checks embedded libraries first, then VFS.
wasmModuleLookup :: VFS -> ModuleLookup Identity
wasmModuleLookup vfs modName = Identity $
  lookupEmbeddedLibrary modName `mplus` vfsLookup modName vfs

-- ----------------------------------------------------------------------------
-- Type Checking with Imports
-- ----------------------------------------------------------------------------

-- | Type-check a module with import resolution.
--
-- This function:
-- 1. Parses the main source
-- 2. Extracts IMPORT statements
-- 3. Recursively resolves imports (from embedded libs + VFS)
-- 4. Type-checks all modules in dependency order
--
-- Returns 'Left' with error messages if resolution/parsing fails,
-- 'Right' with the type-check result otherwise.
checkWithImports :: VFS -> Text -> Either [Text] TypeCheckWithDepsResult
checkWithImports vfs source =
  checkWithImportsAndUri vfs "main" source

-- | Type-check with a specific module name.
checkWithImportsAndUri :: VFS -> Text -> Text -> Either [Text] TypeCheckWithDepsResult
checkWithImportsAndUri vfs moduleName source =
  let uri = moduleNameToProjectUri moduleName
      lookup' = wasmModuleLookup vfs
      Identity result = typecheckWithDependencies lookup' uri source
  in result
