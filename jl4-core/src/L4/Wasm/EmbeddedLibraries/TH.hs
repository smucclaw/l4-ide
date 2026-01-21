{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell helpers for embedding L4 libraries.
--
-- This module is separate from 'L4.Wasm.EmbeddedLibraries' to satisfy
-- GHC's stage restriction (TH helpers must be defined in a separate module
-- from where they're used in splices).
--
-- @since 0.1
module L4.Wasm.EmbeddedLibraries.TH
  ( embedLibrariesFromDir
  , embedOneLibrary
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)

-- | Template Haskell helper to embed all .l4 files from a directory.
-- Returns an expression of type @[(Text, Text)]@.
embedLibrariesFromDir :: FilePath -> Q Exp
embedLibrariesFromDir libDir = do
  -- List all .l4 files
  files <- runIO $ do
    allFiles <- listDirectory libDir
    pure $ filter ((== ".l4") . takeExtension) allFiles
  
  -- Read each file and create a list expression
  libs <- mapM (embedOneLibrary libDir) files
  listE (map pure libs)

-- | Template Haskell helper to embed a single library file.
-- Returns an expression of type @(Text, Text)@.
embedOneLibrary :: FilePath -> FilePath -> Q Exp
embedOneLibrary libDir fileName = do
  let name = takeBaseName fileName
      path = libDir </> fileName
  
  -- Register dependency so rebuilds happen when files change
  addDependentFile path
  
  -- Read file contents at compile time
  contents <- runIO $ readFile path
  
  -- Create the tuple expression: (Text.pack name, Text.pack contents)
  [| (name, contents) |]
