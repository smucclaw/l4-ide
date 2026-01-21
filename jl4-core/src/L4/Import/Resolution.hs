{-# LANGUAGE OverloadedStrings #-}
-- | Shared import resolution logic for L4.
--
-- This module provides the core import resolution algorithm that can be used
-- by both the LSP (with Shake) and WASM (pure) backends. The resolution logic
-- is parameterized over a monad and a module lookup function, allowing different
-- backends to provide their own implementations.
--
-- == Architecture
--
-- @
--                     L4.Import.Resolution
--                     (this module - pure logic)
--                              │
--              ┌───────────────┴───────────────┐
--              │                               │
--         LSP Backend                    WASM Backend
--    (Shake + filesystem)            (VFS + embedded libs)
-- @
--
-- == Usage
--
-- Backends implement 'ModuleLookup' to specify how to find module sources:
--
-- @
-- -- Pure implementation with VFS
-- pureResolver :: VFS -> ModuleLookup (Either [Text])
-- pureResolver vfs modName = pure $ vfsLookup modName vfs
--
-- -- Shake-based implementation  
-- shakeResolver :: ModuleLookup Action
-- shakeResolver modName = use GetFileContents (projectUri modName)
-- @
--
-- @since 0.1
module L4.Import.Resolution
  ( -- * Module Lookup
    ModuleLookup
    -- * URI Generation
  , moduleNameToProjectUri
  , moduleNameToCandidateUris
  , CandidateUri(..)
    -- * Import Extraction
  , extractImportNames
    -- * Resolution
  , resolveImports
  , ResolvedImport(..)
  , ImportResolutionError(..)
    -- * Type Checking with Dependencies
  , typecheckWithDependencies
  , TypeCheckWithDepsResult(..)
  ) where

import Base
import qualified Base.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import L4.Annotation (emptyAnno)
import L4.Lexer (PError(..))
import L4.Syntax
import L4.Parser (execProgramParserWithHintPass)
import L4.TypeCheck (doCheckProgramWithDependencies, initialCheckState, initialCheckEnv, applyFinalSubstitution)
import L4.TypeCheck.Types (CheckResult(..), CheckErrorWithContext, Substitution, Environment, EntityInfo, MixfixRegistry, InfoMap, ScopeMap, NlgMap, DescMap)
import qualified L4.TypeCheck as TypeCheck
import qualified Data.List as List
import qualified L4.Utils.IntervalMap as IV

-- ----------------------------------------------------------------------------
-- Module Lookup Abstraction
-- ----------------------------------------------------------------------------

-- | Function to look up a module's source code by name.
--
-- Returns 'Nothing' if the module cannot be found.
-- The module name is without the .l4 extension (e.g., "prelude", "helper").
type ModuleLookup m = Text -> m (Maybe Text)

-- ----------------------------------------------------------------------------
-- URI Generation
-- ----------------------------------------------------------------------------

-- | A candidate URI for module resolution, with its source.
data CandidateUri = CandidateUri
  { cuUri :: NormalizedUri
  , cuSource :: Text  -- ^ Description of where this URI comes from (for error messages)
  }
  deriving stock (Eq, Show)

-- | Convert a module name to a @project:/@ URI.
--
-- This is the canonical URI scheme for VFS files:
-- @
-- moduleNameToProjectUri "helper" == "project:/helper.l4"
-- @
moduleNameToProjectUri :: Text -> NormalizedUri
moduleNameToProjectUri modName =
  toNormalizedUri $ Uri $ "project:/" <> modName <> ".l4"

-- | Generate candidate URIs for a module name.
--
-- Returns URIs in priority order:
-- 1. @project:/modName.l4@ - VFS lookup
-- 2. Relative to importing file (if base URI provided)
-- 3. Relative to root directory (if root provided)
moduleNameToCandidateUris
  :: Maybe NormalizedUri  -- ^ URI of the importing file (for relative resolution)
  -> Maybe FilePath       -- ^ Root directory (for absolute resolution)
  -> Text                 -- ^ Module name
  -> [CandidateUri]
moduleNameToCandidateUris mBaseUri mRoot modName =
  [ CandidateUri (moduleNameToProjectUri modName) "project VFS"
  ] 
  <> maybe [] relativeCandidate mBaseUri
  <> maybe [] rootCandidate mRoot
  where
    relativeCandidate _baseUri =
      -- TODO: compute relative URI from base
      []
    rootCandidate _root =
      -- TODO: compute file:// URI from root
      []

-- ----------------------------------------------------------------------------
-- Import Extraction
-- ----------------------------------------------------------------------------

-- | Extract import names from a parsed module.
--
-- Returns module names without the .l4 extension.
extractImportNames :: Module Name -> [Text]
extractImportNames = foldTopDecls extractImport
  where
    extractImport :: TopDecl Name -> [Text]
    extractImport = \case
      Import _ (MkImport _ n _) -> [rawNameToText (rawName n)]
      _ -> []

-- ----------------------------------------------------------------------------
-- Import Resolution
-- ----------------------------------------------------------------------------

-- | A successfully resolved import.
data ResolvedImport = ResolvedImport
  { riModuleName :: Text
  , riUri :: NormalizedUri
  , riSource :: Text
  , riParsed :: Module Name
  , riTypeChecked :: CheckResult
  }
  deriving stock (Generic)

-- | Errors that can occur during import resolution.
data ImportResolutionError
  = ModuleNotFound Text [Text]  -- ^ Module name, list of places searched
  | ParseError Text [Text]      -- ^ Module name, parse error messages
  | CyclicImport [Text]         -- ^ Cycle of module names
  deriving stock (Eq, Show, Generic)

-- | Resolve all imports for a module, recursively.
--
-- This function:
-- 1. Extracts IMPORT statements from the parsed module
-- 2. Looks up each import using the provided lookup function
-- 3. Recursively resolves transitive imports
-- 4. Type-checks imports in dependency order
-- 5. Returns all resolved imports (including transitive)
--
-- Cycle detection prevents infinite loops. We track two sets:
-- - @inProgress@: modules currently being resolved (detecting these = cycle)
-- - @resolved@: modules already fully resolved (can skip these)
resolveImports
  :: forall m. Monad m
  => ModuleLookup m                      -- ^ How to find module sources
  -> Set.Set Text                        -- ^ Modules currently being resolved (for cycle detection)
  -> Set.Set Text                        -- ^ Modules already fully resolved (to skip)
  -> [Text]                              -- ^ Import names to resolve
  -> m (Either ImportResolutionError [ResolvedImport])
resolveImports _lookup _inProgress _resolved [] = pure $ Right []
resolveImports lookupModule inProgress resolved (modName : rest)
  -- Already resolved - skip it
  | modName `Set.member` resolved =
      resolveImports lookupModule inProgress resolved rest
  -- Currently being resolved - cycle detected!
  | modName `Set.member` inProgress = 
      pure $ Left $ CyclicImport $ toList inProgress <> [modName]
  | otherwise = do
      -- Look up the module source
      mSource <- lookupModule modName
      case mSource of
        Nothing -> 
          pure $ Left $ ModuleNotFound modName ["embedded libraries", "project VFS"]
        
        Just source -> do
          let uri = moduleNameToProjectUri modName
          
          -- Parse the module
          case execProgramParserWithHintPass uri source of
            Left parseErrors ->
              pure $ Left $ ParseError modName $ map (\(PError msg _ _) -> msg) (toList parseErrors)
            
            Right (parsed, _hints, _warnings) -> do
              -- Extract and resolve transitive imports first
              -- Add this module to inProgress while resolving its imports
              let transitiveImports = extractImportNames parsed
                  newInProgress = Set.insert modName inProgress
              
              transitiveResult <- resolveImports lookupModule newInProgress resolved transitiveImports
              
              case transitiveResult of
                Left err -> pure $ Left err
                Right transitiveResolved -> do
                  -- Type-check this module with its dependencies
                  let (depState, depEnv) = combineResolvedImports uri transitiveResolved
                      updatedParsed = updateModuleImports transitiveResolved parsed
                      tcResult = doCheckProgramWithDependencies depState depEnv updatedParsed
                  
                  let thisResolved = ResolvedImport
                        { riModuleName = modName
                        , riUri = uri
                        , riSource = source
                        , riParsed = parsed
                        , riTypeChecked = tcResult
                        }
                  
                  -- This module is now fully resolved
                  -- Collect all resolved module names from transitive imports
                  let transitiveNames = Set.fromList $ map (\ri -> ri.riModuleName) transitiveResolved
                      newResolved = Set.insert modName $ Set.union resolved transitiveNames
                  
                  -- Continue with remaining imports (back to original inProgress)
                  restResult <- resolveImports lookupModule inProgress newResolved rest
                  
                  case restResult of
                    Left err -> pure $ Left err
                    Right restResolved ->
                      -- Return this module + transitive + rest
                      -- Deduplicate by module name (keep first occurrence)
                      let allResolved = thisResolved : transitiveResolved <> restResolved
                          seen = Set.empty :: Set.Set Text
                          deduped = deduplicateResolved seen allResolved
                      in pure $ Right deduped
  where
    deduplicateResolved :: Set.Set Text -> [ResolvedImport] -> [ResolvedImport]
    deduplicateResolved _ [] = []
    deduplicateResolved seen (ri : ris)
      | ri.riModuleName `Set.member` seen = deduplicateResolved seen ris
      | otherwise = ri : deduplicateResolved (Set.insert ri.riModuleName seen) ris

-- ----------------------------------------------------------------------------
-- Type Checking with Dependencies
-- ----------------------------------------------------------------------------

-- | Result of type-checking with resolved dependencies.
data TypeCheckWithDepsResult = TypeCheckWithDepsResult
  { tcdModule :: Module Resolved
  , tcdErrors :: [CheckErrorWithContext]
  , tcdSubstitution :: Substitution
  , tcdEnvironment :: Environment
  , tcdEntityInfo :: EntityInfo
  , tcdMixfixRegistry :: MixfixRegistry
  , tcdInfoMap :: InfoMap
  , tcdScopeMap :: ScopeMap
  , tcdNlgMap :: NlgMap
  , tcdDescMap :: DescMap
  , tcdSuccess :: Bool
  , tcdResolvedImports :: [ResolvedImport]
  }
  deriving stock (Generic)

-- | Type-check a module with its dependencies.
--
-- This is the main entry point for type-checking with import resolution.
typecheckWithDependencies
  :: forall m. Monad m
  => ModuleLookup m     -- ^ How to find module sources
  -> NormalizedUri      -- ^ URI of the main module
  -> Text               -- ^ Source code of the main module
  -> m (Either [Text] TypeCheckWithDepsResult)
typecheckWithDependencies lookupModule uri source = do
  -- Parse the main module
  case execProgramParserWithHintPass uri source of
    Left parseErrors ->
      pure $ Left $ map (\(PError msg _ _) -> msg) (toList parseErrors)
    
    Right (parsed, _hints, _warnings) -> do
      -- Extract and resolve imports
      let importNames = extractImportNames parsed
      
      importResult <- resolveImports lookupModule Set.empty Set.empty importNames
      
      case importResult of
        Left (ModuleNotFound modName searched) ->
          pure $ Left ["Module not found: " <> modName <> " (searched: " <> Text.intercalate ", " searched <> ")"]
        Left (ParseError modName errs) ->
          pure $ Left $ ("Parse error in " <> modName <> ":") : errs
        Left (CyclicImport cycleModules) ->
          pure $ Left ["Cyclic import detected: " <> Text.intercalate " -> " cycleModules]
        
        Right resolvedImports -> do
          -- Combine environments from imports
          let (initState, initEnv) = combineResolvedImports uri resolvedImports
              updatedParsed = updateModuleImports resolvedImports parsed
              result = doCheckProgramWithDependencies initState initEnv updatedParsed
          
          pure $ Right TypeCheckWithDepsResult
            { tcdModule = result.program
            , tcdErrors = result.errors
            , tcdSubstitution = result.substitution
            , tcdEnvironment = result.environment
            , tcdEntityInfo = result.entityInfo
            , tcdMixfixRegistry = result.mixfixRegistry
            , tcdInfoMap = result.infoMap
            , tcdScopeMap = result.scopeMap
            , tcdNlgMap = result.nlgMap
            , tcdDescMap = result.descMap
            , tcdSuccess = null result.errors
            , tcdResolvedImports = resolvedImports
            }

-- ----------------------------------------------------------------------------
-- Internal Helpers
-- ----------------------------------------------------------------------------

-- | Combine resolved imports into initial CheckState and CheckEnv.
combineResolvedImports :: NormalizedUri -> [ResolvedImport] -> (TypeCheck.CheckState, TypeCheck.CheckEnv)
combineResolvedImports uri imports =
  let
    baseState = initialCheckState
    baseEnv = initialCheckEnv uri
    
    (finalState, finalEnv) = List.foldl' combineOne (baseState, baseEnv) imports
  in
    -- Clear substitution to avoid leaking inference variables
    ( TypeCheck.MkCheckState
        { TypeCheck.substitution = Map.empty
        , TypeCheck.supply = finalState.supply
        , TypeCheck.infoMap = finalState.infoMap
        , TypeCheck.nlgMap = finalState.nlgMap
        , TypeCheck.scopeMap = finalState.scopeMap
        , TypeCheck.descMap = finalState.descMap
        }
    , finalEnv
    )
  where
    combineOne 
      :: (TypeCheck.CheckState, TypeCheck.CheckEnv) 
      -> ResolvedImport 
      -> (TypeCheck.CheckState, TypeCheck.CheckEnv)
    combineOne (accState, accEnv) ri =
      let r = ri.riTypeChecked
          -- IMPORTANT: Apply the substitution to entityInfo to resolve type variables.
          -- Without this, types like 'r25' would leak instead of being resolved to
          -- their actual types (e.g., NUMBER). This matches what the native LSP does
          -- in LSP.L4.Rules when combining TypeCheckResult dependencies.
          resolvedEntityInfo = applyFinalSubstitution r.substitution ri.riUri r.entityInfo
      in ( TypeCheck.MkCheckState
             { TypeCheck.substitution = r.substitution
             , TypeCheck.supply = accState.supply
             , TypeCheck.infoMap = IV.empty
             , TypeCheck.nlgMap = IV.empty
             , TypeCheck.scopeMap = IV.empty
             , TypeCheck.descMap = IV.empty
             }
         , TypeCheck.MkCheckEnv
             { TypeCheck.moduleUri = accEnv.moduleUri
             , TypeCheck.environment = Map.unionWith List.union accEnv.environment r.environment
             , TypeCheck.entityInfo = Map.unionWith (\t1 t2 -> if t1 == t2 then t1 else t1) accEnv.entityInfo resolvedEntityInfo
             , TypeCheck.functionTypeSigs = Map.empty
             , TypeCheck.declTypeSigs = Map.empty
             , TypeCheck.declareDeclarations = Map.empty
             , TypeCheck.assumeDeclarations = Map.empty
             , TypeCheck.mixfixRegistry = Map.unionWith (<>) accEnv.mixfixRegistry r.mixfixRegistry
             , TypeCheck.errorContext = TypeCheck.None
             , TypeCheck.sectionStack = []
             }
         )

-- | Update a module's import declarations with resolved URIs.
updateModuleImports :: [ResolvedImport] -> Module Name -> Module Name
updateModuleImports imports = overImports (updateImport importMapping)
  where
    importMapping :: [(Name, NormalizedUri)]
    importMapping =
      [ (MkName emptyAnno (NormalName ri.riModuleName), ri.riUri)
      | ri <- imports
      ]
