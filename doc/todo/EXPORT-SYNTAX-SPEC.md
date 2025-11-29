# Specification: EXPORT Syntax for Decision Service Functions

## Executive Summary

This document specifies a mechanism for L4 files to declare which functions should be exposed via the decision service REST API, eliminating the need for separate `.yaml` metadata files.

The approach uses the existing `@desc` annotation infrastructure with a simple convention: lines starting with `@desc default` or `@desc export` mark the following function as an API export.

## Motivation

### Current State

To expose a function via the decision service, you currently need:

1. **An L4 file** (`myfunction.l4`) containing the function definition
2. **A YAML file** (`myfunction.yaml`) containing metadata:
   ```yaml
   name: myFunction
   description: "This function does X"
   parameters:
     type: object
     properties:
       age:
         type: number
         description: "The person's age"
     required: [age]
   supportedEvalBackend: [jl4]
   ```

This creates several problems:

- **Duplication**: Parameter names/types must match between L4 and YAML
- **Synchronization**: Changes to L4 function signatures require YAML updates
- **Discoverability**: Hard to see at a glance which functions are API-exposed
- **Deployment friction**: Two files per function

### Desired State

A single L4 file declares everything:

```l4
@desc export This function determines if a person is eligible
GIVEN age IS A Number @desc The person's age in years
GIVETH A Boolean
isEligible age MEANS age >= 18
```

Or for the primary/default function of a module:

```l4
@desc default export Main eligibility checker for the application
GIVEN applicant IS A Person @desc The applicant's details
GIVETH A EligibilityResult
checkEligibility applicant MEANS ...
```

## Design

### Annotation Syntax

We extend the existing `@desc` annotation with keyword prefixes:

```
@desc [default] [export] <description text>
```

Where:

- **`export`**: This function should be exposed via the decision service API
- **`default`**: This is the primary function when the module is called without specifying a function name (implies `export`)
- **`<description text>`**: Human-readable description for API documentation

#### Examples

```l4
-- Export with description
@desc export Calculate the premium based on risk factors
GIVEN risk IS A RiskProfile
GIVETH A Number
calculatePremium risk MEANS ...

-- Default export (primary function of the module)
@desc default export Main entry point for policy evaluation
GIVEN policy IS A Policy, claim IS A Claim
GIVETH A ClaimResult
evaluateClaim policy claim MEANS ...

-- Regular description (not exported)
@desc Helper function for internal use
GIVEN x IS A Number
helper x MEANS x * 2

-- Export without description text
@desc export
GIVEN x IS A Number
GIVETH A Number
publicFunction x MEANS ...
```

### Parameter Descriptions

Parameter descriptions use inline `@desc` annotations on GIVEN clauses:

```l4
@desc export Check if person qualifies for discount
GIVEN
  age IS A Number @desc Customer age in years
  memberSince IS A Date @desc Date of membership start
  totalPurchases IS A Number @desc Total purchase amount in dollars
GIVETH A Boolean
qualifiesForDiscount age memberSince totalPurchases MEANS ...
```

### Parsing Strategy

Rather than modifying the core lexer/parser significantly, we use a **convention-based string matching** approach:

1. The `@desc` annotation text is already captured as `Text`
2. At the point of attachment, check if the text starts with `default`, `export`, or `default export`
3. Extract the flags and remaining description text

```haskell
data DescFlags = DescFlags
  { isDefault :: Bool
  , isExport :: Bool
  }
  deriving (Show, Eq)

data ParsedDesc = ParsedDesc
  { flags :: DescFlags
  , description :: Text
  }

-- | Parse @desc text to extract export flags
parseDescText :: Text -> ParsedDesc
parseDescText txt =
  let stripped = Text.strip txt
      (isDefault, rest1) = stripPrefix "default" stripped
      (isExport, rest2) = stripPrefix "export" rest1
      -- If "default" is present, imply "export"
      actualExport = isExport || isDefault
  in ParsedDesc
       { flags = DescFlags { isDefault, isExport = actualExport }
       , description = Text.strip rest2
       }
  where
    stripPrefix prefix t
      | prefix `Text.isPrefixOf` Text.toLower t =
          (True, Text.strip $ Text.drop (Text.length prefix) t)
      | otherwise = (False, t)
```

## Implementation

### Phase 1: Attach `@desc` to AST Nodes

**Problem**: Currently `@desc` annotations are collected in `PState.descs` but never attached to AST nodes.

**Solution**: Create `addDescCommentsToAst` similar to the existing `addNlgCommentsToAst`.

#### File: `jl4-core/src/L4/Parser/ResolveAnnotation.hs`

Add a new function to attach `@desc` annotations:

```haskell
-- | Attach @desc annotations to the AST nodes that immediately follow them.
-- Unlike NLG annotations which can attach to any named entity, @desc attaches
-- to the immediately following top-level declaration.
addDescCommentsToAst :: HasDesc a => [Desc] -> a -> (a, DescS)
addDescCommentsToAst descs ast = ...
```

The attachment rule is simpler than NLG:

- Each `@desc` attaches to the **first declaration that starts after it**
- Based on source positions (the `@desc` line should be immediately before the declaration)

#### File: `jl4-core/src/L4/Parser.hs`

Modify `execParserForTokens` to also process `descs`:

```haskell
execParserForTokens p file input ts =
  case runJl4Parser env st p (showNormalizedUri file) stream of
    Left err -> Left (fmap (mkPError "parser") $ errorBundlePretty err)
    Right (a, pstate) ->
      let
        (annotatedA, nlgS) = Resolve.addNlgCommentsToAst pstate.nlgs a
        -- NEW: Also attach @desc annotations
        (finalA, descS) = Resolve.addDescCommentsToAst pstate.descs annotatedA
      in
        Right (finalA, nlgS.warnings ++ descS.warnings, pstate)
```

### Phase 2: Extract Export Information

#### File: `jl4-core/src/L4/Export.hs` (NEW)

```haskell
module L4.Export
  ( ExportedFunction(..)
  , ExportedParam(..)
  , getExportedFunctions
  , getDefaultFunction
  ) where

import L4.Syntax
import L4.Annotation

-- | Information about an exported function
data ExportedFunction = ExportedFunction
  { exportName :: Text
  , exportDescription :: Text
  , exportIsDefault :: Bool
  , exportParams :: [ExportedParam]
  , exportReturnType :: Maybe (Type' Resolved)
  , exportDecide :: Decide Resolved
  }
  deriving (Show, Eq)

-- | Information about a function parameter
data ExportedParam = ExportedParam
  { paramName :: Text
  , paramType :: Type' Resolved
  , paramDescription :: Text
  , paramRequired :: Bool
  }
  deriving (Show, Eq)

-- | Extract all exported functions from a module
getExportedFunctions :: Module Resolved -> [ExportedFunction]
getExportedFunctions (MkModule _ _ section) =
  mapMaybe extractExport (sectionDecls section)
  where
    extractExport :: TopDecl Resolved -> Maybe ExportedFunction
    extractExport (Decide _ decide) = do
      desc <- getAnno decide ^. annDesc
      let parsed = parseDescText (getDesc desc)
      guard (parsed.flags.isExport)
      pure $ buildExportedFunction decide parsed
    extractExport (Section _ s) =
      -- Recursively check nested sections
      listToMaybe $ mapMaybe extractExport (sectionDecls s)
    extractExport _ = Nothing

-- | Get the default function if one exists
getDefaultFunction :: Module Resolved -> Maybe ExportedFunction
getDefaultFunction = find (.exportIsDefault) . getExportedFunctions

-- | Build ExportedFunction from Decide and parsed @desc
buildExportedFunction :: Decide Resolved -> ParsedDesc -> ExportedFunction
buildExportedFunction decide@(MkDecide _ typeSig appForm _) parsed =
  ExportedFunction
    { exportName = prettyLayout (appFormName appForm)
    , exportDescription = parsed.description
    , exportIsDefault = parsed.flags.isDefault
    , exportParams = extractParams typeSig
    , exportReturnType = getReturnType typeSig
    , exportDecide = decide
    }

extractParams :: TypeSig Resolved -> [ExportedParam]
extractParams (MkTypeSig _ (MkGivenSig _ params) _) =
  map toExportedParam params
  where
    toExportedParam (MkOptionallyTypedName anno resolved mType) =
      ExportedParam
        { paramName = prettyLayout resolved
        , paramType = fromMaybe inferredType mType
        , paramDescription = maybe "" getDesc (anno ^. annDesc)
        , paramRequired = True  -- Could extend with OPTIONAL keyword later
        }
```

### Phase 3: Decision Service Integration

#### File: `jl4-decision-service/src/Examples.hs`

Modify `loadL4File` to work with or without YAML:

```haskell
loadL4File :: FilePath -> IO (Maybe (FilePath, Text, Text, Function))
loadL4File path = do
  content <- TIO.readFile path

  -- First, try to find exports from L4 annotations
  case tryLoadFromAnnotations path content of
    Just result -> return (Just result)
    Nothing -> do
      -- Fall back to YAML file
      let yamlPath = replaceExtension path ".yaml"
      yamlExists <- doesFileExist yamlPath
      if not yamlExists
        then return Nothing
        else loadFromYaml path yamlPath content

-- | Try to load function metadata from @desc export annotations
tryLoadFromAnnotations :: FilePath -> Text -> Maybe (FilePath, Text, Text, Function)
tryLoadFromAnnotations path content = do
  -- Parse and typecheck to get resolved module
  module' <- parseAndTypecheck path content
  -- Get exported functions
  case getExportedFunctions module' of
    [] -> Nothing  -- No exports, need YAML
    (export:_) -> Just  -- Use first export (or default if present)
      ( path
      , export.exportName
      , content
      , exportToFunction export
      )

-- | Convert ExportedFunction to the Function type used by Server
exportToFunction :: ExportedFunction -> Function
exportToFunction export = Function
  { name = export.exportName
  , description = export.exportDescription
  , parameters = MkParameters
      { parameterMap = Map.fromList
          [(p.paramName, paramToParameter p) | p <- export.exportParams]
      , required = [p.paramName | p <- export.exportParams, p.paramRequired]
      }
  , supportedEvalBackend = [JL4]
  }

paramToParameter :: ExportedParam -> Parameter
paramToParameter p = Parameter
  { parameterType = typeToJsonType p.paramType
  , parameterAlias = Nothing
  , parameterEnum = []
  , parameterDescription = p.paramDescription
  }

-- | Map L4 types to JSON schema types
typeToJsonType :: Type' Resolved -> Text
typeToJsonType ty = case ty of
  TyApp _ name []
    | isNumberType name -> "number"
    | isBoolType name -> "boolean"
    | isStringType name -> "string"
  TyApp _ name [inner]
    | isListType name -> "array"
    | isMaybeType name -> typeToJsonType inner
  _ -> "object"
```

### Phase 4: Multiple Exports per File

A single L4 file can export multiple functions:

```l4
@desc default export Primary policy checker
GIVEN policy IS A Policy
GIVETH A Result
checkPolicy policy MEANS ...

@desc export Secondary validation function
GIVEN data IS A InputData
GIVETH A Boolean
validateInput data MEANS ...

-- Not exported, internal helper
GIVEN x IS A Number
helper x MEANS x * 2
```

The decision service would expose:

- `GET /functions` → returns `[checkPolicy, validateInput]`
- `POST /functions/checkPolicy/evaluation` → evaluates checkPolicy
- `POST /functions/validateInput/evaluation` → evaluates validateInput

When calling the module by UUID without specifying a function, the `default` function is used.

## Migration Path

### Backward Compatibility

- **YAML still works**: If a `.yaml` file exists, it takes precedence
- **Gradual migration**: Convert files one at a time by adding `@desc export` and removing `.yaml`
- **Mixed mode**: Some functions can use YAML, others can use annotations

### Migration Steps

1. For each `.yaml` file:
   - Add `@desc export <description>` above the main function in the `.l4` file
   - Add `@desc` annotations to GIVEN parameters
   - Test that the function still works
   - Delete the `.yaml` file

### Example Migration

**Before** (`premium.yaml`):

```yaml
name: calculatePremium
description: Calculate insurance premium based on risk
parameters:
  type: object
  properties:
    age:
      type: number
      description: Applicant age
    riskScore:
      type: number
      description: Risk assessment score
  required: [age, riskScore]
supportedEvalBackend: [jl4]
```

**Before** (`premium.l4`):

```l4
GIVEN age IS A Number, riskScore IS A Number
GIVETH A Number
calculatePremium age riskScore MEANS
  IF riskScore > 0.7 THEN age * 100
  ELSE age * 50
```

**After** (`premium.l4` only):

```l4
@desc export Calculate insurance premium based on risk
GIVEN
  age IS A Number @desc Applicant age
  riskScore IS A Number @desc Risk assessment score
GIVETH A Number
calculatePremium age riskScore MEANS
  IF riskScore > 0.7 THEN age * 100
  ELSE age * 50
```

## Testing Plan

### Unit Tests

1. **Parsing tests**: Verify `parseDescText` correctly extracts flags

   ```haskell
   parseDescText "export Do something"
     == ParsedDesc (DescFlags False True) "Do something"
   parseDescText "default export Main function"
     == ParsedDesc (DescFlags True True) "Main function"
   parseDescText "Just a description"
     == ParsedDesc (DescFlags False False) "Just a description"
   ```

2. **Attachment tests**: Verify `@desc` attaches to correct declarations

3. **Export extraction tests**: Verify `getExportedFunctions` finds correct functions

### Integration Tests

1. **Decision service without YAML**: Load L4 file with `@desc export`, verify function appears in API
2. **Multiple exports**: Verify all exported functions are available
3. **Default function**: Verify default function is used when calling by UUID
4. **Parameter descriptions**: Verify parameter descriptions appear in API schema

### Golden Tests

Add test files:

- `jl4/examples/ok/export-single.l4` - Single export
- `jl4/examples/ok/export-multiple.l4` - Multiple exports
- `jl4/examples/ok/export-default.l4` - Default export
- `jl4/examples/ok/export-with-params.l4` - Parameter descriptions

## Future Extensions

### Optional Parameters

```l4
@desc export
GIVEN
  required IS A Number @desc Required param
  optional IS A MAYBE Number @desc Optional param (can be null)
GIVETH A Number
myFunction required optional MEANS ...
```

### Enum Parameters

```l4
DECLARE Status IS ONE OF Active, Inactive, Pending

@desc export
GIVEN status IS A Status @desc The current status
GIVETH A Boolean
isActive status MEANS status EQUALS Active
```

The export extractor would detect `Status` is an enum and populate `parameterEnum` in the JSON schema.

### Visibility Modifiers

Future syntax could include:

```l4
@desc private  -- Explicitly not exported (default)
@desc public   -- Alias for export
@desc internal -- Exported but not in public API docs
```

## References

- Issue #635: Critical L4 Decision Service Improvements (Item 6)
- PRs #626, #643: Initial `@desc` annotation implementation
- `DECISION-SERVICE-JSONDECODE-SPEC.md`: Related decision service improvements
- `jl4-core/src/L4/Parser/ResolveAnnotation.hs`: NLG attachment pattern to follow
