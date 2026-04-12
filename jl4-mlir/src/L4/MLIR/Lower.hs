-- | L4 AST → MLIR IR lowering.
--
-- Traverses a typechecked L4 module and produces MLIR operations using
-- the func, arith, scf, and cf dialects. Each L4 DECIDE becomes a
-- func.func, each expression becomes a sequence of SSA operations.
--
-- The lowering runs in a monadic context ('LowerM') that tracks:
--   * Fresh SSA value / block counters
--   * Variable bindings (L4 name → MLIR Value)
--   * Type environment (record/enum layouts)
--   * Accumulated operations (current block)
module L4.MLIR.Lower
  ( lowerModule
  , lowerProgram
  , lowerProgramWithInfo
  , dedupAndSynthExterns
  ) where

import L4.MLIR.IR
import L4.MLIR.Types
import L4.MLIR.Dialect.Func
import L4.MLIR.Dialect.Arith
import L4.MLIR.Dialect.SCF
import L4.MLIR.Dialect.LLVM
import qualified L4.MLIR.Schema as Schema
import L4.MLIR.ABI (l4Value)

import L4.Syntax
  ( Module(..), Section(..), TopDecl(..), Decide(..), Declare(..)
  , Assume(..)
  , Expr(..), Lit(..), Pattern(..), Branch(..), BranchLhs(..)
  , TypeSig(..), GivenSig(..), GivethSig(..), AppForm(..)
  , TypeDecl(..), ConDecl(..), TypedName(..), OptionallyTypedName(..)
  , Type'(..), Resolved(..), RawName(..), Name(..)
  , LocalDecl(..), GuardedExpr(..)
  , NamedExpr(..), InertContext(..), OptionallyNamedType(..)
  , Info(..)
  , rawName, rawNameToText, getActual
  )
import L4.Annotation (HasSrcRange, rangeOf)
import L4.Parser.SrcSpan (SrcRange(..))
import L4.TypeCheck.Types (InfoMap)
import qualified L4.Utils.IntervalMap as IV

import Control.Monad (forM_, forM, unless)
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- ---------------------------------------------------------------------------
-- Lowering monad
-- ---------------------------------------------------------------------------

data LowerState = LowerState
  { nextValue  :: !ValueId
  , nextBlock  :: !BlockId
  , bindings   :: Map Text Value    -- L4 name → SSA value
  , bindingTypes :: Map Text MLIRType  -- L4 name → MLIR type
  , listElemTypes :: Map Text MLIRType -- L4 name → list element type (for LIST OF T)
  , typeEnv    :: TypeEnv           -- Record/enum layouts
  , currentOps :: [Operation]       -- Ops accumulated in current block (reversed)
  , globals    :: [Operation]       -- Top-level global declarations
  , functions  :: [Operation]       -- Collected func.func operations
  , stringPool :: Map Text Text     -- String literal → global name
  , nextString :: !Int              -- Counter for string global names
  , funcSigs   :: Map Text ([MLIRType], MLIRType)  -- Known function signatures
  , funcListElems :: Map Text [Maybe MLIRType]  -- Known list element types per arg
    -- | Ground-truth type info from the jl4-core typechecker. Every
    -- expression's inferred 'Type'' Resolved' is stored here, keyed by
    -- source range. Use 'typeOfExpr' to look up the type of any
    -- expression rather than re-inferring from the syntactic shape.
  , sourceTypeMap :: !InfoMap
    -- | Helpers introduced by @WHERE@ / @LET IN@ clauses that were
    -- lambda-lifted to top-level @func.func@s. For each lifted helper
    -- this stores the list of outer-scope variables it captured, in
    -- the order the captures were prepended as extra leading
    -- parameters. Call sites must prepend the corresponding current
    -- bindings before emitting the call.
  , localCaptures :: Map Text [Text]
  }

type LowerM a = State LowerState a

initState :: InfoMap -> LowerState
initState info = LowerState
  { nextValue = 0
  , nextBlock = 0
  , bindings = Map.empty
  , bindingTypes = Map.empty
  , listElemTypes = Map.empty
  , typeEnv = emptyTypeEnv
  , currentOps = []
  , globals = []
  , functions = []
  , stringPool = Map.empty
  , nextString = 0
  , funcSigs = Map.empty
  , funcListElems = Map.empty
  , sourceTypeMap = info
  , localCaptures = Map.empty
  }

-- | Look up the ground-truth type for an expression using the typechecker's
-- InfoMap.
--
-- Queries by the expression's full source range. Multiple intervals can
-- share a start position (a @Proj@ and its subject, a binary op and its
-- left operand, etc.), so 'IV.smallestContaining' alone would pick the
-- leftmost sub-term instead of *this* expression. We prefer an interval
-- whose range matches exactly our expression's range.
typeOfExpr :: HasSrcRange e => e -> LowerM (Maybe (Type' Resolved))
typeOfExpr e = do
  info <- gets (.sourceTypeMap)
  case rangeOf e of
    Nothing -> pure Nothing
    Just range -> do
      -- Only accept an *exact* range match. The typechecker records
      -- TypeInfo at each node's own range; approximate matches tend to
      -- pick a containing expression with a different type.
      let exactMatches =
            [ ty
            | (iv, TypeInfo ty _) <- IV.search range.start info
            , IV.intervalLow iv == range.start
            , IV.intervalHigh iv == range.end
            ]
      pure $ case exactMatches of
        (ty : _) -> Just ty
        []       -> Nothing


-- ---------------------------------------------------------------------------
-- ABI boxing / unboxing (see "L4.MLIR.ABI")
-- ---------------------------------------------------------------------------

-- | Under the uniform f64 ABI, every SSA value is already f64, so
-- boxing at the call-site / function-boundary is a no-op. These
-- helpers are retained to keep the call-convention call sites
-- self-documenting — the compiler knows where the conceptual
-- boundary is even though no real coercion needs to happen.
boxABI :: MLIRType -> Value -> LowerM Value
boxABI _ src = pure src

unboxABI :: MLIRType -> Value -> LowerM Value
unboxABI _ src = pure src

-- | Call another L4 function (local or extern) through the uniform f64
-- ABI. Boxes each native-typed argument to 'l4Value', emits the call
-- with an all-@f64@ signature, then unboxes the f64 return to the
-- caller's expected native type.
--
-- This is the /only/ way the lowering should invoke an L4 function.
-- Calls to runtime support routines ('__l4_*' helpers) that aren't
-- part of the L4 ABI can still use 'funcCall' directly.
callL4 :: Text -> [(Value, MLIRType)] -> MLIRType -> LowerM Value
callL4 callee args retTy = do
  -- Box each arg at the call site.
  boxed <- forM args $ \(v, t) -> boxABI t v
  -- Emit the call with uniform f64 signature.
  let nArgs = length args
      sigArgs = replicate nArgs l4Value
  boxedResult <- emitVal $ \vid ->
    funcCall [vid] callee boxed sigArgs [l4Value]
  -- Unbox the return to the expected native type.
  unboxABI retTy boxedResult

-- | Allocate a fresh SSA value.
fresh :: LowerM ValueId
fresh = do
  s <- get
  let vid = s.nextValue
  put s { nextValue = vid + 1 }
  pure vid

-- | Allocate a fresh block ID.
freshBlockId :: LowerM BlockId
freshBlockId = do
  s <- get
  let bid = s.nextBlock
  put s { nextBlock = bid + 1 }
  pure bid

-- | Emit an operation into the current block.
emit :: Operation -> LowerM ()
emit op = modify' $ \s -> s { currentOps = op : s.currentOps }

-- | Emit an operation and return its single result value.
emitVal :: (ValueId -> Operation) -> LowerM Value
emitVal mkOp_ = do
  vid <- fresh
  emit (mkOp_ vid)
  pure (SSAValue vid)

-- | Bind an L4 name to an SSA value.
-- Note: 'bind' (type-erased) is intentionally no longer exported. Use
-- 'bindTy' to preserve the variable's MLIR type in 'bindingTypes'.
_bind :: Text -> Value -> LowerM ()
_bind name val = modify' $ \s -> s { bindings = Map.insert name val s.bindings }

-- | Bind an L4 name with type info.
bindTy :: Text -> Value -> MLIRType -> LowerM ()
bindTy name val ty = modify' $ \s -> s
  { bindings = Map.insert name val s.bindings
  , bindingTypes = Map.insert name ty s.bindingTypes
  }

-- | Bind a list element type for a variable.
bindListElem :: Text -> MLIRType -> LowerM ()
bindListElem name ty = modify' $ \s -> s { listElemTypes = Map.insert name ty s.listElemTypes }

-- | Look up a bound name.
lookupVar :: Text -> LowerM (Maybe Value)
lookupVar name = gets $ Map.lookup name . (.bindings)

-- | Run a computation with local bindings (scoped).
withScope :: LowerM a -> LowerM a
withScope action = do
  savedBindings <- gets (.bindings)
  savedTypes <- gets (.bindingTypes)
  savedListTys <- gets (.listElemTypes)
  result <- action
  modify' $ \s -> s
    { bindings = savedBindings
    , bindingTypes = savedTypes
    , listElemTypes = savedListTys
    }
  pure result

-- | Collect operations emitted during an action.
collectOps :: LowerM a -> LowerM (a, [Operation])
collectOps action = do
  saved <- gets (.currentOps)
  modify' $ \s -> s { currentOps = [] }
  result <- action
  ops <- gets (reverse . (.currentOps))
  modify' $ \s -> s { currentOps = saved }
  pure (result, ops)

-- | Register a string literal and return its global name.
internString :: Text -> LowerM Text
internString str = do
  pool <- gets (.stringPool)
  case Map.lookup str pool of
    Just name -> pure name
    Nothing -> do
      idx <- gets (.nextString)
      let name = "__str_" <> Text.pack (show idx)
      modify' $ \s -> s
        { stringPool = Map.insert str name s.stringPool
        , nextString = idx + 1
        , globals = llvmGlobalString name str : s.globals
        }
      pure name

-- ---------------------------------------------------------------------------
-- Module lowering
-- ---------------------------------------------------------------------------

-- | Lower an L4 program (module + dependencies) to MLIR.
--
-- For each dependency module we only register *types* and *function
-- signatures* (as extern declarations) — not the function bodies.
-- That gives the main module everything it needs to type-check calls
-- into imported functions while keeping each symbol defined exactly
-- once and avoiding the wasm-ld @redefinition of symbol@ error.
--
-- Before emitting any extern declarations, we collect the set of names
-- the main module will define locally, so that a dep re-declaration of
-- the same name (common in files that shadow prelude helpers) doesn't
-- produce a duplicate symbol.
lowerProgram :: Module Resolved -> [Module Resolved] -> MLIRModule
lowerProgram = lowerProgramWithInfo IV.empty

-- | Like 'lowerProgram' but threads the typechecker's 'InfoMap' through
-- so the lowering can query ground-truth types for every expression.
-- This is the preferred entry point — callers that already have a
-- 'TypeCheckResult' should pass its @infoMap@ field here.
lowerProgramWithInfo :: InfoMap -> Module Resolved -> [Module Resolved] -> MLIRModule
lowerProgramWithInfo info mainMod deps =
  let mainNames = collectLocalNames mainMod
      finalState = execState (do
        forM_ deps (registerDependencyModule mainNames)
        lowerModuleDecls mainMod
        ) (initState info)
      rawOps = reverse finalState.globals ++ reverse finalState.functions
  in MLIRModule { moduleOps = rawOps }

-- | Final pass run by 'L4.MLIR.Pipeline' once all operations — including
-- runtime builtins — have been assembled. It handles three consumers
-- of MLIR symbol names that don't naturally agree:
--
--   1. @func.func@ bodies — the lowering may emit several bodies with
--      the same L4 name because L4 supports ad-hoc overloading (the
--      @daydate@ library has 'Date', 'Time', 'Datetime' with 1, 2, 3
--      and 4 parameters). WASM/MLIR don't allow duplicate symbols.
--   2. @func.func@ extern declarations inherited from dep modules —
--      may report the wrong arity when the dep has overloaded names.
--   3. @func.call@ sites — the consumer — signatures must match exactly.
--
-- We resolve all of these by __arity-based name mangling__: every
-- @name@ that has more than one definition is rewritten as
-- @name__$arity@ everywhere — in definitions, externs, *and* in call
-- sites. After mangling there's exactly one definition per sanitized
-- symbol. Names used with only a single arity keep their original
-- name (zero-overhead path for the common case).
dedupAndSynthExterns :: [Operation] -> [Operation]
dedupAndSynthExterns ops =
  let isRuntimeBuiltin n = "__l4_" `Text.isPrefixOf` n || "__str_" `Text.isPrefixOf` n

      -- Collect every arity *any name* shows up with — whether from a
      -- @func.func@ declaration/body or a @func.call@ site. Call-site
      -- arities are authoritative (the caller's signature must match)
      -- so we include them when deciding whether to mangle.
      defArities =
        [ (funcSymName op, funcArity op)
        | op <- ops
        , op.opName == "func.func"
        , not (isRuntimeBuiltin (funcSymName op))
        , not (Text.null (funcSymName op))
        ]
      callSiteArities =
        [ (funcCalleeName op, length op.opOperands)
        | op <- allOps ops
        , op.opName == "func.call"
        , let callee = funcCalleeName op
        , not (Text.null callee)
        , not (isRuntimeBuiltin callee)
        ]
      allAritiesByName = Map.fromListWith Set.union
        [ (name, Set.singleton arity)
        | (name, arity) <- defArities <> callSiteArities
        ]

      -- A name needs mangling iff it's referenced at more than one
      -- distinct arity across its bodies, externs, and call sites.
      overloaded = Set.fromList
        [ name
        | (name, arities) <- Map.toList allAritiesByName
        , Set.size arities > 1
        ]

      -- Compute the mangled name for a call site or declaration. If
      -- the name isn't overloaded, return it unchanged.
      mangled :: Text -> Int -> Text
      mangled name arity
        | name `Set.member` overloaded = name <> "__" <> Text.pack (show arity)
        | otherwise = name

      -- Rewrite every func.func and func.call to use the mangled name.
      mangleOp :: Operation -> Operation
      mangleOp op
        | op.opName == "func.func" =
            let name = funcSymName op
                arity = funcArity op
            in op
              { opAttributes = map (mangleAttr "sym_name" (mangled name arity)) op.opAttributes
              , opRegions = map mangleRegion op.opRegions
              }
        | op.opName == "func.call" =
            let name = funcCalleeName op
                arity = length op.opOperands
            in op
              { opAttributes = map (mangleAttrSymRef "callee" (mangled name arity)) op.opAttributes
              }
        | otherwise = op { opRegions = map mangleRegion op.opRegions }

      mangleAttr key newValue na
        | na.attrName == key = na { attrValue = StringAttr newValue }
        | otherwise = na

      mangleAttrSymRef key newValue na
        | na.attrName == key = na { attrValue = FlatSymbolRefAttr newValue }
        | otherwise = na

      mangleRegion r = r { regionBlocks = map mangleBlock r.regionBlocks }
      mangleBlock b  = b { blockOps = map mangleOp b.blockOps }

      mangledOps = map mangleOp ops

      -- Now every symbol has a unique name. Collect defined bodies and
      -- generate uniform-ABI externs for any called name that still
      -- lacks a definition. Runtime builtins pass through verbatim.
      callArities = Map.fromList
        [ (callee, length op.opOperands)
        | op <- allOps mangledOps
        , op.opName == "func.call"
        , let callee = funcCalleeName op
        , not (Text.null callee)
        ]
      definedBodies = Set.fromList
        [ funcSymName op
        | op <- mangledOps
        , op.opName == "func.func"
        , not (null op.opRegions)
        ]

      keepBody op         = op.opName == "func.func" && not (null op.opRegions)
      keepRuntimeExtern op = op.opName == "func.func"
        && null op.opRegions && isRuntimeBuiltin (funcSymName op)
      keepNonFunc op      = op.opName /= "func.func"

      bodies         = filter keepBody mangledOps
      runtimeExterns = filter keepRuntimeExtern mangledOps
      others         = filter keepNonFunc mangledOps

      synthExterns =
        [ mkExternFunc name (replicate arity l4Value) l4Value
        | (name, arity) <- Map.toList callArities
        , name `Set.notMember` definedBodies
        , not (isRuntimeBuiltin name)
        ]
  in others <> dedupByName (runtimeExterns <> synthExterns) <> dedupByName bodies

-- | Count the declared parameters of a @func.func@ operation by reading
-- its @function_type@ attribute.
funcArity :: Operation -> Int
funcArity op =
  case [ft | NamedAttribute k (TypeAttr ft) <- op.opAttributes, k == "function_type"] of
    (FunctionType args _ : _) -> length args
    _ -> 0

-- | The symbol referenced by a @func.call@'s @callee@ attribute.
funcCalleeName :: Operation -> Text
funcCalleeName op =
  case [s | NamedAttribute k (FlatSymbolRefAttr s) <- op.opAttributes, k == "callee"] of
    (s : _) -> s
    []      -> ""

-- | Keep only the first @func.func@ occurrence per @sym_name@.
dedupByName :: [Operation] -> [Operation]
dedupByName = go Set.empty
  where
    go _ [] = []
    go seen (op : rest)
      | op.opName == "func.func" =
          let name = funcSymName op
          in if Text.null name || name `Set.member` seen
               then go seen rest
               else op : go (Set.insert name seen) rest
      | otherwise = op : go seen rest

funcSymName :: Operation -> Text
funcSymName op =
  case [s | NamedAttribute k (StringAttr s) <- op.opAttributes, k == "sym_name"] of
    (s : _) -> s
    []      -> ""


-- | Recursively walk all operations (including those inside regions).
allOps :: [Operation] -> [Operation]
allOps = concatMap walk
  where
    walk op = op : concatMap (concatMap walk . (.blockOps)) (concatMap (.regionBlocks) op.opRegions)


-- | Collect the sanitized names of everything the module will define locally.
-- Covers DECIDEs, ASSUMEs, and local-function DECIDEs inside WHERE / LET.
collectLocalNames :: Module Resolved -> Set.Set Text
collectLocalNames (MkModule _ _ section) = goSection section
  where
    goSection (MkSection _ _ _ decls) = foldr (Set.union . goDecl) Set.empty decls
    goDecl = \case
      Decide _ (MkDecide _ _ appForm _) ->
        Set.singleton (sanitizeName (resolvedName (appFormHead' appForm)))
      Assume _ (MkAssume _ _ appForm _) ->
        Set.singleton (sanitizeName (resolvedName (appFormHead' appForm)))
      Section _ s -> goSection s
      _ -> Set.empty

-- | Walk a dependency module: register record/enum declarations and
-- function signatures, and emit @func.func private@ extern declarations
-- for every @DECIDE@ and @ASSUME@ that the main module does not itself
-- define. No function bodies are emitted.
registerDependencyModule :: Set.Set Text -> Module Resolved -> LowerM ()
registerDependencyModule skipLocal (MkModule _ _ section) = go section
  where
    go (MkSection _ _ _ decls) = forM_ decls processDep

    processDep :: TopDecl Resolved -> LowerM ()
    processDep = \case
      Declare _ d -> lowerDeclare d
      Decide _ d  -> registerExternDecide d
      Assume _ a  -> registerExternAssume a
      Section _ s -> go s
      _ -> pure ()

    shouldSkip :: Text -> LowerM Bool
    shouldSkip name = do
      -- Skip if the main module defines this name, OR if we've already
      -- registered this extern (deps can re-export each other's symbols).
      existing <- gets (Map.lookup name . (.funcSigs))
      pure (Set.member name skipLocal || existing /= Nothing)

    registerExternDecide :: Decide Resolved -> LowerM ()
    registerExternDecide (MkDecide _ typeSig appForm _body) = do
      env <- gets (.typeEnv)
      let name = sanitizeName (resolvedName (appFormHead' appForm))
          (argTypes, sigRetType) = sigToTypesEnv env typeSig
          retType = if hasGiveth typeSig then sigRetType else l4NumberType
          argListElems = listElementsFromSig env typeSig
          declOp = mkExternFunc name argTypes retType
      skip <- shouldSkip name
      unless skip $ modify' $ \s -> s
        { funcSigs = Map.insert name (argTypes, retType) s.funcSigs
        , funcListElems = Map.insert name argListElems s.funcListElems
        , globals = declOp : s.globals
        }

    registerExternAssume :: Assume Resolved -> LowerM ()
    registerExternAssume (MkAssume _ typeSig appForm _) = do
      env <- gets (.typeEnv)
      let name = sanitizeName (resolvedName (appFormHead' appForm))
          (_, retType) = sigToTypesEnv env typeSig
          declOp = mkExternFunc name [] retType
      skip <- shouldSkip name
      unless skip $ modify' $ \s -> s
        { funcSigs = Map.insert name ([], retType) s.funcSigs
        , globals = declOp : s.globals
        }

-- | Build a @func.func private@ external declaration (no body).
-- All argument and return types are the uniform 'l4Value' ABI type
-- (see "L4.MLIR.ABI"). Call sites are responsible for boxing their
-- arguments and unboxing the return value back to the expected native
-- type.
mkExternFunc :: Text -> [MLIRType] -> MLIRType -> Operation
mkExternFunc name argTypes _retType =
  let n = length argTypes
      uniformArgs = replicate n l4Value
  in mkExternFunc' name uniformArgs l4Value

mkExternFunc' :: Text -> [MLIRType] -> MLIRType -> Operation
mkExternFunc' name argTypes retType = Operation
  { opResults = []
  , opName = "func.func"
  , opOperands = []
  , opAttributes =
      [ NamedAttribute "sym_name" (StringAttr name)
      , NamedAttribute "function_type" (TypeAttr (FunctionType argTypes [retType]))
      , NamedAttribute "sym_visibility" (StringAttr "private")
      ]
  , opResultTypes = []
  , opOperandTypes = []
  , opRegions = []
  , opSuccessors = []
  }

-- | Lower a single L4 module.
lowerModule :: Module Resolved -> MLIRModule
lowerModule m = lowerProgram m []

-- | Process all top-level declarations in a module.
lowerModuleDecls :: Module Resolved -> LowerM ()
lowerModuleDecls (MkModule _ _ section) = lowerSection section

lowerSection :: Section Resolved -> LowerM ()
lowerSection (MkSection _ _ _ decls) = do
  -- Three-pass: (1) register types, (2) register function signatures, (3) lower bodies
  forM_ decls registerTypeDecl
  forM_ decls registerFuncSig
  forM_ decls lowerTopDecl

-- | Pre-pass: register type declarations so they're available during lowering.
registerTypeDecl :: TopDecl Resolved -> LowerM ()
registerTypeDecl (Declare _ decl) = lowerDeclare decl
registerTypeDecl (Section _ sect) = do
  let MkSection _ _ _ ds = sect
  forM_ ds registerTypeDecl
registerTypeDecl _ = pure ()

-- | Pre-pass: register function signatures so calls can look up arg types.
registerFuncSig :: TopDecl Resolved -> LowerM ()
registerFuncSig (Decide _ (MkDecide _ typeSig appForm body)) = do
  env <- gets (.typeEnv)
  let name = sanitizeName (resolvedName (appFormHead' appForm))
      (argTypes, sigRetType) = sigToTypesEnv env typeSig
      argListElems = listElementsFromSig env typeSig
  -- For MEANS bindings, infer type statefully (may reference other registered bindings)
  inferredRet <- inferExprTypeM body
  let retType = if hasGiveth typeSig then sigRetType else inferredRet
  modify' $ \s -> s
    { funcSigs = Map.insert name (argTypes, retType) s.funcSigs
    , funcListElems = Map.insert name argListElems s.funcListElems
    }
registerFuncSig (Section _ sect) = do
  let MkSection _ _ _ ds = sect
  forM_ ds registerFuncSig
registerFuncSig _ = pure ()

-- | Extract list element types from a function signature's parameters.
-- For each parameter, returns Just (element type) if parameter is LIST OF T, else Nothing.
listElementsFromSig :: TypeEnv -> TypeSig Resolved -> [Maybe MLIRType]
listElementsFromSig env (MkTypeSig _ (MkGivenSig _ params) _) =
  map paramListElem params
  where
    paramListElem (MkOptionallyTypedName _ _ (Just ty)) = listOfElem env ty
    paramListElem _ = Nothing

-- | If a type is LIST OF T, return Just (MLIR type of T).
listOfElem :: TypeEnv -> Type' Resolved -> Maybe MLIRType
listOfElem env (TyApp _ n [inner]) | rawNameToText (rawName (getActual n)) == "LIST" =
  Just (l4TypeToMLIR env inner)
listOfElem _ _ = Nothing

lowerTopDecl :: TopDecl Resolved -> LowerM ()
lowerTopDecl = \case
  Declare _ decl -> lowerDeclare decl
  Decide _ decl  -> lowerDecide decl
  Section _ sect -> lowerSection sect
  Assume _ assume -> lowerAssume assume
  Directive _ _  -> pure ()  -- Directives are compile-time only
  Import _ _     -> pure ()  -- Imports resolved by jl4-core
  Timezone _ _   -> pure ()  -- Timezone config, not compiled

-- | An @ASSUME@ declaration in L4 is an external parameter — a value
-- that must be supplied by the caller/host. We emit it as an MLIR
-- @func.func private@ declaration (no body) so that references to
-- this name in the compiled module resolve to a valid WASM import
-- that the runtime host can satisfy.
lowerAssume :: Assume Resolved -> LowerM ()
lowerAssume (MkAssume _ typeSig appForm mType) = do
  env <- gets (.typeEnv)
  let name = sanitizeName (resolvedName (appFormHead' appForm))
      -- An ASSUME may have arity, either via its GIVEN sig or its
      -- optional explicit type annotation (e.g. @ASSUME f IS A FUNCTION
      -- FROM Foo TO Bar@). Use whichever carries arity information.
      (sigArgs, sigRet) = sigToTypesEnv env typeSig
      (argTypes, retType) = case mType of
        -- Explicit @IS A FUNCTION FROM a AND b TO r@: use the function type
        Just (Fun _ fArgs fRet) ->
          ( [l4TypeToMLIR env (funArgTy a) | a <- fArgs]
          , l4TypeToMLIR env fRet )
        -- Explicit non-function type: zero-arg accessor of that type
        Just ty | null sigArgs -> ([], l4TypeToMLIR env ty)
        -- Otherwise fall back to the GIVEN signature (may still have args)
        _ -> (sigArgs, sigRet)
      declOp = mkExternFunc name argTypes retType
  modify' $ \s -> s
    { funcSigs = Map.insert name (argTypes, retType) s.funcSigs
    , globals  = declOp : s.globals
    }
  where
    funArgTy (MkOptionallyNamedType _ _ ty) = ty

-- ---------------------------------------------------------------------------
-- Type declarations
-- ---------------------------------------------------------------------------

lowerDeclare :: Declare Resolved -> LowerM ()
lowerDeclare (MkDeclare _ _ appForm typeDecl) = do
  env <- gets (.typeEnv)
  let name = resolvedName (appFormHead' appForm)
  case typeDecl of
    RecordDecl _ _ fields -> do
      let fieldInfos = [(fieldName f, l4TypeToMLIR env (fieldType f)) | f <- fields]
      modify' $ \s -> s { typeEnv = registerRecord name fieldInfos s.typeEnv }

    EnumDecl _ variants -> do
      let variantInfos = zipWith (\(MkConDecl _ n _) i -> (resolvedName n, i)) variants [0..]
      modify' $ \s -> s { typeEnv = registerEnum name variantInfos s.typeEnv }

    SynonymDecl _ _ -> pure ()  -- Type synonyms are erased
  where
    fieldName :: TypedName Resolved -> Text
    fieldName (MkTypedName _ n _ _) = resolvedName n

    fieldType :: TypedName Resolved -> Type' Resolved
    fieldType (MkTypedName _ _ ty _) = ty

-- ---------------------------------------------------------------------------
-- Function/decision lowering
-- ---------------------------------------------------------------------------

lowerDecide :: Decide Resolved -> LowerM ()
lowerDecide (MkDecide _ typeSig appForm body) = do
  env <- gets (.typeEnv)
  let funcName = sanitizeName (resolvedName (appFormHead' appForm))
      params   = appFormParams appForm
      (argTypes, sigRetType) = sigToTypesEnv env typeSig
      listElemTys = listElementsFromSig env typeSig

  -- Respect the signature we already registered in 'registerFuncSig'
  -- so call-site types match the definition — without that, a MEANS
  -- binding that returns a struct would be called as @() -> f64@ but
  -- return @StructType "Foo" …@, causing a type mismatch.
  registeredSigs <- gets (.funcSigs)
  inferredRet <- case Map.lookup funcName registeredSigs of
    Just (_, r) | not (hasGiveth typeSig) -> pure r
    _ -> inferExprTypeM body
  let retType = if hasGiveth typeSig then sigRetType else inferredRet

  -- ABI boundary: every function arg and return is the uniform
  -- 'l4Value' type. Boxing/unboxing at entry and exit is emitted
  -- inline so the function body can use native MLIR types freely.
  entryBlock <- freshBlockId

  -- Allocate one SSA id per param — this is the boxed (f64) arg
  -- presented at the block header. We then unbox it to the native
  -- type the body expects and bind the L4 name to the unboxed value.
  boxedArgIds <- forM params $ \_ -> fresh

  withScope $ do
    -- Collect the ops that unbox each boxed arg into its native form.
    (nativeArgs, unboxOps) <- collectOps $
      forM (zip3 params boxedArgIds argTypes) $ \(paramName, boxedId, paramTy) -> do
        let name = resolvedName paramName
            boxedVal = SSAValue boxedId
        nativeVal <- unboxABI paramTy boxedVal
        bindTy name nativeVal paramTy
        pure (name, nativeVal, paramTy)

    -- Bind list element types where known
    forM_ (zip nativeArgs listElemTys) $ \((name, _, _), mElemTy) -> do
      case mElemTy of
        Just elemTy -> bindListElem name elemTy
        Nothing -> pure ()

    -- Lower the body expression
    (resultVal, bodyOps) <- collectOps $ lowerExpr body retType

    -- Box the body result into the ABI return type.
    (boxedResult, boxOps) <- collectOps $ boxABI retType resultVal

    let retOp = funcReturn [boxedResult] [l4Value]
        block = Block
          { blockId = entryBlock
          , blockArgs = [(vid, l4Value) | vid <- boxedArgIds]
          , blockOps = unboxOps ++ bodyOps ++ boxOps ++ [retOp]
          }
        region = Region [block]
        funcOp = funcFunc funcName
          [(vid, l4Value) | vid <- boxedArgIds]
          [l4Value]
          region

    modify' $ \s -> s { functions = funcOp : s.functions }

-- | Extract argument types and return type from a type signature.
sigToTypesEnv :: TypeEnv -> TypeSig Resolved -> ([MLIRType], MLIRType)
sigToTypesEnv env (MkTypeSig _ givenSig mGiveth) =
  let argTypes = case givenSig of
        MkGivenSig _ params -> map paramToMLIR params
      retType = case mGiveth of
        Just (MkGivethSig _ ty) -> l4TypeToMLIR env ty
        Nothing -> l4BoolType  -- L4 DECIDE ... IF has implicit boolean return
  in (argTypes, retType)
  where
    paramToMLIR (MkOptionallyTypedName _ _ (Just ty)) = l4TypeToMLIR env ty
    paramToMLIR (MkOptionallyTypedName _ _ Nothing) = l4NumberType  -- default

hasGiveth :: TypeSig Resolved -> Bool
hasGiveth (MkTypeSig _ _ (Just _)) = True
hasGiveth _ = False

-- | Stateful type inference.
--
-- First consults the jl4-core typechecker's 'InfoMap' for the
-- ground-truth type of the expression. If the lookup returns a
-- concrete MLIR type, use it. If the InfoMap returns 'Nothing' (no
-- record for this node) or a non-informative type (like a generic
-- type variable or @NoneType@), fall back to the syntactic heuristic.
-- This gives us ground-truth precision where it exists without
-- regressing on nodes the typechecker didn't annotate.
inferExprTypeM :: Expr Resolved -> LowerM MLIRType
inferExprTypeM expr = do
  heuristicTy <- inferExprTypeHeuristic expr
  if isInformativeType heuristicTy
    then pure heuristicTy
    else do
      mTy <- typeOfExpr expr
      case mTy of
        Just ty -> do
          env <- gets (.typeEnv)
          pure (l4TypeToMLIR env ty)
        Nothing -> pure heuristicTy

-- | An MLIR type is informative if it carries real structure (e.g.
-- a concrete record, a float, a pointer to a list) rather than the
-- fallback defaults used when we have no idea.
isInformativeType :: MLIRType -> Bool
isInformativeType NoneType       = False
isInformativeType (NamedType _)  = False
isInformativeType _              = True

-- | Syntactic fallback when the typechecker didn't record a type for
-- this node. Kept as a backstop for synthesized expressions only.
inferExprTypeHeuristic :: Expr Resolved -> LowerM MLIRType
inferExprTypeHeuristic expr = do
  env <- gets (.typeEnv)
  case expr of
    App _ n [] -> do
      -- Variable ref or nullary call.
      -- First check for built-in nullary constructors that produce
      -- known types — the pure 'inferExprType' defaults @App _ _ _@ to
      -- f64 so without these cases a function like
      --     @DECIDE foo IS TRUE@
      -- would be inferred to return f64 while the body actually
      -- produces i1, breaking the ABI return-boxing.
      let name = resolvedName n
      case name of
        -- Uniform f64 ABI: every nullary form resolves to f64.
        "TRUE"    -> pure l4NumberType
        "FALSE"   -> pure l4NumberType
        "EMPTY"   -> pure l4NumberType
        "NOTHING" -> pure l4NumberType
        _ -> do
          mTy <- gets (Map.lookup name . (.bindingTypes))
          case mTy of
            Just ty -> pure ty
            Nothing -> do
              sigs <- gets (.funcSigs)
              case Map.lookup (sanitizeName name) sigs of
                Just (_, r) -> pure r
                Nothing -> case lookupEnumTag name env of
                  Just _ -> pure l4NumberType
                  Nothing -> pure (resolveInferredType env (inferExprType expr))
    App _ n args -> do
      let name = resolvedName n
      case lookupRecordFields name env of
        Just fs | length fs == length args ->
          pure (StructType name (map snd fs))
        -- Uniform f64 ABI: every expression resolves to f64.
        -- The only remaining dispatch here is for callee lookups in the
        -- registered-signature map, so we preserve cross-function arity info.
        _ -> do
          sigs <- gets (.funcSigs)
          case Map.lookup (sanitizeName name) sigs of
            Just (_, r) -> pure r
            Nothing     -> pure l4NumberType
    AppNamed _ n _ _ -> do
      -- Record construction
      let name = rawNameToText (rawName (getActual n))
      pure (resolveInferredType env (NamedType name))
    Cons _ _ _ -> pure l4NumberType
    List _ _ -> pure l4NumberType
    Proj _ _ field -> do
      -- Field access: find the field's type in the type env
      let fieldName = resolvedName field
      pure (findFieldType fieldName env)
    -- Recurse through structural wrappers whose type equals their payload's.
    -- 'inferExprType' is pure and can't re-enter the stateful path, so we
    -- dispatch these here directly — otherwise 'Where'/'LetIn'/etc. fall
    -- through to 'inferExprType' where 'App __AND__' isn't recognised and
    -- the whole expression is defaulted to f64.
    Where _ e _ -> inferExprTypeHeuristic e
    LetIn _ _ e -> inferExprTypeHeuristic e
    IfThenElse _ _ t _ -> inferExprTypeHeuristic t
    Consider _ _ branches -> case branches of
      (MkBranch _ _ e : _) -> inferExprTypeHeuristic e
      [] -> pure l4NumberType
    MultiWayIf _ _ e -> inferExprTypeHeuristic e
    Percent _ _ -> pure l4NumberType
    _ -> pure (resolveInferredType env (inferExprType expr))

-- | Find the MLIR type of a field (searches all records in env).
findFieldType :: Text -> TypeEnv -> MLIRType
findFieldType fieldName env =
  case [ ty
       | (_, fields) <- Map.toList env.records
       , (fname, ty) <- fields
       , fname == fieldName
       ] of
    (t : _) -> t
    [] -> l4NumberType

-- | Resolve NamedType references to concrete struct types using the type env.
resolveInferredType :: TypeEnv -> MLIRType -> MLIRType
resolveInferredType env (NamedType name) =
  case lookupRecordFields name env of
    Just fields -> StructType name (map snd fields)
    Nothing -> case lookupEnumVariants name env of
      Just _ -> IntegerType 32
      Nothing -> NamedType name
resolveInferredType _ ty = ty

-- | Under the uniform f64 ABI, every SSA value produced by 'lowerExpr'
-- is an @f64@ — so expression type inference almost always reduces to
-- "f64". The distinct constructor cases below are retained only for
-- sub-expression *identity* on compound constructors (e.g.
-- 'AppNamed' reports the record's named type, used by the schema
-- emitter for client documentation).
inferExprType :: Expr Resolved -> MLIRType
-- Everything is f64 under the uniform ABI. The 'AppNamed' case
-- retains its record name because 'L4.MLIR.Schema' reads it to emit
-- the JSON type for client consumption.
inferExprType (AppNamed _ n _ _) = NamedType (rawNameToText (rawName (getActual n)))
inferExprType _ = l4NumberType

-- ---------------------------------------------------------------------------
-- Expression lowering
-- ---------------------------------------------------------------------------

-- | Lower an L4 expression, producing an SSA value of the expected type.
lowerExpr :: Expr Resolved -> MLIRType -> LowerM Value
lowerExpr expr expectedTy = case expr of
  -- Literals
  Lit _ lit -> lowerLit lit expectedTy

  -- Variable reference
  App _ n [] -> do
    let name = resolvedName n
        funcN = sanitizeName name
    mVal <- lookupVar name
    case mVal of
      Just val -> pure val
      Nothing -> case name of
        -- Boolean literals (uniform f64 ABI: 0.0 / 1.0)
        "TRUE"  -> do
          i1 <- emitVal $ \vid -> arithConstantBool vid True
          boxBoolI1 i1
        "FALSE" -> do
          i1 <- emitVal $ \vid -> arithConstantBool vid False
          boxBoolI1 i1
        -- MAYBE NOTHING — a 2-slot record [tag=0.0 (false), value=0.0].
        "NOTHING" -> do
          mb <- allocSlots 2
          zero <- emitVal $ \vid -> arithConstantFloat vid 0.0
          storeSlot mb 0 zero
          storeSlot mb 1 zero
          pure mb
        -- Empty list: null pointer encoded as f64(0.0)
        "EMPTY" -> nullListF64
        _ -> do
          env <- gets (.typeEnv)
          case lookupEnumTag name env of
            -- Enum tag: a plain number under the uniform f64 ABI
            Just tag -> emitVal $ \vid -> arithConstantFloat vid (fromIntegral tag)
            Nothing -> do
              sigs <- gets (.funcSigs)
              let retTy = case Map.lookup funcN sigs of
                    Just (_, r) -> r
                    Nothing     -> expectedTy
              -- Nullary call through the uniform f64 ABI.
              callL4 funcN [] retTy

  -- Function application
  App _ n args -> do
    let rawName_ = resolvedName n
        funcN = sanitizeName rawName_
    env <- gets (.typeEnv)
    -- Closure conversion for @WHERE@ / @LET IN@ helpers: when the
    -- callee was lambda-lifted with captured outer-scope variables,
    -- prepend their current bindings to the call's args. See
    -- 'lowerLocalDecl' for where the capture list is recorded.
    captures <- gets (.localCaptures)
    case Map.lookup funcN captures of
      Just capturedNames -> do
        capturedVals <- forM capturedNames $ \cn -> do
          mV <- lookupVar cn
          case mV of
            Just v -> pure v
            Nothing -> emitVal $ \vid -> arithConstantFloat vid 0.0
        userArgPairs <- forM args $ \arg -> do
          ty <- inferExprTypeM arg
          v  <- lowerExpr arg ty
          pure (v, ty)
        let capturePairs = [(v, l4NumberType) | v <- capturedVals]
            retTy = l4NumberType
        callL4 funcN (capturePairs <> userArgPairs) retTy
      Nothing ->
       case lookupRecordFields rawName_ env of
        Just fields | length fields == length args ->
          lowerPositionalRecord rawName_ fields args
        _ ->
         case lookupEnumTag rawName_ env of
          -- Enum constructor tag — encoded as f64 for the uniform ABI.
          Just tag -> emitVal $ \vid -> arithConstantFloat vid (fromIntegral tag)
          Nothing ->
           -- Prelude intrinsics and runtime calls.
           case (rawName_, args) of
            ("__PLUS__", [a, b])    -> lowerBinop arithAddf a b l4NumberType
            ("__MINUS__", [a, b])   -> lowerBinop arithSubf a b l4NumberType
            ("__TIMES__", [a, b])   -> lowerBinop arithMulf a b l4NumberType
            ("__DIVIDE__", [a, b])  -> lowerBinop arithDivf a b l4NumberType
            ("__MODULO__", [a, b])  -> lowerBinop arithRemf a b l4NumberType
            ("__GEQ__", [a, b])     -> lowerCmp OGE a b
            ("__LEQ__", [a, b])     -> lowerCmp OLE a b
            ("__GT__", [a, b])      -> lowerCmp OGT a b
            ("__LT__", [a, b])      -> lowerCmp OLT a b
            ("__EQUALS__", [a, b])  -> lowerCmp OEQ a b
            ("__AND__", [a, b])     -> lowerBoolop arithAndi a b
            ("__OR__", [a, b])      -> lowerBoolop arithOri a b
            ("__NOT__", [a])        -> do
              innerF64 <- lowerExpr a l4NumberType
              innerI1 <- unboxBoolI1 innerF64
              trueBit <- emitVal $ \vid -> arithConstantBool vid True
              notI1 <- emitVal $ \vid -> arithXori vid innerI1 trueBit
              boxBoolI1 notI1
            ("__CONS__", [hd, tl])  -> do
              hdVal <- lowerExpr hd l4NumberType
              tlVal <- lowerExpr tl l4NumberType
              lowerListConsTy l4NumberType hdVal tlVal
            ("min", [a, b])   -> do
              aVal <- lowerExpr a l4NumberType
              bVal <- lowerExpr b l4NumberType
              emitVal $ \vid -> funcCall [vid] "__l4_min"
                [aVal, bVal] [l4NumberType, l4NumberType] [l4NumberType]
            ("max", [a, b])   -> do
              aVal <- lowerExpr a l4NumberType
              bVal <- lowerExpr b l4NumberType
              emitVal $ \vid -> funcCall [vid] "__l4_max"
                [aVal, bVal] [l4NumberType, l4NumberType] [l4NumberType]
            ("count", [a])    -> do
              aVal <- lowerExpr a l4NumberType
              emitVal $ \vid -> funcCall [vid] "__l4_list_count"
                [aVal] [l4NumberType] [l4NumberType]
            ("JUST", [inner]) -> do
              innerVal <- lowerExpr inner l4NumberType
              mb <- allocSlots 2
              one <- emitVal $ \vid -> arithConstantFloat vid 1.0
              storeSlot mb 0 one
              storeSlot mb 1 innerVal
              pure mb
            -- General function application: uniform f64 ABI via callL4.
            _ -> do
              sigs <- gets (.funcSigs)
              let retTy = case Map.lookup funcN sigs of
                    Just (_, r) -> r
                    Nothing     -> expectedTy
              argPairs <- forM args $ \arg -> do
                ty <- inferExprTypeM arg
                v  <- lowerExpr arg ty
                pure (v, ty)
              callL4 funcN argPairs retTy

  -- Named application (WITH syntax for record construction)
  AppNamed _ n namedArgs _ -> lowerRecordConstruction n namedArgs expectedTy

  -- Arithmetic
  Plus _ lhs rhs -> lowerBinop arithAddf lhs rhs l4NumberType
  Minus _ lhs rhs -> lowerBinop arithSubf lhs rhs l4NumberType
  Times _ lhs rhs -> lowerBinop arithMulf lhs rhs l4NumberType
  DividedBy _ lhs rhs -> lowerBinop arithDivf lhs rhs l4NumberType
  Modulo _ lhs rhs -> lowerBinop arithRemf lhs rhs l4NumberType

  -- Comparisons
  Equals _ lhs rhs -> lowerCmp OEQ lhs rhs
  Lt _ lhs rhs -> lowerCmp OLT lhs rhs
  Gt _ lhs rhs -> lowerCmp OGT lhs rhs
  Leq _ lhs rhs -> lowerCmp OLE lhs rhs
  Geq _ lhs rhs -> lowerCmp OGE lhs rhs

  -- Boolean logic
  And _ lhs rhs -> lowerBoolop arithAndi lhs rhs
  Or _ lhs rhs -> lowerBoolop arithOri lhs rhs
  Not _ inner -> do
    innerF64 <- lowerExpr inner l4NumberType
    innerI1 <- unboxBoolI1 innerF64
    trueBit <- emitVal $ \vid -> arithConstantBool vid True
    notI1 <- emitVal $ \vid -> arithXori vid innerI1 trueBit
    boxBoolI1 notI1

  Implies _ lhs rhs -> do
    -- A IMPLIES B  =  NOT A OR B
    lhsF64 <- lowerExpr lhs l4NumberType
    rhsF64 <- lowerExpr rhs l4NumberType
    lhsI1 <- unboxBoolI1 lhsF64
    rhsI1 <- unboxBoolI1 rhsF64
    trueBit <- emitVal $ \vid -> arithConstantBool vid True
    notLhs <- emitVal $ \vid -> arithXori vid lhsI1 trueBit
    resI1 <- emitVal $ \vid -> arithOri vid notLhs rhsI1
    boxBoolI1 resI1

  -- Conditionals
  IfThenElse _ cond then_ else_ -> do
    condF64 <- lowerExpr cond l4NumberType
    condI1 <- unboxBoolI1 condF64
    -- All SSA values in the scf.if region follow the uniform f64 ABI.
    (thenVal, thenOps) <- collectOps $ lowerExpr then_ l4NumberType
    (elseVal, elseOps) <- collectOps $ lowerExpr else_ l4NumberType
    let thenBlock = Block 0 [] (thenOps ++ [scfYield [thenVal] [l4NumberType]])
        elseBlock = Block 0 [] (elseOps ++ [scfYield [elseVal] [l4NumberType]])
        thenRegion = Region [thenBlock]
        elseRegion = Region [elseBlock]
    emitVal $ \vid -> scfIf [vid] condI1 thenRegion elseRegion [l4NumberType]

  -- Pattern matching (CONSIDER ... WHEN)
  Consider _ scrutinee branches -> lowerConsider scrutinee branches expectedTy

  -- Record field projection (foo's `field name`)
  Proj _ record field -> lowerProjection record field expectedTy

  -- WHERE clause — local helpers may be multi-arg functions, may be
  -- mutually recursive, and may capture outer-scope variables. They
  -- are lambda-lifted with explicit closure parameters.
  Where _ body locals -> withScope $ do
    forM_ locals (lowerLocalDecl LocalScopeWhere)
    lowerExpr body expectedTy

  -- LET ... IN ... — value bindings (single-use, typically). By L4
  -- convention these are inlined at their use sites rather than
  -- lifted to new top-level functions. Inlining gives the body
  -- transparent access to the caller's scope and avoids the cost of
  -- closure conversion where it isn't needed.
  LetIn _ locals body -> withScope $ do
    forM_ locals (lowerLocalDecl LocalScopeLetIn)
    lowerExpr body expectedTy

  -- Percent (x% = x / 100)
  Percent _ inner -> do
    val <- lowerExpr inner l4NumberType
    hundred <- emitVal $ \vid -> arithConstantFloat vid 100.0
    emitVal $ \vid -> arithDivf vid val hundred

  -- List literal
  List _ elems -> lowerListLiteral elems expectedTy

  -- List cons (FOLLOWED BY)
  Cons _ hd tl -> do
    hdVal <- lowerExpr hd l4NumberType
    tlVal <- lowerExpr tl l4NumberType
    lowerListConsTy l4NumberType hdVal tlVal

  -- Lambda
  Lam _ givenSig body -> lowerLambda givenSig body expectedTy

  -- Multi-way if (BRANCH ... WHEN guard THEN expr)
  MultiWayIf _ guards otherwise_ -> lowerMultiWayIf guards otherwise_ expectedTy

  -- String concatenation
  Concat _ parts -> lowerConcat parts

  -- String coercion
  AsString _ inner -> do
    -- Call runtime toString
    val <- lowerExpr inner l4NumberType
    emitVal $ \vid -> funcCall [vid] "__l4_to_string" [val] [l4NumberType] [l4NumberType]

  -- Inert text (grammatical scaffolding) — evaluates to identity for
  -- its context. Under the uniform f64 ABI we emit the boolean
  -- identity directly as f64 (0.0 or 1.0).
  Inert _ _ ctx -> case ctx of
    InertCtxAnd  -> emitVal $ \vid -> arithConstantFloat vid 1.0
    InertCtxOr   -> emitVal $ \vid -> arithConstantFloat vid 0.0
    InertCtxNone -> emitVal $ \vid -> arithConstantFloat vid 1.0

  -- Regulative / side-effectful / IO constructs aren't compiled — they
  -- return a uniform-ABI FALSE (0.0).
  Regulative{} -> emitVal $ \vid -> arithConstantFloat vid 0.0
  Event{}      -> emitVal $ \vid -> arithConstantFloat vid 0.0
  Fetch{}      -> emitVal $ \vid -> arithConstantFloat vid 0.0
  Post{}       -> emitVal $ \vid -> arithConstantFloat vid 0.0
  Env{}        -> emitVal $ \vid -> arithConstantFloat vid 0.0
  Breach{}     -> emitVal $ \vid -> arithConstantFloat vid 0.0

  -- Exponent
  Exponent _ base exp_ -> do
    baseVal <- lowerExpr base l4NumberType
    expVal <- lowerExpr exp_ l4NumberType
    emitVal $ \vid -> (funcCall [vid] "__l4_pow" [baseVal, expVal] [l4NumberType, l4NumberType] [l4NumberType])

  -- RAnd/ROr (regulative and/or — same as boolean for compiled code)
  RAnd _ lhs rhs -> lowerBoolop arithAndi lhs rhs
  ROr _ lhs rhs -> lowerBoolop arithOri lhs rhs

-- | Lower a literal.
lowerLit :: Lit -> MLIRType -> LowerM Value
lowerLit (NumericLit _ r) _ =
  emitVal $ \vid -> arithConstantFloat vid (fromRational r)
lowerLit (StringLit _ s) _ = do
  -- Intern the string, take its address (@!llvm.ptr@), then box to f64
  -- so it can live as a uniform-ABI SSA value.
  globalName <- internString s
  ptr <- emitVal $ \vid -> mkOp [vid] "llvm.mlir.addressof" []
    [NamedAttribute "global_name" (FlatSymbolRefAttr globalName)]
    [PointerType] []
  ptrToF64 ptr

-- | Lower a binary arithmetic operation.
lowerBinop :: (ValueId -> Value -> Value -> Operation) -> Expr Resolved -> Expr Resolved -> MLIRType -> LowerM Value
lowerBinop mkBinop lhs rhs ty = do
  lhsVal <- lowerExpr lhs ty
  rhsVal <- lowerExpr rhs ty
  emitVal $ \vid -> mkBinop vid lhsVal rhsVal

-- | Lower a comparison. Native @arith.cmpf@ produces @i1@; we
-- immediately @uitofp@ back to @f64@ so the returned SSA value
-- conforms to the uniform f64 ABI (0.0 = false, 1.0 = true).
lowerCmp :: CmpfPredicate -> Expr Resolved -> Expr Resolved -> LowerM Value
lowerCmp pred_ lhs rhs = do
  lhsVal <- lowerExpr lhs l4NumberType
  rhsVal <- lowerExpr rhs l4NumberType
  i1Val <- emitVal $ \vid -> arithCmpf vid pred_ lhsVal rhsVal
  boxBoolI1 i1Val

-- | Lower a boolean binary operation. Operands arrive as f64 (encoded
-- as 0.0 / 1.0). We unbox each to @i1@, run the native op, then box
-- the @i1@ result back to f64.
lowerBoolop :: (ValueId -> Value -> Value -> Operation) -> Expr Resolved -> Expr Resolved -> LowerM Value
lowerBoolop mkBoolop lhs rhs = do
  lhsF64 <- lowerExpr lhs l4NumberType
  rhsF64 <- lowerExpr rhs l4NumberType
  lhsI1 <- unboxBoolI1 lhsF64
  rhsI1 <- unboxBoolI1 rhsF64
  resultI1 <- emitVal $ \vid -> mkBoolop vid lhsI1 rhsI1
  boxBoolI1 resultI1

-- | Promote an @i1@ SSA value to @f64@ (0.0 / 1.0).
boxBoolI1 :: Value -> LowerM Value
boxBoolI1 i1Val =
  emitVal $ \vid -> mkOp [vid] "arith.uitofp" [i1Val] []
    [l4NumberType] [IntegerType 1]

-- | Demote an @f64@ (0.0 / nonzero) back to @i1@ for native boolean ops.
unboxBoolI1 :: Value -> LowerM Value
unboxBoolI1 f64Val = do
  zero <- emitVal $ \vid -> arithConstantFloat vid 0.0
  emitVal $ \vid -> arithCmpf vid ONE f64Val zero

-- | Lower CONSIDER/WHEN pattern matching.
lowerConsider :: Expr Resolved -> [Branch Resolved] -> MLIRType -> LowerM Value
lowerConsider scrutinee branches _expectedTy = do
  scrutVal <- lowerExpr scrutinee l4NumberType
  lowerBranches scrutVal branches

-- | Lower CONSIDER branches. The scrutinee is f64; all branch results
-- are f64 (uniform ABI), so the synthesised @scf.if@ yields f64.
lowerBranches :: Value -> [Branch Resolved] -> LowerM Value
lowerBranches _ [] = emitVal $ \vid -> arithConstantFloat vid 0.0
lowerBranches _scrutVal [MkBranch _ (Otherwise _) body] =
  lowerExpr body l4NumberType
lowerBranches scrutVal [MkBranch _ (When _ pat) body] = withScope $ do
  bindPatternTy scrutVal l4NumberType l4NumberType pat
  lowerExpr body l4NumberType
lowerBranches scrutVal (MkBranch _ branchLhs body : rest) = do
  case branchLhs of
    Otherwise _ -> lowerExpr body l4NumberType
    When _ pat -> do
      matchVal <- testPatternTy scrutVal l4NumberType pat
      (thenVal, thenOps) <- collectOps $ withScope $ do
        bindPatternTy scrutVal l4NumberType l4NumberType pat
        lowerExpr body l4NumberType
      (elseVal, elseOps) <- collectOps $ lowerBranches scrutVal rest
      let thenBlock = Block 0 [] (thenOps ++ [scfYield [thenVal] [l4NumberType]])
          elseBlock = Block 0 [] (elseOps ++ [scfYield [elseVal] [l4NumberType]])
      emitVal $ \vid -> scfIf [vid] matchVal (Region [thenBlock]) (Region [elseBlock]) [l4NumberType]

-- | Test a pattern against a scrutinee of known type.
-- ---------------------------------------------------------------------------
-- Pattern matching under the uniform f64 ABI
-- ---------------------------------------------------------------------------
--
-- All scrutinees are f64. Tests produce an i1 (for feeding scf.if).
-- Bindings bind f64 values extracted via 'loadSlot'.
-- Conventions by pattern shape:
--   * PatApp "NOTHING" []   — scrutinee is a MAYBE record (ptr→[tag,value]).
--       Match if tag == 0.0 (i.e. unboxBoolI1 tag == false).
--   * PatApp "JUST"    [p]  — match if tag == 1.0. Bind p to slot 1.
--   * PatApp "EMPTY"   []   — scrutinee is a list ptr. Match if ptr is null.
--   * PatCons _ hd tl       — scrutinee is non-null list ptr.
--                             Bind hd=slot 0, tl=slot 1.
--   * PatApp con []         — enum constructor: match if scrutinee == tag.
--   * PatApp con args       — enum-with-data constructor: match the tag.
--                             Args bind to slots starting at 1 (slot 0 = tag).
--   * PatLit (Numeric n)    — match if scrutinee == n.
--   * PatLit (String _)     — deferred (needs __l4_str_eq call).
--   * PatVar _              — always matches, binds the scrutinee as-is.

testPatternTy :: Value -> MLIRType -> Pattern Resolved -> LowerM Value
testPatternTy = go
  where
    go _ _ (PatVar _ _) = trueI1
    go scrutF64 _ (PatApp _ con []) = do
      let name = resolvedName con
      case name of
        "NOTHING" -> do
          tag <- loadSlot scrutF64 0
          tagI1 <- unboxBoolI1 tag
          trueI1v <- trueI1
          emitVal $ \vid -> arithXori vid tagI1 trueI1v
        "EMPTY" -> do
          zero <- emitVal $ \vid -> arithConstantFloat vid 0.0
          emitVal $ \vid -> arithCmpf vid OEQ scrutF64 zero
        _ -> do
          env <- gets (.typeEnv)
          case lookupEnumTag name env of
            Just tag -> enumTagCmpF64 scrutF64 tag
            Nothing  -> trueI1
    go scrutF64 _ (PatApp _ con [_]) = do
      let name = resolvedName con
      case name of
        "JUST" -> do
          tag <- loadSlot scrutF64 0
          unboxBoolI1 tag
        _ -> do
          env <- gets (.typeEnv)
          case lookupEnumTag name env of
            Just tag -> enumTagCmpF64 scrutF64 tag
            Nothing  -> trueI1
    go scrutF64 _ (PatApp _ con _) = do
      env <- gets (.typeEnv)
      case lookupEnumTag (resolvedName con) env of
        Just tag -> enumTagCmpF64 scrutF64 tag
        Nothing  -> trueI1
    go scrutF64 _ (PatLit _ (NumericLit _ r)) = do
      lit <- emitVal $ \vid -> arithConstantFloat vid (fromRational r)
      emitVal $ \vid -> arithCmpf vid OEQ scrutF64 lit
    go _ _ (PatLit _ (StringLit _ _)) = trueI1
    go scrutF64 _ (PatCons _ _ _) = do
      -- List is non-null: scrutinee != 0.0
      zero <- emitVal $ \vid -> arithConstantFloat vid 0.0
      emitVal $ \vid -> arithCmpf vid ONE scrutF64 zero
    go _ _ (PatExpr _ _) = trueI1


-- | Compare an f64-scrutinee against an enum tag (as f64).
enumTagCmpF64 :: Value -> Integer -> LowerM Value
enumTagCmpF64 scrutF64 tag = do
  tagVal <- emitVal $ \vid -> arithConstantFloat vid (fromIntegral tag)
  emitVal $ \vid -> arithCmpf vid OEQ scrutF64 tagVal

trueI1 :: LowerM Value
trueI1 = emitVal $ \vid -> arithConstantBool vid True

-- | Bind pattern variables under the uniform f64 ABI. The scrutinee and
-- all bound values are f64 — enum-tag / maybe / list destructuring is
-- implemented by 'loadSlot'.
bindPatternTy :: Value -> MLIRType -> MLIRType -> Pattern Resolved -> LowerM ()
bindPatternTy val _ _ (PatVar _ n) = bindTy (resolvedName n) val l4NumberType
bindPatternTy val _ _ (PatCons _ hd tl) = do
  hdVal <- loadSlot val 0
  tlVal <- loadSlot val 1
  bindPatternTy hdVal l4NumberType l4NumberType hd
  bindPatternTy tlVal l4NumberType l4NumberType tl
bindPatternTy val _ _ (PatApp _ con [innerPat]) = do
  let name = resolvedName con
  case name of
    "JUST" -> do
      innerVal <- loadSlot val 1
      bindPatternTy innerVal l4NumberType l4NumberType innerPat
    _ -> bindPatternTy val l4NumberType l4NumberType innerPat
bindPatternTy val _ _ (PatApp _ _ pats) =
  -- Enum-with-data constructor: slot 0 is the tag, bind args at 1..n.
  forM_ (zip [1..] pats) $ \(i, p) -> do
    v <- loadSlot val i
    bindPatternTy v l4NumberType l4NumberType p
bindPatternTy _ _ _ (PatLit _ _) = pure ()
bindPatternTy _ _ _ (PatExpr _ _) = pure ()


-- | Bind pattern variables into scope.

-- ---------------------------------------------------------------------------
-- Record / list / maybe storage under the uniform f64 ABI
-- ---------------------------------------------------------------------------
--
-- Compound L4 values (records, lists, MAYBE) live in linear memory as
-- arrays of f64 slots (8 bytes each, naturally aligned for @f64@
-- loads). Each field is a boxed f64 — a number stays as itself, a
-- boolean is 0.0/1.0, a string or nested record/list/maybe is a
-- pointer-bitcast-to-f64. The SSA-level value for any compound
-- is therefore just an f64 containing the pointer to its first slot.
--
-- This keeps the entire SSA value universe flat @f64@ while still
-- supporting compound L4 semantics.

-- | Number of linear-memory bytes needed for @n@ f64 slots.
slotBytes :: Int -> Int
slotBytes n = n * 8

-- | Allocate @n@ f64 slots in linear memory and return the head pointer
-- as an f64 (bitcast through i64).
allocSlots :: Int -> LowerM Value
allocSlots n = do
  size <- emitVal $ \vid -> arithConstantInt vid (fromIntegral (slotBytes n))
  ptr  <- emitVal $ \vid -> funcCall [vid] "__l4_alloc" [size]
                                [IntegerType 64] [PointerType]
  ptrToF64 ptr

-- | Write an f64 value into slot @i@ of a boxed pointer.
storeSlot :: Value -> Int -> Value -> LowerM ()
storeSlot boxedPtr idx v = do
  ptr <- f64ToPtr boxedPtr
  off <- emitVal $ \vid -> arithConstantInt vid (fromIntegral (slotBytes idx))
  slot <- emitVal $ \vid -> llvmGetelementptr vid ptr [off] (IntegerType 8)
  emit $ llvmStore v slot l4NumberType

-- | Read an f64 value from slot @i@ of a boxed pointer.
loadSlot :: Value -> Int -> LowerM Value
loadSlot boxedPtr idx = do
  ptr <- f64ToPtr boxedPtr
  off <- emitVal $ \vid -> arithConstantInt vid (fromIntegral (slotBytes idx))
  slot <- emitVal $ \vid -> llvmGetelementptr vid ptr [off] (IntegerType 8)
  emitVal $ \vid -> llvmLoad vid slot l4NumberType

-- | Bitcast an @!llvm.ptr@ to @f64@ (via i64).
ptrToF64 :: Value -> LowerM Value
ptrToF64 ptr = do
  intV <- emitVal $ \vid -> mkOp [vid] "llvm.ptrtoint" [ptr] []
    [IntegerType 64] [PointerType]
  emitVal $ \vid -> mkOp [vid] "arith.bitcast" [intV] []
    [l4NumberType] [IntegerType 64]

-- | Bitcast an f64 back to @!llvm.ptr@ (via i64).
f64ToPtr :: Value -> LowerM Value
f64ToPtr f = do
  intV <- emitVal $ \vid -> mkOp [vid] "arith.bitcast" [f] []
    [IntegerType 64] [l4NumberType]
  emitVal $ \vid -> mkOp [vid] "llvm.inttoptr" [intV] []
    [PointerType] [IntegerType 64]

-- | Lower positional record construction: @Person "Alice" 30@.
-- All field values are f64 (boxed under the uniform ABI), stored in
-- consecutive linear-memory slots.
lowerPositionalRecord :: Text -> [(Text, MLIRType)] -> [Expr Resolved] -> LowerM Value
lowerPositionalRecord _conName fields argExprs = do
  let n = length fields
  recPtr <- allocSlots n
  forM_ (zip [0..] argExprs) $ \(idx, argExpr) -> do
    argVal <- lowerExpr argExpr l4NumberType
    storeSlot recPtr idx argVal
  pure recPtr

-- | Lower @Record WITH field IS value, ...@ construction. Fields may
-- appear in any order; we dispatch to the declaration-order slot
-- using the registered record layout.
lowerRecordConstruction :: Resolved -> [NamedExpr Resolved] -> MLIRType -> LowerM Value
lowerRecordConstruction con namedArgs _expectedTy = do
  env <- gets (.typeEnv)
  let conName = resolvedName con
      fieldMap = case lookupRecordFields conName env of
        Just fs -> Map.fromList (zip (map fst fs) [0 :: Int ..])
        Nothing -> Map.empty
      n = case lookupRecordFields conName env of
        Just fs -> length fs
        Nothing -> length namedArgs
  recPtr <- allocSlots n
  forM_ namedArgs $ \(MkNamedExpr _ fieldN fieldExpr) -> do
    let fieldName = resolvedName fieldN
        idx = Map.findWithDefault 0 fieldName fieldMap
    fieldVal <- lowerExpr fieldExpr l4NumberType
    storeSlot recPtr idx fieldVal
  pure recPtr

-- | Lower a record field projection (@record's \`field name\`@).
-- Under the uniform f64 ABI: the record is an f64-boxed pointer; we
-- compute the slot index from the declared layout and load the f64
-- directly.
lowerProjection :: Expr Resolved -> Resolved -> MLIRType -> LowerM Value
lowerProjection record field _expectedTy = do
  env <- gets (.typeEnv)
  let fieldName = resolvedName field
      (_, fieldIdx) = findFieldInEnv fieldName env
  recordVal <- lowerExpr record l4NumberType
  loadSlot recordVal fieldIdx

-- | Find which record contains a field and return the struct type + index.
-- Falls back to an anonymous single-field struct if the field can't be
-- located (e.g., field defined in a not-yet-registered imported module).
findFieldInEnv :: Text -> TypeEnv -> (MLIRType, Int)
findFieldInEnv fieldName env =
  case [ (StructType rname (map snd fields), idx)
       | (rname, fields) <- Map.toList env.records
       , (idx, (fname, _)) <- zip [0..] fields
       , fname == fieldName
       ] of
    ((ty, idx) : _) -> (ty, idx)
    [] -> (StructType "l4.unknown" [l4NumberType], 0)

-- | Lower a list literal @[a, b, c]@.
-- Empty list is f64(0.0) which bitcasts to a null pointer; cons nodes
-- are 2-slot records [value, next] stored in linear memory.
lowerListLiteral :: [Expr Resolved] -> MLIRType -> LowerM Value
lowerListLiteral [] _ = nullListF64
lowerListLiteral (x : xs) _ = do
  hdVal <- lowerExpr x l4NumberType
  tlVal <- lowerListLiteral xs l4NumberType
  lowerListConsTy l4NumberType hdVal tlVal

-- | Construct a list cons node: a 2-slot record @[value, next]@.
-- The @elemTy@ parameter is kept for call-site API compatibility but
-- is always f64 under the uniform ABI.
lowerListConsTy :: MLIRType -> Value -> Value -> LowerM Value
lowerListConsTy _elemTy hd tl = do
  node <- allocSlots 2
  storeSlot node 0 hd
  storeSlot node 1 tl
  pure node

-- | The empty-list sentinel: f64(0.0), which bitcasts to a null pointer.
nullListF64 :: LowerM Value
nullListF64 = emitVal $ \vid -> arithConstantFloat vid 0.0


-- | Lower a local declaration (WHERE clause).
-- | L4 local scopes ('WHERE', 'LET IN') are lowered identically: a
-- zero-arg binding is evaluated inline; a multi-arg helper is
-- lambda-lifted to a top-level @func.func@ with its free variables
-- prepended as explicit parameters (closure conversion).
--
-- __Why not inline @LET IN@?__ Inlining looks appealing because 'LET'
-- typically binds a single-use value — but inlining substitutes the
-- /expression/, so if the bound expression allocates (e.g. constructs
-- a record), each use site gets a fresh allocation; pointer identity
-- is lost. It also blows up code size when a helper is called many
-- times, and loops at compile time for recursive bindings. Closure
-- conversion is the semantically-correct choice for both forms.
--
-- The 'LocalScope' tag is retained for documentation / future
-- scope-specific behaviour but both variants go through the same path
-- today.
data LocalScope = LocalScopeWhere | LocalScopeLetIn
  deriving (Eq, Show)

lowerLocalDecl :: LocalScope -> LocalDecl Resolved -> LowerM ()
lowerLocalDecl _scope (LocalDecide _anno (MkDecide dAnno ts appForm body)) = do
  let name = resolvedName (appFormHead' appForm)
      params = appFormParams appForm
  case params of
    [] -> do
      -- Zero-arg binding — evaluate once and bind the value.
      bodyTy <- inferExprTypeM body
      val <- lowerExpr body bodyTy
      bindTy name val bodyTy
    _ -> do
      let funcN = sanitizeName name
          paramSet = Set.fromList (map resolvedName params)
      knownTopLevels <- gets (Set.fromList . Map.keys . (.funcSigs))
      currentBindings <- gets (Map.keysSet . (.bindings))
      let freeInBody = freeVarsOfExpr body
            (paramSet <> knownTopLevels <> Set.singleton name)
          captured = [v | v <- Set.toList freeInBody, v `Set.member` currentBindings]
          extraParams = case givenSigParams ts of
            (template:_) -> [synthParam c template | c <- captured]
            []           -> []
          newAppForm = extendAppFormWith captured appForm
          newTs = extendTypeSigWith extraParams ts

      modify' $ \s -> s
        { localCaptures = Map.insert funcN captured s.localCaptures
        }
      lowerDecide (MkDecide dAnno newTs newAppForm body)
lowerLocalDecl _ (LocalAssume _ _) = pure ()

-- | Collect names referenced as values in an expression, minus a
-- known-bound set. Used by 'lowerLocalDecl' for free-variable analysis.
freeVarsOfExpr :: Expr Resolved -> Set.Set Text -> Set.Set Text
freeVarsOfExpr expr0 bound0 = go bound0 expr0
  where
    go bound = \case
      App _ n [] ->
        let nm = resolvedName n
        in if nm `Set.member` bound
             then Set.empty
             else Set.singleton nm
      App _ _ args -> Set.unions (map (go bound) args)
      AppNamed _ _ named _ ->
        Set.unions [go bound e | MkNamedExpr _ _ e <- named]
      And _ a b        -> Set.union (go bound a) (go bound b)
      Or _ a b         -> Set.union (go bound a) (go bound b)
      RAnd _ a b       -> Set.union (go bound a) (go bound b)
      ROr _ a b        -> Set.union (go bound a) (go bound b)
      Implies _ a b    -> Set.union (go bound a) (go bound b)
      Equals _ a b     -> Set.union (go bound a) (go bound b)
      Not _ a          -> go bound a
      Plus _ a b       -> Set.union (go bound a) (go bound b)
      Minus _ a b      -> Set.union (go bound a) (go bound b)
      Times _ a b      -> Set.union (go bound a) (go bound b)
      DividedBy _ a b  -> Set.union (go bound a) (go bound b)
      Modulo _ a b     -> Set.union (go bound a) (go bound b)
      Exponent _ a b   -> Set.union (go bound a) (go bound b)
      Cons _ a b       -> Set.union (go bound a) (go bound b)
      Leq _ a b        -> Set.union (go bound a) (go bound b)
      Geq _ a b        -> Set.union (go bound a) (go bound b)
      Lt _ a b         -> Set.union (go bound a) (go bound b)
      Gt _ a b         -> Set.union (go bound a) (go bound b)
      Proj _ r _       -> go bound r
      IfThenElse _ c t e -> Set.unions [go bound c, go bound t, go bound e]
      MultiWayIf _ guards e -> Set.unions (go bound e : [Set.union (go bound g) (go bound b) | MkGuardedExpr _ g b <- guards])
      Consider _ s branches ->
        Set.union (go bound s) $ Set.unions
          [ go (bound <> patternBinds p) b | MkBranch _ (When _ p) b <- branches ]
          <> Set.unions [ go bound b | MkBranch _ (Otherwise _) b <- branches ]
      Where _ b locals ->
        let (newBound, localFrees) = foldl stepLocal (bound, Set.empty) locals
        in Set.union (go newBound b) localFrees
      LetIn _ locals b ->
        let (newBound, localFrees) = foldl stepLocal (bound, Set.empty) locals
        in Set.union (go newBound b) localFrees
      Percent _ a   -> go bound a
      List _ xs     -> Set.unions (map (go bound) xs)
      Concat _ xs   -> Set.unions (map (go bound) xs)
      AsString _ a  -> go bound a
      Lam _ (MkGivenSig _ lamParams) b ->
        go (bound <> Set.fromList [resolvedName p | MkOptionallyTypedName _ p _ <- lamParams]) b
      Lit{} -> Set.empty
      Inert{} -> Set.empty
      Regulative{} -> Set.empty
      Event{} -> Set.empty
      Fetch _ a -> go bound a
      Env _ a -> go bound a
      Post _ a b c -> Set.unions [go bound a, go bound b, go bound c]
      Breach _ ma mb -> Set.unions (maybe Set.empty (go bound) ma : [maybe Set.empty (go bound) mb])

    stepLocal (bnd, frees) (LocalDecide _ (MkDecide _ _ af bdy)) =
      let nm = resolvedName (appFormHead' af)
          inner = go (bnd <> Set.fromList (map resolvedName (appFormParams af))) bdy
      in (Set.insert nm bnd, frees <> inner)
    stepLocal (bnd, frees) (LocalAssume _ (MkAssume _ _ af _)) =
      (Set.insert (resolvedName (appFormHead' af)) bnd, frees)

    patternBinds :: Pattern Resolved -> Set.Set Text
    patternBinds = \case
      PatVar _ n -> Set.singleton (resolvedName n)
      PatApp _ _ pats -> Set.unions (map patternBinds pats)
      PatCons _ h t -> Set.union (patternBinds h) (patternBinds t)
      PatExpr _ _ -> Set.empty
      PatLit _ _ -> Set.empty

-- | Helpers for synthesising extended AppForm / TypeSig / params with
-- captured variables prepended.

givenSigParams :: TypeSig Resolved -> [OptionallyTypedName Resolved]
givenSigParams (MkTypeSig _ (MkGivenSig _ ps) _) = ps

synthParam :: Text -> OptionallyTypedName Resolved -> OptionallyTypedName Resolved
synthParam newName (MkOptionallyTypedName ann templateName mTy) =
  -- Clone the template's Resolved structure but substitute the raw name.
  -- (The Resolved unique is reused; two bindings with the same unique is
  -- fine because MLIR doesn't see L4 uniques — only our sanitized names.)
  MkOptionallyTypedName ann (renameResolved newName templateName) mTy

renameResolved :: Text -> Resolved -> Resolved
renameResolved newName r = case r of
  Def u n        -> Def u (setRawName newName n)
  Ref n u o      -> Ref (setRawName newName n) u o
  OutOfScope u n -> OutOfScope u (setRawName newName n)

setRawName :: Text -> Name -> Name
setRawName t (MkName ann _) = MkName ann (NormalName t)

extendAppFormWith :: [Text] -> AppForm Resolved -> AppForm Resolved
extendAppFormWith extras (MkAppForm ann headR userParams maka) =
  let newParams = [ renameResolved t headR | t <- extras ] <> userParams
  in MkAppForm ann headR newParams maka

extendTypeSigWith :: [OptionallyTypedName Resolved] -> TypeSig Resolved -> TypeSig Resolved
extendTypeSigWith extras (MkTypeSig ann (MkGivenSig gAnn ps) mGiv) =
  MkTypeSig ann (MkGivenSig gAnn (extras <> ps)) mGiv

-- | Lower a lambda expression.
lowerLambda :: GivenSig Resolved -> Expr Resolved -> MLIRType -> LowerM Value
lowerLambda (MkGivenSig _ _params) _body _expectedTy = do
  -- For now, lambdas that appear in L4 are rare and typically get
  -- inlined by the evaluator. Emit as a local function.
  emitVal $ \vid -> arithConstantFloat vid 0.0  -- placeholder

-- | Lower a multi-way if (BRANCH ... WHEN guard THEN expr).
lowerMultiWayIf :: [GuardedExpr Resolved] -> Expr Resolved -> MLIRType -> LowerM Value
lowerMultiWayIf [] otherwise_ _ty = lowerExpr otherwise_ l4NumberType
lowerMultiWayIf (MkGuardedExpr _ guard_ body : rest) otherwise_ _ty = do
  condF64 <- lowerExpr guard_ l4NumberType
  condI1 <- unboxBoolI1 condF64
  (thenVal, thenOps) <- collectOps $ lowerExpr body l4NumberType
  (elseVal, elseOps) <- collectOps $ lowerMultiWayIf rest otherwise_ l4NumberType
  let thenBlock = Block 0 [] (thenOps ++ [scfYield [thenVal] [l4NumberType]])
      elseBlock = Block 0 [] (elseOps ++ [scfYield [elseVal] [l4NumberType]])
  emitVal $ \vid -> scfIf [vid] condI1 (Region [thenBlock]) (Region [elseBlock]) [l4NumberType]

-- | Lower string concatenation.
lowerConcat :: [Expr Resolved] -> LowerM Value
lowerConcat [] = do
  -- Empty string: intern the zero-length constant and box as f64 pointer.
  globalName <- internString ""
  ptr <- emitVal $ \vid -> mkOp [vid] "llvm.mlir.addressof" []
    [NamedAttribute "global_name" (FlatSymbolRefAttr globalName)]
    [PointerType] []
  ptrToF64 ptr
lowerConcat [x] = lowerExpr x l4NumberType
lowerConcat (x : xs) = do
  xVal <- lowerExpr x l4NumberType
  restVal <- lowerConcat xs
  emitVal $ \vid -> funcCall [vid] "__l4_str_concat"
    [xVal, restVal] [l4NumberType, l4NumberType] [l4NumberType]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

resolvedName :: Resolved -> Text
resolvedName r = rawNameToText (rawName (getActual r))

appFormHead' :: AppForm Resolved -> Resolved
appFormHead' (MkAppForm _ n _ _) = n

appFormParams :: AppForm Resolved -> [Resolved]
appFormParams (MkAppForm _ _ ps _) = ps

-- | Sanitize an L4 name for use as a WASM export symbol.
--
-- Delegates to 'L4.MLIR.Schema.sanitizeWasmSymbol' so function symbol
-- names in the emitted WASM match the names recorded in the
-- accompanying @.schema.json@ exactly. See 'L4.MLIR.Schema' for the
-- full discussion of the two-layer sanitization (API name vs symbol).
sanitizeName :: Text -> Text
sanitizeName = Schema.sanitizeWasmSymbol

lookupEnumTag :: Text -> TypeEnv -> Maybe Integer
lookupEnumTag name env =
  case [ tag | (_, variants) <- Map.toList env.enums
       , (vname, tag) <- variants
       , vname == name
       ] of
    (t : _) -> Just t
    [] -> Nothing
