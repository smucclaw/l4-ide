-- | Pretty-printer for MLIR textual IR format.
--
-- Emits valid MLIR assembly that can be consumed by @mlir-opt@,
-- @mlir-translate@, and other MLIR tools. Follows the MLIR
-- textual format specification.
module L4.MLIR.Emit
  ( emitModule
  , emitOperation
  , emitType
  , renderMLIR
  ) where

import L4.MLIR.IR

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

-- | Render an MLIR module to Text.
renderMLIR :: MLIRModule -> Text
renderMLIR = renderStrict . layoutPretty defaultLayoutOptions . emitModule

-- | Emit a top-level MLIR module.
emitModule :: MLIRModule -> Doc ann
emitModule m =
  "module" <+> lbrace <> hardline
  <> indent 2 (vsep (map emitOperation m.moduleOps))
  <> hardline <> rbrace <> hardline

-- | Emit a single MLIR operation.
emitOperation :: Operation -> Doc ann
emitOperation op
  -- func.func has special syntax
  | op.opName == "func.func" = emitFuncFunc op
  -- memref.global / llvm.mlir.global have special syntax
  | op.opName == "memref.global" = emitGlobal op
  | op.opName == "llvm.mlir.global" = emitLlvmGlobal op
  -- Generic operation format
  | otherwise = emitGenericOp op

emitFuncFunc :: Operation -> Doc ann
emitFuncFunc op =
  let symName = findStringAttr "sym_name" op.opAttributes
      vis = findStringAttr "sym_visibility" op.opAttributes
      funcTy = findTypeAttr "function_type" op.opAttributes
      (argTys, retTys) = case funcTy of
        Just (FunctionType as rs) -> (as, rs)
        _ -> ([], [])
      visPart = if Text.null vis then mempty else pretty vis <+> mempty
      -- For declarations (no body), use synthetic arg names
      argNames = case op.opRegions of
        [r] -> case r.regionBlocks of
          (b : _) -> b.blockArgs
          [] -> zip [0..] argTys
        _ -> zip [0..] argTys
      retPart = if null retTys then mempty else " ->" <+> emitTypeList retTys
  in "func.func" <+> visPart <> "@" <> pretty symName
    <> parens (hsep (punctuate comma (zipWith emitFuncArg argNames argTys)))
    <> retPart
    <> case op.opRegions of
         (r : _) -> " " <> emitRegion r
         []      -> mempty

emitFuncArg :: (ValueId, MLIRType) -> MLIRType -> Doc ann
emitFuncArg (vid, _) ty = emitValueId vid <> ":" <+> emitType ty

emitGlobal :: Operation -> Doc ann
emitGlobal op =
  let name = findStringAttr "sym_name" op.opAttributes
      ty = findTypeAttr "type" op.opAttributes
  in "memref.global" <+> "@" <> pretty name
    <+> ":" <+> maybe "none" emitType ty
    <+> "=" <+> emitAttrValue (findAttr "initial_value" op.opAttributes)

emitLlvmGlobal :: Operation -> Doc ann
emitLlvmGlobal op =
  let name = findStringAttr "sym_name" op.opAttributes
      val = findAttr "value" op.opAttributes
      -- LLVM's @!llvm.array<N x i8>@ size must be the UTF-8 *byte* count,
      -- not the character count. 'Text.length' counts code points, which
      -- disagrees on any non-ASCII input (emoji, accents, etc.). We also
      -- append a trailing NUL byte so the host runtime's C-string reader
      -- can walk to the terminator instead of running past one global
      -- and into the next.
      (nulTerminated, strByteLen) = case val of
        Just (StringAttr s) ->
          let bytes = BS.length (Text.Encoding.encodeUtf8 s)
          in (Just (StringAttr (s <> Text.singleton '\0')), bytes + 1)
        _ -> (val, 0)
  in "llvm.mlir.global private constant @" <> pretty name
    <> parens (emitAttrValue nulTerminated)
    <+> ":" <+> "!llvm.array<" <> pretty strByteLen <+> "x i8>"

emitGenericOp :: Operation -> Doc ann
emitGenericOp op
  | op.opName == "func.call" = emitFuncCall op
  | op.opName == "func.return" = emitFuncReturn op
  | op.opName == "scf.yield" = emitScfYield op
  | op.opName == "arith.constant" = emitArithConstant op
  | op.opName == "scf.if" = emitScfIf op
  | op.opName == "llvm.extractvalue" = emitLlvmExtractvalue op
  | op.opName == "llvm.insertvalue" = emitLlvmInsertvalue op
  | op.opName == "llvm.mlir.undef" = emitLlvmUndef op
  | op.opName == "arith.cmpf" = emitArithCmp op True
  | op.opName == "arith.cmpi" = emitArithCmp op False
  | isArithBinop op.opName = emitArithBinop op
  | isArithCastOp op.opName = emitArithCast op
  | op.opName == "llvm.ptrtoint" = emitLlvmPtrToInt op
  | op.opName == "llvm.inttoptr" = emitLlvmIntToPtr op
  | op.opName == "llvm.icmp" = emitLlvmIcmp op
  | op.opName == "llvm.mlir.null" = emitLlvmNull op
  | op.opName == "llvm.load" = emitLlvmLoad op
  | op.opName == "llvm.store" = emitLlvmStore op
  | op.opName == "llvm.alloca" = emitLlvmAlloca op
  | op.opName == "memref.get_global" = emitMemrefGetGlobal op
  | op.opName == "llvm.mlir.addressof" = emitLlvmAddressof op
  | op.opName == "llvm.getelementptr" = emitLlvmGep op
  | op.opName == "llvm.call" = emitLlvmCall op
  | otherwise = emitGenericOpDefault op

-- | @llvm.call@: like @func.call@ — @llvm.call @callee(%args) : (ts) -> rt@.
emitLlvmCall :: Operation -> Doc ann
emitLlvmCall op =
  let callee = findSymbolRefAttr "callee" op.opAttributes
      resultPart = if null op.opResults
        then mempty
        else hsep (punctuate comma (map emitValueId op.opResults)) <+> "=" <+> mempty
      argsPart = parens (hsep (punctuate comma (map emitValue op.opOperands)))
      typePart = parens (hsep (punctuate comma (map emitType op.opOperandTypes)))
        <+> "->" <+> emitTypeList (if null op.opResultTypes then [NoneType] else op.opResultTypes)
  in resultPart <> "llvm.call" <+> "@" <> pretty callee <> argsPart <+> ":" <+> typePart

-- | @llvm.getelementptr@ inline syntax:
--     %r = llvm.getelementptr %base[%idx] : (!llvm.ptr, i64) -> !llvm.ptr
emitLlvmGep :: Operation -> Doc ann
emitLlvmGep op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      (base, idxs) = case op.opOperands of
        (b : is) -> (emitValue b, is)
        []       -> ("/*missing*/", [])
      idxPart = brackets (hsep (punctuate comma (map emitValue idxs)))
      elemTy = case findAttr "elem_type" op.opAttributes of
        Just (TypeAttr t) -> emitType t
        _ -> "i8"
      argTyList = parens (hsep (punctuate comma ("!llvm.ptr" : map emitType (drop 1 op.opOperandTypes))))
  -- Modern LLVM dialect GEP syntax: the element type is the *trailing*
  -- type annotation, not an attribute.
  --     %r = llvm.getelementptr %p[%i] : (!llvm.ptr, i64) -> !llvm.ptr, i8
  in resultPart <> "llvm.getelementptr" <+> base <> idxPart
     <+> ":" <+> argTyList <+> "->" <+> "!llvm.ptr" <> "," <+> elemTy

-- | %r = llvm.mlir.addressof @global : !llvm.ptr
emitLlvmAddressof :: Operation -> Doc ann
emitLlvmAddressof op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      name = findSymbolRefAttr "global_name" op.opAttributes
  in resultPart <> "llvm.mlir.addressof" <+> "@" <> pretty name <+> ":" <+> "!llvm.ptr"

-- | %r = memref.get_global @name : memref<N x i8>
emitMemrefGetGlobal :: Operation -> Doc ann
emitMemrefGetGlobal op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      name = findSymbolRefAttr "name" op.opAttributes
      ty = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "memref<?xi8>"
  in resultPart <> "memref.get_global" <+> "@" <> pretty name <+> ":" <+> ty

-- | %r = llvm.load %ptr : !llvm.ptr -> !type
emitLlvmLoad :: Operation -> Doc ann
emitLlvmLoad op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      ptr = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing*/"
      ty = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "i64"
  in resultPart <> "llvm.load" <+> ptr <+> ":" <+> "!llvm.ptr" <+> "->" <+> ty

-- | llvm.store %val, %ptr : !type, !llvm.ptr
emitLlvmStore :: Operation -> Doc ann
emitLlvmStore op =
  let (val, ptr) = case op.opOperands of
        [v, p] -> (emitValue v, emitValue p)
        _ -> ("/*missing*/", "/*missing*/")
      valTy = case op.opOperandTypes of
        (t : _) -> emitType t
        _ -> "f64"  -- let MLIR infer if not provided explicitly
  in "llvm.store" <+> val <> "," <+> ptr <+> ":" <+> valTy <> "," <+> "!llvm.ptr"

-- | %r = llvm.alloca %size x !type : (i64) -> !llvm.ptr
emitLlvmAlloca :: Operation -> Doc ann
emitLlvmAlloca op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      size = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing*/"
      elemTy = case findAttr "elem_type" op.opAttributes of
        Just (TypeAttr t) -> emitType t
        _ -> "i8"
  in resultPart <> "llvm.alloca" <+> size <+> "x" <+> elemTy
     <+> ":" <+> "(i64) -> !llvm.ptr"

-- | llvm.icmp "eq" %a, %b : !llvm.ptr
emitLlvmIcmp :: Operation -> Doc ann
emitLlvmIcmp op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      pred_ = findStringAttr "predicate" op.opAttributes
      operandsPart = hsep (punctuate comma (map emitValue op.opOperands))
      operandTy = case op.opOperandTypes of
        (t : _) -> emitType t
        [] -> "!llvm.ptr"
  in resultPart <> "llvm.icmp" <+> dquotes (pretty pred_) <+> operandsPart <+> ":" <+> operandTy

-- | %r = llvm.mlir.zero : !llvm.ptr  (replaces deprecated llvm.mlir.null)
emitLlvmNull :: Operation -> Doc ann
emitLlvmNull op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
  in resultPart <> "llvm.mlir.zero" <+> ":" <+> "!llvm.ptr"

-- | Recognize arith dialect binary ops that use inline syntax.
isArithBinop :: Text -> Bool
isArithBinop name = name `elem`
  [ "arith.addf", "arith.subf", "arith.mulf", "arith.divf", "arith.remf"
  , "arith.addi", "arith.subi", "arith.muli", "arith.divsi", "arith.remsi"
  , "arith.andi", "arith.ori", "arith.xori"
  , "arith.negf", "arith.select"
  ]

-- | Inline binary op syntax: %r = arith.addf %a, %b : f64
emitArithBinop :: Operation -> Doc ann
emitArithBinop op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      operandsPart = hsep (punctuate comma (map emitValue op.opOperands))
      -- arith.select has 3 operands (i1, T, T) — use its result type
      -- other ops have same type for operands and result
      ty = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "f64"
  in resultPart <> pretty op.opName <+> operandsPart <+> ":" <+> ty

-- | ABI boundary cast ops: @arith.uitofp@, @arith.sitofp@,
-- @arith.fptoui@, @arith.fptosi@, @arith.bitcast@, @arith.extui@,
-- @arith.trunci@, @arith.extsi@. MLIR's custom syntax is
-- @%r = OPNAME %x : SRC to DST@.
isArithCastOp :: Text -> Bool
isArithCastOp name = name `elem`
  [ "arith.uitofp", "arith.sitofp", "arith.fptoui", "arith.fptosi"
  , "arith.bitcast", "arith.extui", "arith.trunci", "arith.extsi"
  ]

emitArithCast :: Operation -> Doc ann
emitArithCast op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      src = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing*/"
      srcTy = case op.opOperandTypes of
        (t : _) -> emitType t
        [] -> "f64"
      dstTy = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "f64"
  in resultPart <> pretty op.opName <+> src <+> ":" <+> srcTy <+> "to" <+> dstTy

-- | @llvm.ptrtoint@ / @llvm.inttoptr@: inline syntax.
-- @%r = llvm.ptrtoint %p : !llvm.ptr to i64@.
emitLlvmPtrToInt :: Operation -> Doc ann
emitLlvmPtrToInt op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      src = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing*/"
      dstTy = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "i64"
  in resultPart <> "llvm.ptrtoint" <+> src <+> ":" <+> "!llvm.ptr" <+> "to" <+> dstTy

emitLlvmIntToPtr :: Operation -> Doc ann
emitLlvmIntToPtr op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      src = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing*/"
      srcTy = case op.opOperandTypes of
        (t : _) -> emitType t
        [] -> "i64"
  in resultPart <> "llvm.inttoptr" <+> src <+> ":" <+> srcTy <+> "to" <+> "!llvm.ptr"

emitFuncCall :: Operation -> Doc ann
emitFuncCall op =
  let callee = findSymbolRefAttr "callee" op.opAttributes
      resultPart = if null op.opResults
        then mempty
        else hsep (punctuate comma (map emitValueId op.opResults)) <+> "=" <+> mempty
      argsPart = parens (hsep (punctuate comma (map emitValue op.opOperands)))
      typePart = parens (hsep (punctuate comma (map emitType op.opOperandTypes)))
        <+> "->" <+> emitTypeList op.opResultTypes
  in resultPart <> "func.call" <+> "@" <> pretty callee <> argsPart <+> ":" <+> typePart

emitFuncReturn :: Operation -> Doc ann
emitFuncReturn op =
  if null op.opOperands
    then "func.return"
    else "func.return" <+> hsep (punctuate comma (map emitValue op.opOperands))
      <+> ":" <+> hsep (punctuate comma (map emitType op.opOperandTypes))

emitScfYield :: Operation -> Doc ann
emitScfYield op =
  if null op.opOperands
    then "scf.yield"
    else "scf.yield" <+> hsep (punctuate comma (map emitValue op.opOperands))
      <+> ":" <+> hsep (punctuate comma (map emitType op.opOperandTypes))

-- | scf.if %cond -> (result_types) { then } else { else }
emitScfIf :: Operation -> Doc ann
emitScfIf op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      cond = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing cond*/"
      retPart = if null op.opResultTypes
        then mempty
        else " ->" <+> parens (hsep (punctuate comma (map emitType op.opResultTypes)))
      regions = case op.opRegions of
        [t, e] -> " " <> emitRegion t <+> "else" <+> emitRegion e
        [t]    -> " " <> emitRegion t
        _      -> mempty
  in resultPart <> "scf.if" <+> cond <> retPart <> regions

-- | arith.constant uses inline attribute syntax: %0 = arith.constant 42.0 : f64
emitArithConstant :: Operation -> Doc ann
emitArithConstant op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      val = findAttr "value" op.opAttributes
      ty = case op.opResultTypes of
        (t : _) -> t
        [] -> FloatType 64
  in resultPart <> "arith.constant" <+> emitConstantValue val <+> ":" <+> emitType ty
  where
    emitConstantValue (Just (FloatAttr _ v)) = prettyFloat v
    emitConstantValue (Just (IntegerAttr _ v)) = pretty v
    emitConstantValue (Just (BoolAttr True)) = "true"
    emitConstantValue (Just (BoolAttr False)) = "false"
    emitConstantValue _ = "0"

-- | llvm.extractvalue %struct[idx] : !llvm.struct<...>
emitLlvmExtractvalue :: Operation -> Doc ann
emitLlvmExtractvalue op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      container = case op.opOperands of
        (v : _) -> emitValue v
        [] -> "/*missing*/"
      positions = case findAttr "position" op.opAttributes of
        Just (ArrayAttr idxs) -> hcat (map (\a -> "[" <> emitPositionIdx a <> "]") idxs)
        _ -> "[0]"
      containerTy = case op.opOperandTypes of
        (t : _) -> emitType t
        [] -> "!llvm.struct<>"
  in resultPart <> "llvm.extractvalue" <+> container <> positions <+> ":" <+> containerTy
  where
    emitPositionIdx (IntegerAttr _ v) = pretty v
    emitPositionIdx _ = "0"

-- | llvm.insertvalue %val, %struct[idx] : !llvm.struct<...>
emitLlvmInsertvalue :: Operation -> Doc ann
emitLlvmInsertvalue op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      (val, container) = case op.opOperands of
        [c, v] -> (emitValue v, emitValue c)
        _ -> ("/*missing*/", "/*missing*/")
      positions = case findAttr "position" op.opAttributes of
        Just (ArrayAttr idxs) -> hcat (map (\a -> "[" <> emitPositionIdx a <> "]") idxs)
        _ -> "[0]"
      containerTy = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "!llvm.struct<>"
  in resultPart <> "llvm.insertvalue" <+> val <> "," <+> container <> positions <+> ":" <+> containerTy
  where
    emitPositionIdx (IntegerAttr _ v) = pretty v
    emitPositionIdx _ = "0"

-- | llvm.mlir.undef : !type
emitLlvmUndef :: Operation -> Doc ann
emitLlvmUndef op =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      ty = case op.opResultTypes of
        (t : _) -> emitType t
        [] -> "!llvm.struct<>"
  in resultPart <> "llvm.mlir.undef" <+> ":" <+> ty

-- | arith.cmpf/cmpi with keyword predicate: arith.cmpf oge, %a, %b : f64
emitArithCmp :: Operation -> Bool -> Doc ann
emitArithCmp op isFloat =
  let resultPart = case op.opResults of
        (vid : _) -> emitValueId vid <+> "=" <+> mempty
        [] -> mempty
      predKeyword = case findAttr "predicate" op.opAttributes of
        Just (IntegerAttr _ v) ->
          if isFloat then cmpfPredKeyword v else cmpiPredKeyword v
        _ -> "oeq"
      operandsPart = hsep (punctuate comma (map emitValue op.opOperands))
      operandTy = case op.opOperandTypes of
        (t : _) -> emitType t
        [] -> "f64"
  in resultPart <> "arith." <> (if isFloat then "cmpf" else "cmpi")
     <+> predKeyword <> "," <+> operandsPart <+> ":" <+> operandTy

cmpfPredKeyword :: Integer -> Doc ann
cmpfPredKeyword = \case
  0 -> "false"; 1 -> "oeq"; 2 -> "ogt"; 3 -> "oge"; 4 -> "olt"; 5 -> "ole"
  6 -> "one"; 7 -> "ord"; 8 -> "ueq"; 9 -> "ugt"; 10 -> "uge"; 11 -> "ult"
  12 -> "ule"; 13 -> "une"; 14 -> "uno"; 15 -> "true"
  _ -> "oeq"

cmpiPredKeyword :: Integer -> Doc ann
cmpiPredKeyword = \case
  0 -> "eq"; 1 -> "ne"; 2 -> "slt"; 3 -> "sle"; 4 -> "sgt"; 5 -> "sge"
  6 -> "ult"; 7 -> "ule"; 8 -> "ugt"; 9 -> "uge"
  _ -> "eq"

emitGenericOpDefault :: Operation -> Doc ann
emitGenericOpDefault op =
  let resultPart = if null op.opResults
        then mempty
        else hsep (punctuate comma (map emitValueId op.opResults)) <+> "=" <+> mempty
      opNameDoc = pretty op.opName
      operandsPart = if null op.opOperands
        then mempty
        else parens (hsep (punctuate comma (map emitValue op.opOperands)))
      -- Successor blocks
      successorPart = if null op.opSuccessors
        then mempty
        else " " <> brackets (hsep (punctuate comma (map emitBlockRef op.opSuccessors)))
      -- Inline attributes (skip well-known structural ones)
      userAttrs = filter (not . isStructuralAttr) op.opAttributes
      attrPart = if null userAttrs
        then mempty
        else " " <> lbrace <> hsep (punctuate comma (map emitNamedAttr userAttrs)) <> rbrace
      -- Type signature
      typePart = emitOpTypeSig op
      -- Regions
      regionPart = if null op.opRegions
        then mempty
        else " " <> hsep (map emitRegion op.opRegions)
  in resultPart <> opNameDoc <+> operandsPart <> successorPart <> attrPart <> regionPart <+> ":" <+> typePart

emitOpTypeSig :: Operation -> Doc ann
emitOpTypeSig op
  -- Return-only ops (constants)
  | null op.opOperandTypes && not (null op.opResultTypes) =
      emitTypeList op.opResultTypes
  -- Functional signature
  | not (null op.opOperandTypes) || not (null op.opResultTypes) =
      parens (hsep (punctuate comma (map emitType op.opOperandTypes)))
      <+> "->" <+> emitTypeList op.opResultTypes
  -- No types
  | otherwise = "()"

emitTypeList :: [MLIRType] -> Doc ann
emitTypeList [t] = emitType t
emitTypeList ts  = parens (hsep (punctuate comma (map emitType ts)))

-- | Emit an MLIR type.
emitType :: MLIRType -> Doc ann
emitType = \case
  IntegerType w -> "i" <> pretty w
  IndexType -> "index"
  FloatType 16 -> "f16"
  FloatType 32 -> "f32"
  FloatType 64 -> "f64"
  FloatType w -> "f" <> pretty w
  NoneType -> "none"
  FunctionType args results ->
    parens (hsep (punctuate comma (map emitType args)))
    <+> "->" <+> emitTypeList results
  MemRefType shape elem_ ->
    "memref<" <> hsep (punctuate "x" (map emitDim shape ++ [emitType elem_])) <> ">"
  UnrankedMemRefType elem_ ->
    "memref<*x" <> emitType elem_ <> ">"
  TupleType ts ->
    "tuple<" <> hsep (punctuate comma (map emitType ts)) <> ">"
  StructType name fields ->
    -- Internal types (l4.*) use anonymous structs; user types use named structs
    if "l4." `Text.isPrefixOf` name
    then "!llvm.struct<(" <> hsep (punctuate comma (map emitType fields)) <> ")>"
    else "!llvm.struct<\"" <> pretty name <> "\", (" <> hsep (punctuate comma (map emitType fields)) <> ")>"
  PointerType -> "!llvm.ptr"
  ArrayType n elem_ ->
    "!llvm.array<" <> pretty n <+> "x" <+> emitType elem_ <> ">"
  NamedType n -> "!" <> pretty n
  where
    emitDim Nothing  = "?"
    emitDim (Just n) = pretty n

-- | Emit an SSA value reference.
emitValue :: Value -> Doc ann
emitValue = \case
  SSAValue vid -> "%" <> pretty vid
  BlockArg bid idx -> "%" <> "arg" <> pretty bid <> "_" <> pretty idx
  ConstantRef name -> "@" <> pretty name

emitValueId :: ValueId -> Doc ann
emitValueId vid = "%" <> pretty vid

emitBlockRef :: BlockId -> Doc ann
emitBlockRef bid = "^bb" <> pretty bid

-- | Emit a region (list of blocks). Entry block has no label.
emitRegion :: Region -> Doc ann
emitRegion r = case r.regionBlocks of
  [] -> lbrace <> hardline <> rbrace
  (entry : rest) -> lbrace <> hardline
    <> indent 2 (emitBlock entry)
    <> (if null rest then mempty else hardline <> vsep (map emitLabeledBlock rest))
    <> hardline <> rbrace

-- | Emit a basic block. Entry blocks (first in a region) omit the label.
emitBlock :: Block -> Doc ann
emitBlock b = vsep (map emitOperation b.blockOps)

-- | Emit a non-entry basic block (with label).
emitLabeledBlock :: Block -> Doc ann
emitLabeledBlock b =
  let header = emitBlockRef b.blockId
        <> (if null b.blockArgs then mempty
            else parens (hsep (punctuate comma (map emitBlockArg b.blockArgs))))
        <> ":"
      ops = vsep (map emitOperation b.blockOps)
  in header <> hardline <> indent 2 ops

emitBlockArg :: (ValueId, MLIRType) -> Doc ann
emitBlockArg (vid, ty) = emitValueId vid <> ":" <+> emitType ty

-- | Emit a named attribute.
emitNamedAttr :: NamedAttribute -> Doc ann
emitNamedAttr na = pretty na.attrName <+> "=" <+> emitAttrValue (Just na.attrValue)

emitAttrValue :: Maybe Attribute -> Doc ann
emitAttrValue Nothing = "unit"
emitAttrValue (Just attr) = case attr of
  IntegerAttr ty val -> pretty val <+> ":" <+> emitType ty
  FloatAttr ty val -> prettyFloat val <+> ":" <+> emitType ty
  StringAttr s -> dquotes (pretty (escapeString s))
  BoolAttr True -> "true"
  BoolAttr False -> "false"
  TypeAttr ty -> emitType ty
  ArrayAttr as -> brackets (hsep (punctuate comma (map (emitAttrValue . Just) as)))
  DenseAttr ty as -> "dense<" <> brackets (hsep (punctuate comma (map (emitAttrValue . Just) as))) <> ">" <+> ":" <+> emitType ty
  UnitAttr -> "unit"
  FlatSymbolRefAttr s -> "@" <> pretty s
  SymbolNameAttr s -> dquotes (pretty s)
  DictAttr nas -> lbrace <> hsep (punctuate comma (map emitNamedAttr nas)) <> rbrace

prettyFloat :: Double -> Doc ann
prettyFloat d
  | isInfinite d && d > 0 = "0x7FF0000000000000"
  | isInfinite d          = "0xFFF0000000000000"
  | isNaN d               = "0x7FF8000000000000"
  | d == 0 && isNegZero d = "-0.0"
  | d == fromIntegral (round d :: Integer) = pretty (show d)
  | otherwise = pretty (show d)

isNegZero :: Double -> Bool
isNegZero x = x == 0 && show x == "-0.0"

escapeString :: Text -> Text
escapeString = Text.concatMap $ \c -> case c of
  '"'  -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\t' -> "\\t"
  _    -> Text.singleton c

-- Attribute lookup helpers

findStringAttr :: Text -> [NamedAttribute] -> Text
findStringAttr key attrs = case [s | NamedAttribute k (StringAttr s) <- attrs, k == key] of
  (s : _) -> s
  [] -> ""

findSymbolRefAttr :: Text -> [NamedAttribute] -> Text
findSymbolRefAttr key attrs = case [s | NamedAttribute k (FlatSymbolRefAttr s) <- attrs, k == key] of
  (s : _) -> s
  [] -> ""

findTypeAttr :: Text -> [NamedAttribute] -> Maybe MLIRType
findTypeAttr key attrs = case [t | NamedAttribute k (TypeAttr t) <- attrs, k == key] of
  (t : _) -> Just t
  [] -> Nothing

findAttr :: Text -> [NamedAttribute] -> Maybe Attribute
findAttr key attrs = case [v | NamedAttribute k v <- attrs, k == key] of
  (v : _) -> Just v
  [] -> Nothing

isStructuralAttr :: NamedAttribute -> Bool
isStructuralAttr na = na.attrName `elem` ["sym_name", "function_type", "callee", "type", "elem_type"]
