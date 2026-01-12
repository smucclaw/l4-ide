{-# LANGUAGE RecordWildCards #-}
-- | Lint warning for mixed AND/OR operators at the same indentation level.
--
-- In L4's layout-sensitive syntax, operators at the same column level are
-- typically at the same precedence level. Mixing AND and OR at the same
-- column is often a logic error (confusing precedence) and this module
-- warns about such patterns.
--
-- Acceptable patterns:
--   @
--   a AND b AND c   -- All ANDs at same column: OK
--   a OR b OR c     -- All ORs at same column: OK
--   a AND (b OR c)  -- OR is on same line (in parentheses): OK
--   @
--
-- Problematic pattern (will warn):
--   @
--   a
--     AND b
--     OR c          -- AND and OR at same column, different lines: WARNING
--   @
module L4.Lint.AndOrDepth
  ( AndOrWarning(..)
  , OpType(..)
  , checkAndOrDepth
  , checkModuleAndOrDepth
  ) where

import L4.Syntax
import L4.Annotation
import L4.Parser.SrcSpan
import Data.List (groupBy, sortBy, nubBy)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe, mapMaybe)

-- | A warning about mixed AND/OR at the same indentation level.
data AndOrWarning = MkAndOrWarning
  { warningRange :: Maybe SrcRange
    -- ^ The source range to highlight (typically the operator keyword)
  , operatorType :: OpType
    -- ^ Which operator type this warning is for
  , conflictingColumn :: Int
    -- ^ The column where the conflict occurs
  }
  deriving stock (Show, Eq)

-- | The type of boolean operator.
data OpType = OpAnd | OpOr
  deriving stock (Show, Eq)

-- | Information about an AND/OR operator found in the AST.
data OpInfo = MkOpInfo
  { opType :: OpType
  , opLine :: Int
  , opColumn :: Int
  , opRange :: Maybe SrcRange
  }
  deriving stock (Show, Eq)

-- | Find the first CSN (keyword) position in an Anno's payload.
-- This finds the actual AND/OR keyword position, not the expression start.
findKeywordPosition :: Anno -> Maybe (Int, Int, Maybe SrcRange)
findKeywordPosition ann =
  let -- Find all CSN elements in the payload
      csnRanges = mapMaybe getCsnRange ann.payload

      -- Get range from a CSN element
      getCsnRange (AnnoCsn mRange _) = mRange
      getCsnRange (AnnoHole _) = Nothing
  in do
    -- Use the first CSN's range (which should be the operator keyword)
    range <- listToMaybe csnRanges
    pure (range.start.line, range.start.column, Just range)

-- | Collect all AND/OR operators from a boolean expression tree.
-- Returns operators with their keyword line/column positions.
-- Only recurses through boolean operators to stay within the same logical scope.
-- Nested constructs (IfThenElse, Consider, Where, etc.) are separate scopes
-- and are checked independently at the DECIDE level.
collectAndOrOps :: Expr n -> [OpInfo]
collectAndOrOps = go
  where
    go expr = case expr of
      -- Boolean operators we're looking for
      And ann left right ->
        mkOpInfo OpAnd ann <> go left <> go right
      Or ann left right ->
        mkOpInfo OpOr ann <> go left <> go right
      RAnd ann left right ->
        mkOpInfo OpAnd ann <> go left <> go right
      ROr ann left right ->
        mkOpInfo OpOr ann <> go left <> go right
      -- Continue through boolean connectives
      Not _ e -> go e
      Implies _ l r -> go l <> go r
      Equals _ l r -> go l <> go r
      -- Other constructs are separate scopes - don't recurse
      _ -> []

    mkOpInfo :: OpType -> Anno -> [OpInfo]
    mkOpInfo opTy ann = case findKeywordPosition ann of
      Nothing -> []
      Just (ln, col, rng) -> [MkOpInfo
        { opType = opTy
        , opLine = ln
        , opColumn = col
        , opRange = rng
        }]

-- | Check if a list of operators has mixed AND/OR at the same column.
-- Only warns if operators are on DIFFERENT lines but same column.
-- Returns warnings for any conflicting operators.
checkOpConflicts :: [OpInfo] -> [AndOrWarning]
checkOpConflicts ops =
  let
    -- Group operators by column
    sortedOps = sortBy (comparing (.opColumn)) ops
    groupedByCol = groupBy (\a b -> a.opColumn == b.opColumn) sortedOps

    -- Check each column group for mixed operators on different lines
    checkGroup [] = []
    checkGroup grp@(firstOp:_) =
      let -- Get unique lines for each operator type
          andLines = [o.opLine | o <- grp, o.opType == OpAnd]
          orLines = [o.opLine | o <- grp, o.opType == OpOr]

          -- Only warn if AND and OR appear on DIFFERENT lines at the same column
          -- This avoids warning on `AND (x OR y)` where OR is on the same line
          hasConflict = not (null andLines) && not (null orLines) &&
                        any (`notElem` orLines) andLines -- Different lines exist

          col = firstOp.opColumn
      in if hasConflict
         then map (\o -> MkAndOrWarning o.opRange o.opType col) grp
         else []
  in
    concatMap checkGroup groupedByCol

-- | Check a single expression for AND/OR mixing.
checkExprAndOr :: Expr n -> [AndOrWarning]
checkExprAndOr expr = checkOpConflicts (collectAndOrOps expr)

-- | Deduplicate warnings by range.
dedupeWarnings :: [AndOrWarning] -> [AndOrWarning]
dedupeWarnings = nubBy (\a b -> a.warningRange == b.warningRange)

-- | Check a DECIDE declaration for AND/OR warnings.
checkDecide :: Decide n -> [AndOrWarning]
checkDecide (MkDecide _ _ _ body) = dedupeWarnings $ checkExprAndOr body

-- | Check an entire module for AND/OR warnings.
checkModuleAndOrDepth :: Module n -> [AndOrWarning]
checkModuleAndOrDepth (MkModule _ _ section) = checkSection section

-- | Check a section for AND/OR warnings.
checkSection :: Section n -> [AndOrWarning]
checkSection (MkSection _ _ _ decls) = foldMap checkTopDecl decls

-- | Check a top-level declaration for AND/OR warnings.
checkTopDecl :: TopDecl n -> [AndOrWarning]
checkTopDecl (Decide _ d) = checkDecide d
checkTopDecl (Declare _ _) = []
checkTopDecl (Assume _ _) = []
checkTopDecl (Directive _ dir) = checkDirective dir
checkTopDecl (Import _ _) = []
checkTopDecl (Section _ s) = checkSection s

-- | Check a directive for AND/OR warnings.
checkDirective :: Directive n -> [AndOrWarning]
checkDirective (LazyEval _ e) = dedupeWarnings $ checkExprAndOr e
checkDirective (LazyEvalTrace _ e) = dedupeWarnings $ checkExprAndOr e
checkDirective (Check _ e) = dedupeWarnings $ checkExprAndOr e
checkDirective (Contract _ e1 e2 es) = dedupeWarnings $ checkExprAndOr e1 <> checkExprAndOr e2 <> foldMap checkExprAndOr es
checkDirective (Assert _ e) = dedupeWarnings $ checkExprAndOr e

-- | Convenience function to check a parsed module.
-- Returns all AND/OR depth warnings found.
checkAndOrDepth :: Module Name -> [AndOrWarning]
checkAndOrDepth = checkModuleAndOrDepth
