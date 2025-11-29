{-# LANGUAGE LambdaCase #-}
module Backend.DirectiveFilter
  ( filterIdeDirectives
  ) where

import Base
import L4.Syntax

-- | Remove IDE-specific directives from a module AST.
--
-- Filters out:
--   - LazyEval (#EVAL)
--   - LazyEvalTrace (#EVALTRACE)
--   - Check (#CHECK)
--   - Assert (#ASSERT)
--
-- Keeps:
--   - Contract (#CONTRACT) - may have semantic meaning
--   - All other declarations (DECIDE, DECLARE, ASSUME, etc.)
filterIdeDirectives :: Module n -> Module n
filterIdeDirectives (MkModule anno imports section) =
  MkModule anno imports (filterSection section)

filterSection :: Section n -> Section n
filterSection (MkSection anno lvl heading decls) =
  MkSection anno lvl heading (mapMaybe filterTopDecl decls)

filterTopDecl :: TopDecl n -> Maybe (TopDecl n)
filterTopDecl = \case
  Directive _ d | isIdeDirective d -> Nothing
  Section anno s -> Just $ Section anno (filterSection s)
  other -> Just other

isIdeDirective :: Directive n -> Bool
isIdeDirective = \case
  LazyEval{}      -> True   -- #EVAL
  LazyEvalTrace{} -> True   -- #EVALTRACE
  Check{}         -> True   -- #CHECK
  Assert{}        -> True   -- #ASSERT
  Contract{}      -> False  -- Keep #CONTRACT
