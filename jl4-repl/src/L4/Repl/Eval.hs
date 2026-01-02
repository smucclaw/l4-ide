module L4.Repl.Eval 
  ( evalExprInContext
  , getMergedContext
  ) where

import qualified Data.Map.Strict as Map
import Language.LSP.Protocol.Types (NormalizedFilePath, NormalizedUri, normalizedFilePathToUri)

import Development.IDE.Graph (Action)

import L4.Syntax (Expr, Resolved)
import L4.TypeCheck.Types (Environment, EntityInfo)
import L4.EvaluateLazy (EvalConfig, EvalDirectiveResult)
import LSP.Core.Shake (IdeState)
import LSP.Core.Service (runAction)
import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules

import L4.Repl.Types

-- | Get merged context from all loaded files
getMergedContext :: IdeState -> [NormalizedFilePath] -> IO (Maybe EvalContext)
getMergedContext ide files = do
  results <- runAction "repl-context" ide $ do
    traverse getFileContext files
  pure $ mergeContexts (catMaybes results)
  where
    getFileContext :: NormalizedFilePath -> Action (Maybe (Environment, EntityInfo))
    getFileContext nfp = do
      let uri = normalizedFilePathToUri nfp
      mtc <- Shake.use Rules.SuccessfulTypeCheck uri
      pure $ case mtc of
        Nothing -> Nothing
        Just tc -> Just (tc.environment, tc.entityInfo)

    mergeContexts :: [(Environment, EntityInfo)] -> Maybe EvalContext
    mergeContexts [] = Nothing
    mergeContexts ctxs = Just EvalContext
      { environment = Map.unions (map fst ctxs)
      , entityInfo = Map.unions (map snd ctxs)
      }

    catMaybes = foldr (\x acc -> maybe acc (:acc) x) []

-- | Evaluate an expression in the given context
evalExprInContext 
  :: EvalConfig 
  -> EvalContext 
  -> NormalizedUri
  -> Expr Resolved 
  -> IO (Maybe EvalDirectiveResult)
evalExprInContext _config _ctx _uri _expr = do
  -- For now, we can't easily evaluate standalone expressions without a module context
  -- This is a placeholder that will need proper implementation
  -- TODO: Use execEvalExprInContextOfModule properly
  pure Nothing
