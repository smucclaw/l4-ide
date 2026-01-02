{-|
GraphViz Options

This module contains just the GraphVizOptions type, separated from GraphViz2
to avoid module cycles (TracePolicy needs GraphVizOptions, but shouldn't
depend on the full GraphViz2 module which depends on Trace).
-}
module L4.EvaluateLazy.GraphVizOptions where

-- | Options for controlling GraphViz output
data GraphVizOptions = GraphVizOptions
  { includeValues :: Bool
  , showUnevaluated :: Bool
  , simplifyTrivial :: Bool
  , maxDepth :: Maybe Int
  -- Optimization flags
  , collapseFunctionLookups :: Bool
  , collapseSimplePaths :: Bool
  , deduplicateBindings :: Bool  -- Merge nodes for shared WHERE/LET bindings
  } deriving (Eq, Show)

defaultGraphVizOptions :: GraphVizOptions
defaultGraphVizOptions = GraphVizOptions
  { includeValues = True
  , showUnevaluated = False
  , simplifyTrivial = True
  , maxDepth = Nothing
  , collapseFunctionLookups = False
  , collapseSimplePaths = False
  , deduplicateBindings = True
  }
