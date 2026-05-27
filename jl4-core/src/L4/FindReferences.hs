{-# LANGUAGE RecordWildCards #-}
-- | Computing info for "find references"
--
-- This module provides a simple implementation of find references that works
-- on a single file. It finds all usages of a symbol at a given position.
module L4.FindReferences
  ( findReferences
  , ReferenceMapping(..)
  , buildReferenceMapping
  , lookupReference
  , singletonReferenceMapping
  ) where

import Base
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import L4.Annotation
import L4.Parser.SrcSpan (SrcPos(..), SrcRange(..))
import L4.Syntax (Resolved, Module(..), Unique, getUnique)
import L4.TypeCheck (toResolved)
import qualified L4.Utils.IntervalMap as IV

-- | A mapping from positions to definitions and from definitions to all their uses.
--
-- The two maps deduplicate by construction: the LSP references handler
-- 'mconcat's per-file mappings ('GetReferences uri' walks both 'uri''s own
-- module AND every transitive dep), which means the SAME resolved name
-- often arrives via multiple files. If we stored ranges as @[SrcRange]@,
-- the list would grow O(deps × files), 'lookupReference' would emit each
-- range that many times, and the response balloons (observed: 20k+ duplicated
-- locations for ~20 real references). 'Set SrcRange' collapses those down to
-- one entry per (range, moduleUri) pair.
data ReferenceMapping = ReferenceMapping
  { actualToOriginal :: IV.IntervalMap SrcPos (Unique, NormalizedUri)
    -- ^ Getting the original occurrence of a name, based on its reference's source range.
    --   The carried 'NormalizedUri' is the URI of the file that contributed this
    --   interval. It's needed because 'SrcPos' is (line, column) only — when the
    --   reference handler 'mconcat's per-file mappings before searching, intervals
    --   from different files at identical coordinates would otherwise collide and
    --   'lookupReference' would surface uniques from unrelated files. The URI lets
    --   the search filter to intervals from the cursor's own file.
  , originalToActual :: Map Unique (Set SrcRange)
    -- ^ Getting the source range of all references of an original definition.
    --   'Set' rather than '[]' so 'mconcat' across redundant per-file mappings
    --   yields one entry per range instead of multiplying by dep-fan-out.
  }
  deriving stock (Generic)

-- | NFData instance that doesn't force the IntervalMap deeply (it can be very large)
instance NFData ReferenceMapping where
  rnf ReferenceMapping{..} =
    actualToOriginal `seq` rnf originalToActual

instance Semigroup ReferenceMapping where
  a <> b = ReferenceMapping
    { actualToOriginal = a.actualToOriginal <> b.actualToOriginal
    , originalToActual = Map.unionWith Set.union a.originalToActual b.originalToActual
    }

instance Monoid ReferenceMapping where
  mempty = ReferenceMapping IV.empty Map.empty

-- | Create a singleton reference mapping
singletonReferenceMapping :: Unique -> SrcRange -> ReferenceMapping
singletonReferenceMapping originalName actualRange = ReferenceMapping
  { actualToOriginal =
      IV.singleton
        (IV.srcRangeToInterval actualRange)
        (originalName, actualRange.moduleUri)
  , originalToActual = Map.singleton originalName (Set.singleton actualRange)
  }

-- | Build a reference mapping from a resolved module.
buildReferenceMapping :: Module Resolved -> ReferenceMapping
buildReferenceMapping mod' = foldMap spanOf (toResolved mod')
  where
    spanOf :: Resolved -> ReferenceMapping
    spanOf resolved =
      maybe
        mempty
        (singletonReferenceMapping $ getUnique resolved)
        (rangeOf resolved)

-- | Look up all references for the symbol at the given position in
-- the file identified by 'cursorUri'. The URI argument disambiguates
-- when the mapping has been 'mconcat'ed across multiple files: an
-- interval that contains 'pos' only counts as the cursor's anchor if
-- it originated in the same file as the cursor (positions in different
-- files at identical line/col would otherwise contribute spurious
-- uniques and pull in unrelated symbols' ranges).
--
-- We also deduplicate the matched uniques themselves: 'actualToOriginal'
-- is a flat list of @(interval, value)@ pairs whose 'mappend' is
-- concatenation, so the cursor's interval typically appears once per
-- file that contributed it. Without 'Set.fromList', every duplicate
-- would re-emit the full range list — same blow-up as below, just on
-- the unique axis.
lookupReference :: NormalizedUri -> SrcPos -> ReferenceMapping -> [SrcRange]
lookupReference cursorUri pos mapping =
  let matchedUniques = Set.fromList
        [ n
        | (_, (n, intervalUri)) <- IV.search pos mapping.actualToOriginal
        , intervalUri == cursorUri
        ]
      allRanges = foldMap
        (\n -> Map.findWithDefault Set.empty n mapping.originalToActual)
        matchedUniques
  in Set.toList allRanges

-- | Find all references to the symbol at the given position.
-- Returns a list of source ranges where the symbol is used.
findReferences :: SrcPos -> Module Resolved -> [SrcRange]
findReferences pos mod'@(MkModule _ uri _) =
  let mapping = buildReferenceMapping mod'
  in lookupReference uri pos mapping
