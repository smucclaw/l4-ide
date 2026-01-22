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
import L4.Annotation
import L4.Parser.SrcSpan (SrcPos(..), SrcRange(..))
import L4.Syntax (Resolved, Module, Unique, getUnique)
import L4.TypeCheck (toResolved)
import qualified L4.Utils.IntervalMap as IV

-- | A mapping from positions to definitions and from definitions to all their uses.
data ReferenceMapping = ReferenceMapping
  { actualToOriginal :: IV.IntervalMap SrcPos Unique
    -- ^ Getting the original occurrence of a name, based on its reference's source range
  , originalToActual :: Map Unique [SrcRange]
    -- ^ Getting the source range of all references of an original definition
  }
  deriving stock (Generic)

-- | NFData instance that doesn't force the IntervalMap deeply (it can be very large)
instance NFData ReferenceMapping where
  rnf ReferenceMapping{..} =
    actualToOriginal `seq` rnf originalToActual

instance Semigroup ReferenceMapping where
  a <> b = ReferenceMapping
    { actualToOriginal = a.actualToOriginal <> b.actualToOriginal
    , originalToActual = Map.unionWith (<>) a.originalToActual b.originalToActual
    }

instance Monoid ReferenceMapping where
  mempty = ReferenceMapping IV.empty Map.empty

-- | Create a singleton reference mapping
singletonReferenceMapping :: Unique -> SrcRange -> ReferenceMapping
singletonReferenceMapping originalName actualRange = ReferenceMapping
  { actualToOriginal = IV.singleton (IV.srcRangeToInterval actualRange) originalName
  , originalToActual = Map.singleton originalName [actualRange]
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

-- | Look up all references for the symbol at the given position.
lookupReference :: SrcPos -> ReferenceMapping -> [SrcRange]
lookupReference pos mapping = do
  (_, n) <- IV.search pos mapping.actualToOriginal
  maybe [] id $ Map.lookup n mapping.originalToActual

-- | Find all references to the symbol at the given position.
-- Returns a list of source ranges where the symbol is used.
findReferences :: SrcPos -> Module Resolved -> [SrcRange]
findReferences pos mod' =
  let mapping = buildReferenceMapping mod'
  in lookupReference pos mapping
