# Dictionary Support in Prelude

## Status: Completed

## Problem

Dictionary support was duplicated across files:

- `jl4-core/libraries/prelude.l4` had basic dictionary type and construction
- `~/src/legalese/opm2l4/sample-l4/by-meng.l4` duplicated these and added grouping functions

This caused:

1. Code duplication
2. Inconsistent naming (`fmap` vs `fmapl`, `pmap` vs `fmapp`)
3. Missing standard operations that Haskell programmers expect

## Solution

### Naming Convention

All dictionary functions use `dict` prefix to avoid namespace conflicts (L4 lacks qualified imports). When L4 gains section-scoped imports, unprefixed names can be added.

Functions implemented:

**Construction:**

- `emptyDict` - empty dictionary
- `singleton` - alias for `singleToDict`
- `singleToDict k v` - single entry
- `pairToDict pair` - from single pair
- `listToDict pairs` / `fromList` - from association list
- `fromListGrouped pairs` - from list, grouping values by key

**Query:**

- `dictLookup key dict` - lookup returning MAYBE
- `dictMember key dict` - test if key exists
- `dictNotMember key dict` - test if key doesn't exist
- `dictFindWithDefault default key dict` - lookup with default

**Conversion:**

- `dictToList dict` - extract contents
- `dictKeys dict` - list of keys
- `dictElems dict` - list of values
- `dictSize dict` - count entries
- `dictIsEmpty dict` - test if empty

**Modification:**

- `dictInsert key value dict` - add/replace entry
- `dictInsertWith combiner key value dict` - insert with combining function
- `dictDelete key dict` - remove entry
- `dictAdjust f key dict` - modify value at key
- `dictUpdate f key dict` - modify or delete via MAYBE

**Combination:**

- `dictUnion dict1 dict2` - left-biased union
- `dictUnionWith combiner dict1 dict2` - union with combining function

**Higher-order:**

- `mapDict f dict` - map over values
- `dictMapWithKey f dict` - map with key access
- `filterDict pred dict` - filter by value predicate
- `dictFilterWithKey pred dict` - filter by key-value predicate
- `foldrDict f z dict` / `foldlDict f z dict` - folds over values
- `dictFoldrWithKey f z dict` / `dictFoldlWithKey f z dict` - folds with keys

**Grouping utilities:**

- `insertValue key value pairs` - insert into grouped assoc list
- `groupPairs pairs` - group `[(k,v)]` into `[(k,[v])]`

**Pair operations:**

- `pmap f pair` - map over second element of pair (Functor)
- `fmap f pairs` - map over second element of each pair in list
- `mapSnd` - alias for `pmap`
- `mapPairs` - alias for `fmap` on pairs

## Changes to by-meng.l4

1. Removed all duplicate type/function definitions
2. Uses prelude functions (`Dictionary`, `groupPairs`, `fmap`, etc.)
3. Fixed `wrapper for 2e` return type (`MAYBE` → `EITHER STRING`)

## Testing

All 402 tests pass. The prelude golden file was regenerated.
