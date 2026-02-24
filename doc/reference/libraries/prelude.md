## Prelude Libarary

The prelude can be imported into every L4 program with `IMPORT prelude` and provides foundational functions for working with lists, Maybe types, Booleans, and more.

### Location

[jl4-core/libraries/prelude.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4)

### Key Functions

#### List Functions

**Construction and Deconstruction:**

- `null` - Check if list is empty
- `reverse` - Reverse a list
- `replicate` - Create list of n copies
- `range` - Generate numeric range

**Transformation:**

- `map` - Apply function to each element
- `filter` - Keep elements matching predicate
- `count` - Count number of elements in a list
- `take` - First n elements
- `drop` - All but first n elements
- `takeWhile` / `dropWhile` - Conditional take/drop

**Combination:**

- `append` - Concatenate two lists
- `concat` - Flatten list of lists
- `zip` / `zipWith` - Combine two lists
- `partition` - Split by predicate

**Aggregation:**

- `foldr` / `foldl` - Fold (reduce) a list
- `sum` - Sum of numbers
- `product` - Product of numbers
- `maximum` / `minimum` - Largest/smallest element
- `and` / `or` - Logical aggregation
- `all` / `any` - Check if all/any satisfy predicate

**Searching:**

- `elem` - Check if element is in list
- `at` - Get element at index
- `lookup` - Find value by key in association list

**Sorting:**

- `sort` - Sort numbers
- `sortBy` - Sort with custom comparator
- `insertBy` - Insert maintaining order

**Uniqueness:**

- `nub` / `nubBy` - Remove duplicates
- `delete` / `deleteBy` - Remove element

#### Maybe Functions

- `isJust` / `isNothing` - Check Maybe status
- `fromMaybe` - Extract with default
- `maybe` - Fold over Maybe
- `orElse` - Alternative Maybe
- `mapMaybe` - Filter map
- `catMaybes` - Extract all JUST values
- `asum` / `firstJust` - First successful Maybe
- `maybeToList` / `listToMaybe` - Convert between Maybe and List

#### Either Functions

- `either` - Fold over Either

#### Pair Functions

- `pmap` / `mapSnd` - Map over second element
- `fmap` / `mapPairs` - Map over list of pairs

#### Dictionary Functions

**Construction:**

- `emptyDict` - Create empty dictionary
- `singleton` / `singleToDict` - Single key-value entry
- `pairToDict` - From pair
- `listToDict` / `fromList` - From association list
- `fromListGrouped` - Group values by key

**Query:**

- `dictLookup` - Find value by key
- `dictMember` / `dictNotMember` - Check key existence
- `dictFindWithDefault` - Lookup with default
- `dictKeys` - All keys
- `dictElems` - All values
- `dictToList` - Convert to association list
- `dictSize` - Number of entries
- `dictIsEmpty` - Check if empty

**Modification:**

- `dictInsert` - Add or update entry
- `dictInsertWith` - Insert with combining function
- `dictDelete` - Remove entry
- `dictAdjust` - Modify value at key
- `dictUpdate` - Modify or delete

**Combination:**

- `dictUnion` - Merge dictionaries
- `dictUnionWith` - Merge with combining function

**Higher-Order:**

- `mapDict` - Map over values
- `dictMapWithKey` - Map with key access
- `filterDict` - Filter by value predicate
- `dictFilterWithKey` - Filter by key-value predicate
- `foldrDict` / `foldlDict` - Fold over values
- `dictFoldrWithKey` / `dictFoldlWithKey` - Fold with keys

**Grouping:**

- `insertValue` - Insert into grouped pairs
- `groupPairs` - Group flat pairs by key

#### Utility Functions

- `id` - Identity function
- `const` - Constant function
- `even` / `odd` - Number parity
- `max` / `min` - Maximum/minimum of two values
- `TBD` - Polymorphic placeholder

### Example: Using Prelude Functions

[prelude-example.l4](prelude-example.l4)

**See [prelude.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4) source for all functions.**
