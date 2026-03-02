# L4 Standard Library and Builtins

Complete index of builtin operators, builtin functions, and standard library functions.

---

## Builtin Operators (No Import Needed)

### Arithmetic

| Operator | Alias | Type | Haskell |
|----------|-------|------|---------|
| `+` | `PLUS` | `NUMBER -> NUMBER -> NUMBER` | `(+)` |
| `-` | `MINUS` | `NUMBER -> NUMBER -> NUMBER` | `(-)` |
| `*` | `TIMES` | `NUMBER -> NUMBER -> NUMBER` | `(*)` |
| `/` | `DIVIDED BY` | `NUMBER -> NUMBER -> NUMBER` | `div` / `(/)` |
| `MODULO` | - | `NUMBER -> NUMBER -> NUMBER` | `mod` |
| `EXPONENT` | - | `NUMBER -> NUMBER -> NUMBER` | `(**)` |
| `FLOOR` | - | `NUMBER -> NUMBER` | `floor` |
| `CEILING` | - | `NUMBER -> NUMBER` | `ceiling` |
| `TRUNC` | - | `NUMBER -> NUMBER` | `truncate` |

### Comparison

| Operator | Alias | Type | Haskell |
|----------|-------|------|---------|
| `=` | `EQUALS` | `a -> a -> BOOLEAN` | `(==)` |
| `>` | `GREATER THAN` | `a -> a -> BOOLEAN` | `(>)` |
| `<` | `LESS THAN` | `a -> a -> BOOLEAN` | `(<)` |
| `>=` | `AT LEAST` | `a -> a -> BOOLEAN` | `(>=)` |
| `<=` | `AT MOST` | `a -> a -> BOOLEAN` | `(<=)` |

**Note:** `=` is equality, NOT assignment. L4 has no assignment (pure functional).

### Boolean

| Operator | Alias | Precedence |
|----------|-------|------------|
| `NOT` | - | Highest |
| `AND` | `&&` | High |
| `OR` | `\|\|` | Medium |
| `IMPLIES` | `=>` | Lowest |
| `UNLESS` | - | = `AND NOT` |

### String

| Operator | Type | Description |
|----------|------|-------------|
| `CONCAT x, y, ...` | `STRING -> ... -> STRING` | Concatenate multiple strings |
| `CONTAINS` | `STRING -> STRING -> BOOLEAN` | Substring test |
| `STARTS WITH` | `STRING -> STRING -> BOOLEAN` | Prefix test |
| `ENDS WITH` | `STRING -> STRING -> BOOLEAN` | Suffix test |
| `INDEXOF` | `STRING -> STRING -> NUMBER` | Find position |
| `SPLIT` | `STRING -> STRING -> LIST OF STRING` | Split by delimiter |
| `CHARAT` | `STRING -> NUMBER -> STRING` | Character at index |
| `SUBSTRING` | `STRING -> NUMBER -> NUMBER -> STRING` | Substring(str, start, len) |
| `STRINGLENGTH` | `STRING -> NUMBER` | String length |
| `TOUPPER` | `STRING -> STRING` | Uppercase |
| `TOLOWER` | `STRING -> STRING` | Lowercase |
| `APPEND` (infix) | `STRING -> STRING -> STRING` | Binary concat (prelude). **Caution:** lowercase `append` is a different function for lists — see prelude list operations. |

### Type Coercion

| Function | Type | Description |
|----------|------|-------------|
| `TOSTRING(x)` | `a -> STRING` | Convert NUMBER/BOOLEAN/DATE/TIME/DATETIME to STRING |
| `TODATE(s)` | `STRING -> DATE` | Parse "YYYY-MM-DD" |
| `TOTIME(s)` | `STRING -> TIME` | Parse "HH:MM:SS" or "HH:MM" |
| `TODATETIME(s)` | `STRING -> DATETIME` | Parse ISO-8601 |

### List Construction

| Syntax | Description | Haskell |
|--------|-------------|---------|
| `LIST x, y, z` | Literal list | `[x, y, z]` |
| `EMPTY` | Empty list | `[]` |
| `x FOLLOWED BY xs` | Cons | `x : xs` |

### Nullary Builtins

| Name | Type | Description |
|------|------|-------------|
| `TODAY` | `DATE` | Current date in document timezone |
| `TIMEZONE` | `STRING` | Document timezone (IANA name) |

---

## prelude (`IMPORT prelude`)

### List Functions

| Function | Type (simplified) | Haskell | Description |
|----------|-------------------|---------|-------------|
| `null list` | `[a] -> Bool` | `null` | Is list empty? |
| `count list` | `[a] -> Number` | `length` | List length |
| `map f list` | `(a->b) -> [a] -> [b]` | `map` | Transform each element |
| `filter f list` | `(a->Bool) -> [a] -> [a]` | `filter` | Keep matching elements |
| `foldr f z list` | `(a->r->r) -> r -> [a] -> r` | `foldr` | Right fold |
| `foldl f z list` | `(r->a->r) -> r -> [a] -> r` | `foldl` | Left fold |
| `append l1 l2` | `[a] -> [a] -> [a]` | `(++)` | Concatenate lists. **Caution:** uppercase `APPEND` is a different function for strings. |
| `concat lists` | `[[a]] -> [a]` | `concat` | Flatten list of lists |
| `reverse list` | `[a] -> [a]` | `reverse` | Reverse list |
| `at list i` | `[a] -> Number -> a` | `(!!)` | Index access (0-based) |
| `take n list` | `Number -> [a] -> [a]` | `take` | First n elements |
| `drop n list` | `Number -> [a] -> [a]` | `drop` | Drop first n elements |
| `takeWhile f list` | `(a->Bool) -> [a] -> [a]` | `takeWhile` | Take while predicate holds |
| `dropWhile f list` | `(a->Bool) -> [a] -> [a]` | `dropWhile` | Drop while predicate holds |
| `replicate n x` | `Number -> a -> [a]` | `replicate` | n copies of x |
| `range low high` | `Number -> Number -> [Number]` | `[low..high]` | Number range |
| `elem x list` | `a -> [a] -> Bool` | `elem` | Membership test |
| `sort list` | `[Number] -> [Number]` | `sort` | Sort numbers ascending |
| `sortBy cmp list` | `(a->a->Bool) -> [a] -> [a]` | `sortBy` | Sort by comparator |
| `zip l1 l2` | `[a] -> [b] -> [Pair a b]` | `zip` | Pair up elements |
| `zipWith f l1 l2` | `(a->b->c) -> [a] -> [b] -> [c]` | `zipWith` | Combine pairwise |
| `nub list` | `[a] -> [a]` | `nub` | Remove duplicates |
| `nubBy eq list` | `(a->a->Bool) -> [a] -> [a]` | `nubBy` | Remove duplicates by equality |
| `delete x list` | `a -> [a] -> [a]` | `delete` | Remove first occurrence |
| `deleteBy eq x list` | `(a->a->Bool) -> a -> [a] -> [a]` | `deleteBy` | Remove by equality |
| `partition f list` | `(a->Bool) -> [a] -> Pair [a] [a]` | `partition` | Split by predicate |
| `insertBy cmp x list` | `(a->a->Bool) -> a -> [a] -> [a]` | `insertBy` | Insert maintaining order |

### Boolean Quantifiers

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `all f list` | `(a->Bool) -> [a] -> Bool` | `all` | All satisfy predicate? |
| `any f list` | `(a->Bool) -> [a] -> Bool` | `any` | Any satisfy predicate? |
| `and list` | `[Bool] -> Bool` | `and` | All true? |
| `or list` | `[Bool] -> Bool` | `or` | Any true? |

### Numeric

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `sum list` | `[Number] -> Number` | `sum` | Sum of numbers |
| `product list` | `[Number] -> Number` | `product` | Product of numbers |
| `max x y` | `Number -> Number -> Number` | `max` | Maximum of two |
| `min x y` | `Number -> Number -> Number` | `min` | Minimum of two |
| `maximum list` | `[Number] -> Number` | `maximum` | Maximum of list |
| `minimum list` | `[Number] -> Number` | `minimum` | Minimum of list |
| `even x` | `Number -> Bool` | `even` | Is even? |
| `odd x` | `Number -> Bool` | `odd` | Is odd? |

### Maybe (Optional) Functions

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `fromMaybe default x` | `a -> Maybe a -> a` | `fromMaybe` | Unwrap with default |
| `maybe f z x` | `(a->b) -> b -> Maybe a -> b` | `maybe` | Fold over Maybe |
| `orElse x y` | `Maybe a -> Maybe a -> Maybe a` | `(<\|>)` | First non-Nothing |
| `isJust x` | `Maybe a -> Bool` | `isJust` | Is Just? |
| `isNothing x` | `Maybe a -> Bool` | `isNothing` | Is Nothing? |
| `maybeToList x` | `Maybe a -> [a]` | `maybeToList` | Maybe to 0-or-1 list |
| `listToMaybe list` | `[a] -> Maybe a` | `listToMaybe` | Head or Nothing |
| `mapMaybe f list` | `(a -> Maybe b) -> [a] -> [b]` | `mapMaybe` | Filter-map |
| `catMaybes list` | `[Maybe a] -> [a]` | `catMaybes` | Collect Justs |
| `asum list` | `[Maybe a] -> Maybe a` | `asum` | First Just in list |
| `firstJust list` | `[Maybe a] -> Maybe a` | - | Alias for asum |

### Either Functions

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `either left right x` | `(a->c) -> (b->c) -> Either a b -> c` | `either` | Fold over Either |

**Pattern match:** `WHEN LEFT x THEN ...` / `WHEN RIGHT y THEN ...`

### Pair Functions

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `PAIR OF x, y` | Constructor | `(x, y)` | Create pair |
| `p's fst` | `Pair a b -> a` | `fst` | First element |
| `p's snd` | `Pair a b -> b` | `snd` | Second element |
| `pmap f p` | `(b->c) -> Pair a b -> Pair a c` | `fmap` on snd | Map over second |

### Utility

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `id x` | `a -> a` | `id` | Identity function |
| `const x y` | `a -> b -> a` | `const` | Always return first |
| `TBD` | `a` | `undefined` | Placeholder (crashes) |

### Dictionary (Map / HashMap / Associative Array)

**Type:** `Dictionary k v` — association list of key-value pairs
**Backed by:** `LIST OF PAIR OF k, v`

| Function | Type | Haskell (Data.Map) | Description |
|----------|------|--------------------|-------------|
| `emptyDict` | `Dictionary k v` | `empty` | Empty dictionary |
| `singleton k v` / `singleToDict` | `k -> v -> Dictionary k v` | `singleton` | One-entry dict |
| `listToDict pairs` / `fromList` | `[Pair k v] -> Dictionary k v` | `fromList` | Create from pairs |
| `dictToList dict` | `Dictionary k v -> [Pair k v]` | `toList` | Extract pairs |
| `dictKeys dict` | `Dictionary k v -> [k]` | `keys` | All keys |
| `dictElems dict` | `Dictionary k v -> [v]` | `elems` | All values |
| `dictSize dict` | `Dictionary k v -> Number` | `size` | Entry count |
| `dictIsEmpty dict` | `Dictionary k v -> Bool` | `null` | Is empty? |
| `dictLookup key dict` | `k -> Dictionary k v -> Maybe v` | `lookup` | Query by key |
| `dictMember key dict` | `k -> Dictionary k v -> Bool` | `member` | Key exists? |
| `dictNotMember key dict` | `k -> Dictionary k v -> Bool` | `notMember` | Key absent? |
| `dictFindWithDefault def key dict` | `v -> k -> Dictionary k v -> v` | `findWithDefault` | Lookup with default |
| `dictInsert key val dict` | `k -> v -> Dictionary k v -> Dictionary k v` | `insert` | Add/replace entry |
| `dictInsertWith f key val dict` | `(v->v->v) -> k -> v -> Dict -> Dict` | `insertWith` | Insert with combiner |
| `dictDelete key dict` | `k -> Dictionary k v -> Dictionary k v` | `delete` | Remove entry |
| `dictAdjust f key dict` | `(v->v) -> k -> Dict -> Dict` | `adjust` | Modify at key |
| `dictUpdate f key dict` | `(v->Maybe v) -> k -> Dict -> Dict` | `update` | Modify or delete |
| `dictUnion d1 d2` | `Dict -> Dict -> Dict` | `union` | Left-biased merge |
| `dictUnionWith f d1 d2` | `(v->v->v) -> Dict -> Dict -> Dict` | `unionWith` | Merge with combiner |
| `mapDict f dict` | `(v->w) -> Dict k v -> Dict k w` | `map` | Map over values |
| `dictMapWithKey f dict` | `(k->v->w) -> Dict k v -> Dict k w` | `mapWithKey` | Map with key access |
| `filterDict f dict` | `(v->Bool) -> Dict -> Dict` | `filter` | Filter by value |
| `dictFilterWithKey f dict` | `(k->v->Bool) -> Dict -> Dict` | `filterWithKey` | Filter by key+value |
| `foldrDict f z dict` | `(v->r->r) -> r -> Dict -> r` | `foldr` | Right fold values |
| `foldlDict f z dict` | `(r->v->r) -> r -> Dict -> r` | `foldl` | Left fold values |
| `dictFoldrWithKey f z dict` | `(k->v->r->r) -> r -> Dict -> r` | `foldrWithKey` | Right fold with keys |
| `dictFoldlWithKey f z dict` | `(r->k->v->r) -> r -> Dict -> r` | `foldlWithKey` | Left fold with keys |
| `fromListGrouped pairs` | `[Pair k v] -> Dict k [v]` | - | Group by key |
| `groupPairs pairs` | `[Pair k v] -> [Pair k [v]]` | - | Group flat pairs |
| `lookup key assocList` | `k -> [Pair k v] -> Maybe v` | `lookup` | Raw assoc list lookup |

---

## math (`IMPORT math`)

Requires: `IMPORT prelude`

| Function | Type | Haskell | Description |
|----------|------|---------|-------------|
| `EULER` | `NUMBER` | - | Euler's number (2.718...) |
| `exp x` | `NUMBER -> NUMBER` | `exp` | e^x |
| `ln x` | `NUMBER -> MAYBE NUMBER` | `log` | Natural logarithm (Nothing if x <= 0) |
| `log10 x` | `NUMBER -> MAYBE NUMBER` | `logBase 10` | Base-10 logarithm |
| `sqrt x` | `NUMBER -> MAYBE NUMBER` | `sqrt` | Square root (Nothing if x < 0) |
| `sin x` | `NUMBER -> MAYBE NUMBER` | `sin` | Sine |
| `cos x` | `NUMBER -> MAYBE NUMBER` | `cos` | Cosine |
| `tan x` | `NUMBER -> MAYBE NUMBER` | `tan` | Tangent (Nothing near pi/2) |
| `asin x` | `NUMBER -> MAYBE NUMBER` | `asin` | Arcsine (domain [-1,1]) |
| `acos x` | `NUMBER -> MAYBE NUMBER` | `acos` | Arccosine (domain [-1,1]) |
| `atan x` | `NUMBER -> MAYBE NUMBER` | `atan` | Arctangent |
| `absNumber x` | `NUMBER -> NUMBER` | `abs` | Absolute value |

**Note:** Trig functions return MAYBE to handle domain errors safely.

---

## daydate (`IMPORT daydate`)

Date arithmetic library. ISO 8601 conventions.

### Constants

`Monday`(1)..`Sunday`(0), `January`(1)..`December`(12), `Days in a week`(7), `Days in a year`(365.2425), `Days in a month`(30.44)

### Constructors

| Function | Type | Description |
|----------|------|-------------|
| `Date day month year` | `NUMBER -> NUMBER -> NUMBER -> DATE` | From d/m/y |
| `Date days` | `NUMBER -> DATE` | From serial (days since epoch) |
| `Day date` | `DATE -> NUMBER` | Date to serial number |
| `Day day month year` | `NUMBER -> NUMBER -> NUMBER -> NUMBER` | d/m/y to serial |
| `Year year` | `NUMBER -> DATE` | Jan 1 of year |
| `Month month year` | `NUMBER -> NUMBER -> DATE` | 1st of month |
| `Week week year` | `NUMBER -> NUMBER -> DATE` | Monday of ISO week |
| `January day year` | `NUMBER -> NUMBER -> DATE` | Convenience (all 12 months) |

### Date Queries

| Function | Description |
|----------|-------------|
| `Weekday of date/days` | Day of week (0=Sun, 1=Mon...) |
| `Week of the year date` | ISO week number |
| `Month of the year date` | Month number |
| `Days in month month year` | Days in that month |
| `Days in year year` | 365 or 366 |
| `is weekend date` | Saturday or Sunday? |
| `is weekday date` | Monday through Friday? |
| `is leap year year` | Leap year? |
| `Name of month date` | "January" etc. |
| `Name of weekday date` | "Monday" etc. |

### Relative Dates

| Function | Description |
|----------|-------------|
| `the day after date` | +1 day |
| `the day before date` | -1 day |
| `the week after date` | Next Monday |
| `the week before date` | Previous Monday |
| `the month after date` | 1st of next month |
| `the month before date` | 1st of previous month |
| `the year after date` | Jan 1 of next year |
| `the year before date` | Jan 1 of previous year |
| `the earlier of date1 date2` | min(date1, date2) |
| `the later of date1 date2` | max(date1, date2) |

### Date Arithmetic (Operator Overloads)

```l4
date + n           -- Add n days
date - n           -- Subtract n days
date1 - date2      -- Difference in days (NUMBER)
date1 >= date2     -- Comparison
```

---

## time (`IMPORT time`)

Wall-clock time of day (no date, no timezone). 24-hour format.

### Constants

`Midnight` (00:00:00), `Noon` (12:00:00), `Seconds in a minute` (60), `Minutes in an hour` (60), `Hours in a day` (24)

### Constructors

| Function | Description |
|----------|-------------|
| `Time h m s` | From hours, minutes, seconds |
| `Time h m` | From hours, minutes (s=0) |
| `Time h` | From hours only |
| `Time h m s am/pm` | 12-hour format with meridiem |

### Extractors

`the hour of t`, `the minute of t`, `the second of t`

### Arithmetic

`t \`plus hours\` n`, `t \`plus minutes\` n`, `t \`plus seconds\` n`, `t \`minus hours\` n`, `t \`minus minutes\` n`, `t \`minus seconds\` n`

### Predicates

`t \`is before noon\``, `t \`is after noon\``, `t \`is morning\``, `t \`is afternoon\``, `t \`is evening\``

### Comparators

`the earlier of t1 t2`, `the later of t1 t2`, `t1 >= t2`, `t1 - t2` (difference in seconds)

---

## datetime (`IMPORT datetime`)

Combined date + time + timezone. Stored internally as UTC.

### Constructors

| Function | Description |
|----------|-------------|
| `Datetime date time` | Using document TIMEZONE |
| `Datetime date time tz` | Explicit IANA timezone |
| `Datetime date h m s` | From date + h/m/s |
| `Datetime date` | Midnight in document TIMEZONE |

### Extractors

`Date of dt`, `Time of dt`, `Timezone of dt`, `the hour of dt`, `the minute of dt`, `the second of dt`, `the day of dt`, `the month of dt`, `the year of dt`

### Relative

`at midnight dt`, `at noon dt`, `dt \`at\` time`

### Comparators

`the earlier of dt1 dt2`, `the later of dt1 dt2`, `dt1 >= dt2`, `dt1 <= dt2`

---

## currency (`IMPORT currency`) — Prototype

ISO 4217 currency handling. Integer minor units (cents) to avoid floating-point. Key functions: `Money minorUnits code`, `major to minor units`, `add money`, `multiply money`. Supports USD, EUR, GBP, JPY, SGD, and others.

**Status:** Prototype. API may change.

---

## legal-persons (`IMPORT legal-persons`) — Prototype

Legal entity types, identity documents, capacity checks. Key functions: `age in years`, `is adult birthDate jurisdictionCode`, `can enter contract`, `is beneficial owner`. Includes corporate entity types and jurisdiction-aware majority age.

**Status:** Prototype. API may change.

---

## Search Terms

**Data structures:** list, array, dictionary, map, hashmap, associative array, pair, tuple, maybe, optional, either, result, set

**Operations:** map, filter, fold, reduce, sort, zip, concat, flatten, reverse, take, drop, slice, indexOf, contains, split, join, groupBy, partition, unique, distinct, deduplicate

**Math:** floor, ceiling, round, truncate, absolute value, power, exponent, logarithm, square root, trigonometry, sin, cos, tan

**Date/Time:** date arithmetic, add days, subtract days, date difference, day of week, week number, leap year, ISO 8601, timezone, UTC, midnight, noon, am/pm, meridiem

**Financial:** currency, money, cents, minor units, major units, ISO 4217, USD, EUR, exchange

**Legal:** legal person, natural person, corporate entity, age of majority, beneficial owner, jurisdiction, citizenship, incorporation
