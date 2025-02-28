# JL4 language context-free grammar

We use the following meta-notation for
separated lists:
```
foo{sep}* ::=
    epsilon             -- the empty symbol
  | foo "sep" foo{sep}*
```
Most of these can be replaced using layout / indentation,
see further below.

```
program ::=
  anonymoussection section*

anonymoussection ::=
  topdecl{;}*

section ::=
  sectionsymbols name? aka? topdecl{;}*

sectionsymbols ::=
  '§'+

topdecl ::=
  declare | decide | assume | directive | import

import ::=
  "IMPORT" name

localdecl ::=
  decide | assume

declare ::=
  typesig declare'

givens ::=
  "GIVEN" param{,}*

param ::=
  name ("IS" type)?

reqparam ::=
  name "IS" type

article ::=
  "A" | "AN" | "THE"

declare' ::=
  "DECLARE" appform typedecl

typedecl ::=
  recorddecl | enumdecl | synonymdecl

recorddecl ::=
  "HAS" reqparam{,}*

enumdecl ::=
  "IS" "ONE" "OF"
    condecl{,}*

synonymdecl ::=
  "IS" type

condecl ::=
  name recorddecl?

-- ignoring operator priority
type ::=
    article? atype
  | "FOR" "ALL" name+{AND} article? type  -- quantified type
  | "(" type ")"

atype ::=
    "TYPE"
  | name+                                 -- type application
  | name "OF" tyargs                      -- also type application
  | name "WITH" namedtyargs               -- also type application [currently unsupported]
  | "FUNCTION" "FROM" funtyargs "TO" type -- function type

namedtyargs ::=
  namedtyarg{,}*

namedtyarg ::=
  name "IS" type

tyargs ::=
  type{,}+

funtyargs ::=
  type{AND}+

decide ::=
  typesig decide'

typesig ::=
  givens? giveth?

giveth ::=
  "GIVETH" type

decide' ::=
    "DECIDE" appform ("IS" | "IF") expr  -- all forms are equivalent
  | appform "MEANS" expr

appform ::=
    rawappform aka?

rawappform ::=
    name+
  | name "OF" nameargs
  -- possibly allow "WITH" here as well?

aka ::=
  "AKA" name{,}*

assume ::=
  typesig assume'

assume' ::=
  "ASSUME" appform ("IS" type)?

-- ignoring operator priority
expr ::=
    expr infixop expr
  | name+                              -- constructor / function application
  | name "OF" args                     -- also constructor / function application
  | name "WITH" namedargs              -- also constructor / function application
  | givens "YIELD" expr                -- anonymous function / lambda
  | expr "'s" name                     -- record projection
  | "THE" name "OF" expr               -- also record projection (under discussion / should perhaps become generally possible as application syntax)
  | "IF" expr "THEN" expr "ELSE" expr  -- elimination for Booleans
  | "CONSIDER" expr branches           -- elimination for general datatypes
  | expr "WHERE" localdecls            -- local declarations
  | "LIST" expr{,}*                    -- literal lists
  | "NOT" expr                         -- negation [should probably be predefined, not built-in]
  | ...                                -- numeric literals (at least integers)
  | ...                                -- string literals (in double quotes)

-- All operators have a textual form, but some may also have
-- a symbolic form. These have to be built-in for now (not *necessarily*
-- keywords), because operators currently cannot be user-defined.
infixop ::=
    "AND" | "&&"
  | "OR" | "||"
  | "IMPLIES" | "=>"
  | "PLUS" | "+"
  | "MINUS" | "-"
  | "TIMES" | "*"
  | "DIVIDED" "BY" | "/"
  | "EQUALS" | "="
  | "GREATER" "THAN" | "ABOVE" | ">"
  | "LESS" "THAN" | "BELOW" | "<"
  | "AT" "LEAST" | ">="
  | "AT" "MOST" | "<="
  | "FOLLOWED" "BY"     -- "cons" on lists
  | ...                 -- we will probably add more operators

branches ::=
  branch{,}*

branch ::=
    "WHEN" pattern "THEN" expr
  | "OTHERWISE" expr

pattern ::=
  | name+
  | pattern "FOLLOWED" "BY" pattern  (the only infix constructor we have right now)
  | name "OF" patargs
  | name "WITH" namedpatargs

namedargs ::=
  namedarg{,}*

namedarg ::=
  name "IS" article? expr

args ::=
  expr{,}+

nameargs ::=
  name{,}+

namedpatargs ::=
  namedpatarg{,}*

namedpatarg ::=
  name "IS" article? pattern

patargs ::=
  pattern{,}+

localdecls ::=
  localdecl{;}*

directive ::=
    "#EVAL" expr
  | "#CHECK" expr
  | ...
```

# Some remarks on ambiguity and layout

```
DECLARE FOO IS ONE OF con1 HAS x IS A BOOL, y IS A NUMBER, con2
```

Disambiguation with layout:
```
DECLARE FOO IS ONE OF
  con1 HAS
    x IS A BOOL
    y IS A NUMBER
  con2
```

Disambiguation with parentheses:

There are in principle two options:
```
DECLARE FOO IS ONE OF (con1 HAS x IS A BOOL, y IS A NUMBER), con2
DECLARE FOO IS ONE OF con1 HAS (x IS A BOOL, y IS A NUMBER), con2
```
We will go for the first option.

Annotations are treated just as lexical comments.

Line comments start with `--`.
Block comments start with `{-` and end with `-}`.

# Layout

Principle 1:

Every use of `{...}` in the grammar can be replaced with
layout, basically putting every element of the sequence on
a new line and omitting the separator.

Principle 2:

Do something sensible with layout of infix operators for
expressions. (I know this is not a real spec yet.)

# Annotations

We have initially three kinds of annotations:

- general comments,
- NLG hints,
- and references.

For NLG hints, we allow to write them as a comment
starting with `@nlg` or within square brackets.

For references, we allow to write them as a comment
starting with `@ref` or within double angle brackets.

Annotations for specific entities refer to whatever
precedes them.
