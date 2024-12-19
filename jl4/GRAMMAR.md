# JL4 language context-free grammar

-- foo{sep}* is short for essentially sepBy
-- deferred: annotations

program ::=
  topdecl{;}*

topdecl ::=
  declare | binding | assumption | directive

declare ::=
  givens? declare'

givens ::=
  "GIVEN" param{,}*

param ::=
  name ("IS" article? type)?

reqparam ::=
  name "IS" article? type

article ::=
  "A" | "AN" | "THE"

declare' ::=
  "DECLARE" name+ (recorddecl | enumdecl)

recorddecl ::=
  "HAS" reqparam{,}*

enumdecl ::=
  "IS" "ONE" "OF"
    condecl{,}*

condecl ::=
  name recorddecl?

-- modulo operator priority
type ::=
    "TYPE"
  | name+       (type application)
  | name "OF" tyargs
  | FUNCTION FROM tyargs TO type
    -- probably not needed:
  | "BOOL"
  | "NUMBER"
  | "STRING"
  | "LIST"
  | "OPTIONAL"

tyargs ::=
  type{AND}+

binding ::=
  givens? giveth? binding'

giveth ::=
  "GIVETH" type

binding' ::=
    "DECIDE" name+ ("IS" | "IF") expr
  | name+ "MEANS" expr

assumption ::=
  givens? assumption'

assumption' ::=
  "ASSUME" name+ "IS" article? type

-- module operator priority
expr ::=
    expr infixop expr
  | name+                 (function application)
  | name "OF" args        (also function application)
  | name "WITH" namedargs (ADT construction / function application)
  | givens "YIELD" expr   (lambda)
  | expr "'s" name        (record projection)
  | "THE" name "OF" expr  (also record projection, under discussion)
  | "IF" expr "THEN" expr "ELSE" expr
  | "CONSIDER" expr branches
  | expr "WHERE" decls
  | article? "LIST" ("OF"?) expr{,}*
    -- probably not needed:
  | "EMPTY"
  | "MISSING"
  | "JUST"

branches ::=
  branch{,}*

branch ::=
    "WHEN" pattern "THEN" expr
  | "OTHERWISE" expr

namedargs ::=
  namedarg{,}*

namedarg ::=
  name "IS" article? expr
  
args ::=
  expr{AND}+

decls ::=
  binding{;}*

directive ::=
    "#EVAL" expr
  | "#CHECK" expr
  | ...


# Some remarks on ambiguity and layout

DECLARE FOO IS ONE OF con1 HAS x IS A BOOL, y IS A NUMBER, con2

Disambiguation with layout:

DECLARE FOO IS ONE OF
  con1 HAS
    x IS A BOOL
    y IS A NUMBER
  con2

Disambiguation with parentheses:

There are in principle two options:

DECLARE FOO IS ONE OF (con1 HAS x IS A BOOL, y IS A NUMBER), con2
DECLARE FOO IS ONE OF con1 HAS (x IS A BOOL, y IS A NUMBER), con2

We will go for the first option.

Annotations are treated just as lexical comments.

Line comments start with "--".
Block comments start with "/-" and end with "-/".

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
starting with "@nlg" or within square brackets.

For references, we allow to write them as a comment
starting with "@ref" or within angle brackets.

Annotations for specific entities refer to whatever
precedes them.
