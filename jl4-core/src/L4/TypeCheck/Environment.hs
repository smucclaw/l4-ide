{-# LANGUAGE TemplateHaskell #-}
-- | Build the initial environment used during scope and type checking.
module L4.TypeCheck.Environment where

import qualified Base.Map as Map
import L4.Annotation
import L4.Syntax
import L4.TypeCheck.Types
import L4.TypeCheck.Environment.TH

-- NOTE: this adds names, uniques, defining and referencing occurences of an identifier as a declaration.
-- It is important, that this function is only called here, once, for a specific environment, because
-- otherwise the uniques will clash.
-- If you need a new unq/ref/def, just add its name here and a new declaration group will be added to this
-- module.
-- You can use `rename` to change the name of the value, while keeping the name of the declaration that you want.
-- This is useful if the value name you're thinking of is either very long, or already chosen by another declaration.
-- (e.g. if you want to pun the term and type levels)
mkBuiltins
  [ "boolean"
  , "false"
  , "true"
  , "number"
  , "string"
  , "keyword"  -- For mixfix keyword placeholders
  , "list"
  , "empty"
  , "maybe" `rename` "MAYBE"
  , "nothing" `rename` "NOTHING"
  , "just" `rename` "JUST"
  , "contract" `rename` "PROVISION"
  , "fulfil" `rename` "FULFILLED"
  , "evalContract" `rename` "EVALTRACE"
  , "event"
  , "eventC" `rename` "EVENT"
  , "isInteger" `rename` "IS INTEGER"
  , "floor" `rename` "FLOOR"
  , "ceiling" `rename` "CEILING"
  , "round" `rename` "ROUND"
  , "waitUntil" `rename`  "WAIT UNTIL"
  , "fetch" `rename` "FETCH"
  , "env" `rename` "ENV"
  , "post" `rename` "POST"
  , "jsonEncode" `rename` "JSONENCODE"
  , "jsonDecode" `rename` "JSONDECODE"
  , "a'" `rename` "a", "b'" `rename` "b"
  , "plus" `rename` "__PLUS__"
  , "minus" `rename` "__MINUS__"
  , "times" `rename` "__TIMES__"
  , "divide" `rename` "__DIVIDE__"
  , "modulo" `rename` "__MODULO__"
  , "lt" `rename` "__LT__" `variants` ["Number", "String", "Bool"]
  , "leq" `rename` "__LEQ__" `variants` ["Number", "String", "Bool"]
  , "gt" `rename` "__GT__" `variants` ["Number", "String", "Bool"]
  , "geq" `rename` "__GEQ__" `variants` ["Number", "String", "Bool"]
  , "and" `rename` "__AND__"
  , "or" `rename` "__OR__"
  , "implies" `rename` "__IMPLIES__"
  , "not" `rename` "__NOT__"
  , "cons" `rename` "__CONS__"
  , "equals" `rename` "__EQUALS__"
  , "a", "b", "c", "d", "g", "h", "i"
  -- NOTE: these are NOT supposed to be in the initial environment for the type/scope checker
  -- this is essentially the event that's always leading to a breach - it is only relevant
  -- for its timestamp
  , "neverMatchesParty", "neverMatchesAct"
  , "underscore" `rename` "_"
  -- String functions (unary)
  , "stringLength" `rename` "STRINGLENGTH"
  , "toUpper" `rename` "TOUPPER"
  , "toLower" `rename` "TOLOWER"
  , "trim" `rename` "TRIM"
  -- String functions (binary)
  , "contains" `rename` "CONTAINS"
  , "startsWith" `rename` "STARTSWITH"
  , "endsWith" `rename` "ENDSWITH"
  , "indexOf" `rename` "INDEXOF"
  -- String functions (ternary)
  , "substring" `rename` "SUBSTRING"
  , "replace" `rename` "REPLACE"
  ]

boolean :: Type' Resolved
boolean = app booleanRef []

-- NUMBER

number :: Type' Resolved
number = app numberRef []

-- STRING

string :: Type' Resolved
string = TyApp emptyAnno stringRef []

-- KEYWORD (for mixfix keyword placeholders)
keyword :: Type' Resolved
keyword = TyApp emptyAnno keywordRef []

-- LIST

list :: Type' Resolved -> Type' Resolved
list a = app listRef [a]

-- MAYBE

maybeType :: Type' Resolved -> Type' Resolved
maybeType a = app maybeRef [a]

-- PROVISION

contract :: Type' Resolved -> Type' Resolved -> Type' Resolved
contract party action = TyApp emptyAnno contractRef [party, action]

-- Number conversion

isInteger :: Type' Resolved
isInteger = fun_ [number] boolean

roundBuiltin :: Type' Resolved
roundBuiltin = fun_ [number] number

ceilingBuiltin :: Type' Resolved
ceilingBuiltin = fun_ [number] number

floorBuiltin :: Type' Resolved
floorBuiltin = fun_ [number] number

fetchBuiltin :: Type' Resolved
fetchBuiltin = fun_ [string] string

envBuiltin :: Type' Resolved
envBuiltin = fun_ [string] (maybeType string)

postBuiltin :: Type' Resolved
postBuiltin = fun_ [string, string, string] string

jsonEncodeBuiltin :: Type' Resolved
jsonEncodeBuiltin = forall' [aDef] $ fun_ [a] string
  where
    a = app aRef []

jsonDecodeBuiltin :: Type' Resolved
jsonDecodeBuiltin = forall' [aDef] $ fun_ [string] (maybeType a)
  where
    a = app aRef []

-- Basic Arithmetic

plusBuiltin :: Type' Resolved
plusBuiltin = binOpFun

minusBuiltin :: Type' Resolved
minusBuiltin = binOpFun

timesBuiltin :: Type' Resolved
timesBuiltin = binOpFun

divideBuiltin :: Type' Resolved
divideBuiltin = binOpFun

moduloBuiltin :: Type' Resolved
moduloBuiltin = binOpFun

binOpFun :: Type' Resolved
binOpFun = fun_ [number, number] number

ltBuiltins :: [Type' Resolved]
ltBuiltins = compBuiltins

leqBuiltins :: [Type' Resolved]
leqBuiltins = compBuiltins

gtBuiltins :: [Type' Resolved]
gtBuiltins = compBuiltins

geqBuiltins :: [Type' Resolved]
geqBuiltins = compBuiltins

-- Order of types must match the order in the '*Builtins'
ltUniques :: [Unique]
ltUniques = [ltNumberUnique,  ltStringUnique,  ltBoolUnique]

-- Order of types must match the order in the '*Builtins'
leqUniques :: [Unique]
leqUniques = [leqNumberUnique, leqStringUnique, leqBoolUnique]

-- Order of types must match the order in the '*Builtins'
gtUniques :: [Unique]
gtUniques = [gtNumberUnique,  gtStringUnique,  gtBoolUnique]

-- Order of types must match the order in the '*Builtins'
geqUniques :: [Unique]
geqUniques = [geqNumberUnique, geqStringUnique, geqBoolUnique]

-- Order of types must match the order in the '*Builtins'
compBuiltins :: [Type' Resolved]
compBuiltins = [fun_ [ty, ty] boolean | ty <- [number, string, boolean]]

andBuiltin :: Type' Resolved
andBuiltin = fun_ [boolean, boolean] boolean

orBuiltin :: Type' Resolved
orBuiltin = fun_ [boolean, boolean] boolean

impliesBuiltin :: Type' Resolved
impliesBuiltin = fun_ [boolean, boolean] boolean

notBuiltin :: Type' Resolved
notBuiltin = fun_ [boolean] boolean

consBuiltin :: Type' Resolved
consBuiltin = forall' [aDef] $ fun_ [a, list a] (list a)
  where
    a = app aRef []

equalsBuiltin :: Type' Resolved
equalsBuiltin = forall' [aDef] $ fun_ [a, a] boolean
  where
    a = app aRef []

-- String functions

-- Unary: STRING → NUMBER
stringLengthBuiltin :: Type' Resolved
stringLengthBuiltin = fun_ [string] number

-- Unary: STRING → STRING
toUpperBuiltin :: Type' Resolved
toUpperBuiltin = fun_ [string] string

toLowerBuiltin :: Type' Resolved
toLowerBuiltin = fun_ [string] string

trimBuiltin :: Type' Resolved
trimBuiltin = fun_ [string] string

-- Binary: STRING → STRING → BOOLEAN
containsBuiltin :: Type' Resolved
containsBuiltin = fun_ [string, string] boolean

startsWithBuiltin :: Type' Resolved
startsWithBuiltin = fun_ [string, string] boolean

endsWithBuiltin :: Type' Resolved
endsWithBuiltin = fun_ [string, string] boolean

-- Binary: STRING → STRING → NUMBER
indexOfBuiltin :: Type' Resolved
indexOfBuiltin = fun_ [string, string] number

-- Ternary: STRING → NUMBER → NUMBER → STRING
substringBuiltin :: Type' Resolved
substringBuiltin = fun_ [string, number, number] string

-- Ternary: STRING → STRING → STRING → STRING
replaceBuiltin :: Type' Resolved
replaceBuiltin = fun_ [string, string, string] string

-- infos

booleanInfo :: CheckEntity
booleanInfo =
  KnownType 0 [] Nothing

falseInfo :: CheckEntity
falseInfo =
  KnownTerm boolean Constructor

trueInfo :: CheckEntity
trueInfo =
  KnownTerm boolean Constructor

numberInfo :: CheckEntity
numberInfo =
  KnownType 0 [] Nothing

rationalInfo :: CheckEntity
rationalInfo =
  KnownType 0 [] Nothing

stringInfo :: CheckEntity
stringInfo =
  KnownType 0 [] Nothing

-- KEYWORD (for mixfix keyword placeholders)
keywordInfo :: CheckEntity
keywordInfo =
  KnownType 0 [] Nothing

listInfo :: CheckEntity
listInfo =
  KnownType 1 [aDef] Nothing

emptyInfo :: CheckEntity
emptyInfo =
  KnownTerm (forall' [aDef] (list (app aRef []))) Constructor

maybeInfo :: CheckEntity
maybeInfo =
  KnownType 1 [aDef] Nothing

nothingInfo :: CheckEntity
nothingInfo =
  KnownTerm (forall' [aDef] (maybeType (app aRef []))) Constructor

justInfo :: CheckEntity
justInfo =
  KnownTerm (forall' [aDef] (fun_ [app aRef []] (maybeType (app aRef [])))) Constructor

-- Number conversion

isIntegerInfo :: CheckEntity
isIntegerInfo =
  KnownTerm isInteger Computable

roundInfo :: CheckEntity
roundInfo =
  KnownTerm roundBuiltin Computable

ceilingInfo :: CheckEntity
ceilingInfo =
  KnownTerm ceilingBuiltin Computable

floorInfo :: CheckEntity
floorInfo =
  KnownTerm floorBuiltin Computable

fetchInfo :: CheckEntity
fetchInfo =
  KnownTerm fetchBuiltin Computable

envInfo :: CheckEntity
envInfo =
  KnownTerm envBuiltin Computable

postInfo :: CheckEntity
postInfo =
  KnownTerm postBuiltin Computable

jsonEncodeInfo :: CheckEntity
jsonEncodeInfo =
  KnownTerm jsonEncodeBuiltin Computable

jsonDecodeInfo :: CheckEntity
jsonDecodeInfo =
  KnownTerm jsonDecodeBuiltin Computable

-- Basic Arithmetic

plusInfo :: CheckEntity
plusInfo =
  KnownTerm plusBuiltin Computable

minusInfo :: CheckEntity
minusInfo =
  KnownTerm minusBuiltin Computable

timesInfo :: CheckEntity
timesInfo =
  KnownTerm timesBuiltin Computable

divideInfo :: CheckEntity
divideInfo =
  KnownTerm divideBuiltin Computable

moduloInfo :: CheckEntity
moduloInfo =
  KnownTerm moduloBuiltin Computable

-- Boolean

andInfo :: CheckEntity
andInfo = KnownTerm andBuiltin Computable

orInfo :: CheckEntity
orInfo = KnownTerm orBuiltin Computable

impliesInfo :: CheckEntity
impliesInfo = KnownTerm impliesBuiltin Computable

notInfo :: CheckEntity
notInfo = KnownTerm notBuiltin Computable

consInfo :: CheckEntity
consInfo = KnownTerm consBuiltin Computable

equalsInfo :: CheckEntity
equalsInfo = KnownTerm equalsBuiltin Computable

-- String functions

stringLengthInfo :: CheckEntity
stringLengthInfo = KnownTerm stringLengthBuiltin Computable

toUpperInfo :: CheckEntity
toUpperInfo = KnownTerm toUpperBuiltin Computable

toLowerInfo :: CheckEntity
toLowerInfo = KnownTerm toLowerBuiltin Computable

trimInfo :: CheckEntity
trimInfo = KnownTerm trimBuiltin Computable

containsInfo :: CheckEntity
containsInfo = KnownTerm containsBuiltin Computable

startsWithInfo :: CheckEntity
startsWithInfo = KnownTerm startsWithBuiltin Computable

endsWithInfo :: CheckEntity
endsWithInfo = KnownTerm endsWithBuiltin Computable

indexOfInfo :: CheckEntity
indexOfInfo = KnownTerm indexOfBuiltin Computable

substringInfo :: CheckEntity
substringInfo = KnownTerm substringBuiltin Computable

replaceInfo :: CheckEntity
replaceInfo = KnownTerm replaceBuiltin Computable

-- Comparison

ltInfos :: [CheckEntity]
ltInfos = [KnownTerm ltBuiltin Computable | ltBuiltin <- ltBuiltins]

leqInfos :: [CheckEntity]
leqInfos = [KnownTerm leqBuiltin Computable | leqBuiltin <- leqBuiltins]

gtInfos :: [CheckEntity]
gtInfos = [KnownTerm gtBuiltin Computable | gtBuiltin <- gtBuiltins]

geqInfos :: [CheckEntity]
geqInfos = [KnownTerm gtBuiltin Computable | gtBuiltin <- geqBuiltins]

-- Contract

contractInfo :: CheckEntity
contractInfo =
  KnownType 2 [] Nothing

-- forall a b. PROVISION a b
fulfilInfo :: CheckEntity
fulfilInfo = KnownTerm (Forall emptyAnno [hDef, iDef] (TyApp emptyAnno contractRef [TyApp emptyAnno hRef [], TyApp emptyAnno iRef []])) Constructor

event :: Type' Resolved -> Type' Resolved -> Type' Resolved
event party action = TyApp emptyAnno eventRef [party, action]

eventInfo, eventCInfo :: CheckEntity
eventInfo = KnownType 2 [] Nothing
eventCInfo = KnownTerm (Forall emptyAnno [dDef, gDef] (Fun emptyAnno [mkOnt partyTy, mkOnt actTy, mkOnt number] (TyApp emptyAnno eventRef [partyTy, actTy]))) Constructor
  where
  actTy = mkTyVar gRef
  partyTy = mkTyVar dRef
  mkTyVar x = TyApp emptyAnno x []
  mkOnt = MkOptionallyNamedType emptyAnno Nothing

-- forall a b. PROVISION a b -> NUMBER -> [Event a b] -> PROVISION a b
evalContractInfo :: CheckEntity
evalContractInfo = KnownTerm (Forall emptyAnno [bDef, cDef] (Fun emptyAnno [mkOnt ctrct, mkOnt number, mkOnt (list eventTy)] ctrct)) Computable
  where
  ctrct = contract (mkTyVar bRef) (mkTyVar cRef)
  eventTy = TyApp emptyAnno eventRef [mkTyVar bRef, mkTyVar cRef]
  mkOnt = MkOptionallyNamedType emptyAnno Nothing
  mkTyVar r = TyApp emptyAnno r []

-- forall a b. NUMBER -> EVENT a b
waitUntilInfo :: CheckEntity
waitUntilInfo = KnownTerm (forall' [a'Def, b'Def] (fun [MkOptionallyNamedType emptyAnno Nothing number] $ event (tyvar a'Ref) (tyvar b'Ref))) Computable
  where tyvar r = TyApp emptyAnno r []

initialEnvironment :: Environment
initialEnvironment =
  Map.fromList
    [ (rawName booleanName,      [booleanUnique     ])
    , (rawName falseName,        [falseUnique       ])
    , (rawName trueName,         [trueUnique        ])
    , (rawName numberName,       [numberUnique      ])
    , (rawName stringName,       [stringUnique      ])
    , (rawName listName,         [listUnique        ])
    , (rawName emptyName,        [emptyUnique       ])
    , (rawName maybeName,        [maybeUnique       ])
    , (rawName nothingName,      [nothingUnique     ])
    , (rawName justName,         [justUnique        ])
    , (rawName contractName,     [contractUnique    ])
    , (rawName eventName,        [eventUnique       ])
    , (rawName eventCName,       [eventCUnique      ])
    , (rawName evalContractName, [evalContractUnique])
    , (rawName fulfilName,       [fulfilUnique      ])
    , (rawName isIntegerName,    [isIntegerUnique ])
    , (rawName roundName,        [roundUnique     ])
    , (rawName ceilingName,      [ceilingUnique   ])
    , (rawName floorName,        [floorUnique     ])
    , (rawName fetchName,        [fetchUnique     ])
    , (rawName envName,          [envUnique       ])
    , (rawName jsonEncodeName,   [jsonEncodeUnique])
    , (rawName jsonDecodeName,   [jsonDecodeUnique])
    , (rawName plusName,         [plusUnique      ])
    , (rawName minusName,        [minusUnique     ])
    , (rawName timesName,        [timesUnique     ])
    , (rawName divideName,       [divideUnique    ])
    , (rawName moduloName,       [moduloUnique    ])
    , (rawName ltName,           ltUniques)
    , (rawName leqName,          leqUniques)
    , (rawName gtName,           gtUniques)
    , (rawName geqName,          geqUniques)
    , (rawName andName,          [andUnique       ])
    , (rawName orName,           [orUnique        ])
    , (rawName impliesName,      [impliesUnique   ])
    , (rawName notName,          [notUnique       ])
    , (rawName consName,         [consUnique      ])
    , (rawName equalsName,       [equalsUnique    ])
    , (rawName waitUntilName,    [waitUntilUnique])
    -- String functions
    , (rawName stringLengthName, [stringLengthUnique])
    , (rawName toUpperName,      [toUpperUnique     ])
    , (rawName toLowerName,      [toLowerUnique     ])
    , (rawName trimName,         [trimUnique        ])
    , (rawName containsName,     [containsUnique    ])
    , (rawName startsWithName,   [startsWithUnique  ])
    , (rawName endsWithName,     [endsWithUnique    ])
    , (rawName indexOfName,      [indexOfUnique     ])
    , (rawName substringName,    [substringUnique   ])
    , (rawName replaceName,      [replaceUnique     ])
    ]
      -- NOTE: we currently do not include the Cons constructor because it has special syntax

initialEntityInfo :: EntityInfo
initialEntityInfo =
  Map.fromList $
    [ (booleanUnique,      (booleanName,      booleanInfo     ))
    , (falseUnique,        (falseName,        falseInfo       ))
    , (trueUnique,         (trueName,         trueInfo        ))
    , (numberUnique,       (numberName,       numberInfo      ))
    , (stringUnique,       (stringName,       stringInfo      ))
    , (listUnique,         (listName,         listInfo        ))
    , (emptyUnique,        (emptyName,        emptyInfo       ))
    , (maybeUnique,        (maybeName,        maybeInfo       ))
    , (nothingUnique,      (nothingName,      nothingInfo     ))
    , (justUnique,         (justName,         justInfo        ))
    , (contractUnique,     (contractName,     contractInfo    ))
    , (eventUnique,        (eventName,        eventInfo       ))
    , (eventCUnique,       (eventCName,       eventCInfo      ))
    , (evalContractUnique, (evalContractName, evalContractInfo))
    , (fulfilUnique,       (fulfilName,       fulfilInfo      ))
    , (isIntegerUnique,    (isIntegerName,    isIntegerInfo   ))
    , (roundUnique,        (roundName,        roundInfo       ))
    , (ceilingUnique,      (ceilingName,      ceilingInfo     ))
    , (floorUnique,        (floorName,        floorInfo       ))
    , (fetchUnique,        (fetchName,        fetchInfo       ))
    , (envUnique,          (envName,          envInfo         ))
    , (postUnique,         (postName,         postInfo        ))
    , (jsonEncodeUnique,   (jsonEncodeName,   jsonEncodeInfo  ))
    , (jsonDecodeUnique,   (jsonDecodeName,   jsonDecodeInfo  ))
    , (plusUnique,         (plusName,         plusInfo        ))
    , (minusUnique,        (minusName,        minusInfo       ))
    , (timesUnique,        (timesName,        timesInfo       ))
    , (divideUnique,       (divideName,       divideInfo      ))
    , (moduloUnique,       (moduloName,       moduloInfo      ))
    , (andUnique,          (andName,          andInfo         ))
    , (orUnique,           (orName,           orInfo          ))
    , (impliesUnique,      (impliesName,      impliesInfo     ))
    , (notUnique,          (notName,          notInfo         ))
    , (consUnique,         (consName,         consInfo        ))
    , (equalsUnique,       (equalsName,       equalsInfo      ))
    , (waitUntilUnique,    (waitUntilName,    waitUntilInfo   ))
    -- String functions
    , (stringLengthUnique, (stringLengthName, stringLengthInfo))
    , (toUpperUnique,      (toUpperName,      toUpperInfo     ))
    , (toLowerUnique,      (toLowerName,      toLowerInfo     ))
    , (trimUnique,         (trimName,         trimInfo        ))
    , (containsUnique,     (containsName,     containsInfo    ))
    , (startsWithUnique,   (startsWithName,   startsWithInfo  ))
    , (endsWithUnique,     (endsWithName,     endsWithInfo    ))
    , (indexOfUnique,      (indexOfName,      indexOfInfo     ))
    , (substringUnique,    (substringName,    substringInfo   ))
    , (replaceUnique,      (replaceName,      replaceInfo     ))
    ]
    <>
      [ (uniq, (name, info))
      | (name, uniqs, infos) <-
        [ (ltName , ltUniques , ltInfos )
        , (leqName, leqUniques, leqInfos)
        , (gtName , gtUniques , gtInfos )
        , (geqName, geqUniques, geqInfos)
        ]
      , (uniq, info) <- zip uniqs infos
      ]
