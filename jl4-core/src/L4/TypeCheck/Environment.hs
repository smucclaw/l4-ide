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
  , "date" `rename` "DATE"
  , "string"
  , "keyword"  -- For mixfix keyword placeholders
  , "list"
  , "empty"
  , "maybe" `rename` "MAYBE"
  , "nothing" `rename` "NOTHING"
  , "just" `rename` "JUST"
  , "either" `rename` "EITHER"
  , "left" `rename` "LEFT"
  , "right" `rename` "RIGHT"
  , "contract" `rename` "DEONTIC"
  , "fulfil" `rename` "FULFILLED"
  , "evalContract" `rename` "EVALTRACE"
  , "event"
  , "eventC" `rename` "EVENT"
  , "isInteger" `rename` "IS INTEGER"
  , "floor" `rename` "FLOOR"
  , "ceiling" `rename` "CEILING"
  , "round" `rename` "ROUND"
  , "sqrt" `rename` "SQRT"
  , "ln" `rename` "LN"
  , "log10" `rename` "LOG10"
  , "sin" `rename` "SIN"
  , "cos" `rename` "COS"
  , "tan" `rename` "TAN"
  , "asin" `rename` "ASIN"
  , "acos" `rename` "ACOS"
  , "atan" `rename` "ATAN"
  , "exponent" `rename` "EXPONENT"
  , "waitUntil" `rename`  "WAIT UNTIL"
  , "fetch" `rename` "FETCH"
  , "env" `rename` "ENV"
  , "post" `rename` "POST"
  , "jsonEncode" `rename` "JSONENCODE"
  , "jsonDecode" `rename` "JSONDECODE"
  , "todaySerial" `rename` "TODAY"
  , "nowSerial" `rename` "NOW"
  , "dateFromText" `rename` "DATEVALUE"
  , "dateSerial" `rename` "DATE_SERIAL"
  , "dateFromSerial" `rename` "DATE_FROM_SERIAL"
  , "dateFromDMY" `rename` "DATE_FROM_DMY"
  , "dateDay" `rename` "DATE_DAY"
  , "dateMonth" `rename` "DATE_MONTH"
  , "dateYear" `rename` "DATE_YEAR"
  , "timeValueFraction" `rename` "TIMEVALUE"
  , "everBetween" `rename` "EVER BETWEEN"
  , "alwaysBetween" `rename` "ALWAYS BETWEEN"
  , "whenLast" `rename` "WHEN LAST"
  , "whenNext" `rename` "WHEN NEXT"
  , "valueAt" `rename` "VALUE AT"
  , "evalAsOfSystemTime" `rename` "EVAL AS OF SYSTEM TIME"
  , "evalUnderValidTime" `rename` "EVAL UNDER VALID TIME"
  , "evalUnderRulesEffectiveAt" `rename` "EVAL UNDER RULES EFFECTIVE AT"
  , "evalUnderRulesEncodedAt" `rename` "EVAL UNDER RULES ENCODED AT"
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
  , "split" `rename` "SPLIT"
  , "charAt" `rename` "CHARAT"
  -- String functions (ternary)
  , "substring" `rename` "SUBSTRING"
  , "replace" `rename` "REPLACE"
  , "toString" `rename` "TOSTRING"
  , "toNumber" `rename` "TONUMBER"
  , "toDate" `rename` "TODATE"
  , "trunc" `rename` "TRUNC"
  ]

boolean :: Type' Resolved
boolean = app booleanRef []

-- NUMBER

number :: Type' Resolved
number = app numberRef []

-- DATE

date :: Type' Resolved
date = app dateRef []

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

-- EITHER

eitherType :: Type' Resolved -> Type' Resolved -> Type' Resolved
eitherType a b = app eitherRef [a, b]

-- DEONTIC

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

sqrtBuiltin :: Type' Resolved
sqrtBuiltin = fun_ [number] number

lnBuiltin :: Type' Resolved
lnBuiltin = fun_ [number] number

log10Builtin :: Type' Resolved
log10Builtin = fun_ [number] number

sinBuiltin :: Type' Resolved
sinBuiltin = fun_ [number] number

cosBuiltin :: Type' Resolved
cosBuiltin = fun_ [number] number

tanBuiltin :: Type' Resolved
tanBuiltin = fun_ [number] number

asinBuiltin :: Type' Resolved
asinBuiltin = fun_ [number] number

acosBuiltin :: Type' Resolved
acosBuiltin = fun_ [number] number

atanBuiltin :: Type' Resolved
atanBuiltin = fun_ [number] number

exponentBuiltin :: Type' Resolved
exponentBuiltin = binOpFun

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
jsonDecodeBuiltin = forall' [aDef] $ fun_ [string] (eitherType string a)
  where
    a = app aRef []

todayBuiltin :: Type' Resolved
todayBuiltin = date

nowBuiltin :: Type' Resolved
nowBuiltin = number

dateValueBuiltin :: Type' Resolved
dateValueBuiltin = fun_ [string] (eitherType string date)

dateSerialBuiltin :: Type' Resolved
dateSerialBuiltin = fun_ [date] number

dateFromSerialBuiltin :: Type' Resolved
dateFromSerialBuiltin = fun_ [number] date

dateFromDmyBuiltin :: Type' Resolved
dateFromDmyBuiltin = fun_ [number, number, number] date

dateDayBuiltin, dateMonthBuiltin, dateYearBuiltin :: Type' Resolved
dateDayBuiltin = fun_ [date] number
dateMonthBuiltin = fun_ [date] number
dateYearBuiltin = fun_ [date] number

timeValueBuiltin :: Type' Resolved
timeValueBuiltin = fun_ [string] (eitherType string number)

datePredicate :: Type' Resolved
datePredicate = fun_ [date] boolean

everBetweenBuiltin, alwaysBetweenBuiltin :: Type' Resolved
everBetweenBuiltin = fun_ [date, date, datePredicate] boolean
alwaysBetweenBuiltin = fun_ [date, date, datePredicate] boolean

whenLastBuiltin, whenNextBuiltin :: Type' Resolved
whenLastBuiltin = fun_ [date, datePredicate] (maybeType date)
whenNextBuiltin = fun_ [date, datePredicate] (maybeType date)

valueAtBuiltin :: Type' Resolved
valueAtBuiltin =
  forall' [aDef] $
    fun_ [date, fun_ [date] (app aRef [])] (app aRef [])

-- EVAL under alternate system time (serial-based). Type: NUMBER -> a -> a
evalAsOfSystemTimeBuiltin :: Type' Resolved
evalAsOfSystemTimeBuiltin =
  forall' [aDef] $
    fun_ [number, app aRef []] (app aRef [])

-- EVAL under valid time (serial-based). Type: NUMBER -> a -> a
evalUnderValidTimeBuiltin :: Type' Resolved
evalUnderValidTimeBuiltin =
  forall' [aDef] $
    fun_ [number, app aRef []] (app aRef [])

-- EVAL under rules effective date. Type: DATE SERIAL -> a -> a
evalUnderRulesEffectiveAtBuiltin :: Type' Resolved
evalUnderRulesEffectiveAtBuiltin =
  forall' [aDef] $
    fun_ [number, app aRef []] (app aRef [])

-- EVAL under rules encoded date. Type: DATE SERIAL -> a -> a
evalUnderRulesEncodedAtBuiltin :: Type' Resolved
evalUnderRulesEncodedAtBuiltin =
  forall' [aDef] $
    fun_ [number, app aRef []] (app aRef [])

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

-- Binary: STRING → STRING → LIST OF STRING
splitBuiltin :: Type' Resolved
splitBuiltin = fun_ [string, string] (list string)

-- Binary: STRING → NUMBER → STRING
charAtBuiltin :: Type' Resolved
charAtBuiltin = fun_ [string, number] string

-- Ternary: STRING → NUMBER → NUMBER → STRING
substringBuiltin :: Type' Resolved
substringBuiltin = fun_ [string, number, number] string

-- Ternary: STRING → STRING → STRING → STRING
replaceBuiltin :: Type' Resolved
replaceBuiltin = fun_ [string, string, string] string

toStringBuiltin :: Type' Resolved
toStringBuiltin = forall' [aDef] $ fun_ [app aRef []] string

toNumberBuiltin :: Type' Resolved
toNumberBuiltin = fun_ [string] (maybeType number)

toDateBuiltin :: Type' Resolved
toDateBuiltin = forall' [aDef] $ fun_ [string] (maybeType (app aRef []))

truncBuiltin :: Type' Resolved
truncBuiltin = fun_ [number, number] number

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

dateInfo :: CheckEntity
dateInfo =
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

eitherInfo :: CheckEntity
eitherInfo =
  KnownType 2 [aDef, bDef] Nothing

leftInfo :: CheckEntity
leftInfo =
  KnownTerm (forall' [aDef, bDef] (fun_ [app aRef []] (eitherType (app aRef []) (app bRef [])))) Constructor

rightInfo :: CheckEntity
rightInfo =
  KnownTerm (forall' [aDef, bDef] (fun_ [app bRef []] (eitherType (app aRef []) (app bRef [])))) Constructor

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

sqrtInfo :: CheckEntity
sqrtInfo =
  KnownTerm sqrtBuiltin Computable

lnInfo :: CheckEntity
lnInfo =
  KnownTerm lnBuiltin Computable

log10Info :: CheckEntity
log10Info =
  KnownTerm log10Builtin Computable

sinInfo :: CheckEntity
sinInfo =
  KnownTerm sinBuiltin Computable

cosInfo :: CheckEntity
cosInfo =
  KnownTerm cosBuiltin Computable

tanInfo :: CheckEntity
tanInfo =
  KnownTerm tanBuiltin Computable

asinInfo :: CheckEntity
asinInfo =
  KnownTerm asinBuiltin Computable

acosInfo :: CheckEntity
acosInfo =
  KnownTerm acosBuiltin Computable

atanInfo :: CheckEntity
atanInfo =
  KnownTerm atanBuiltin Computable

exponentInfo :: CheckEntity
exponentInfo =
  KnownTerm exponentBuiltin Computable

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

todayInfo :: CheckEntity
todayInfo = KnownTerm todayBuiltin Computable

nowInfo :: CheckEntity
nowInfo = KnownTerm nowBuiltin Computable

dateFromTextInfo :: CheckEntity
dateFromTextInfo = KnownTerm dateValueBuiltin Computable

dateSerialInfo, dateFromSerialInfo, dateFromDmyInfo :: CheckEntity
dateSerialInfo = KnownTerm dateSerialBuiltin Computable
dateFromSerialInfo = KnownTerm dateFromSerialBuiltin Computable
dateFromDmyInfo = KnownTerm dateFromDmyBuiltin Computable

dateDayInfo, dateMonthInfo, dateYearInfo :: CheckEntity
dateDayInfo = KnownTerm dateDayBuiltin Computable
dateMonthInfo = KnownTerm dateMonthBuiltin Computable
dateYearInfo = KnownTerm dateYearBuiltin Computable

timeValueInfo :: CheckEntity
timeValueInfo = KnownTerm timeValueBuiltin Computable

everBetweenInfo, alwaysBetweenInfo :: CheckEntity
everBetweenInfo = KnownTerm everBetweenBuiltin Computable
alwaysBetweenInfo = KnownTerm alwaysBetweenBuiltin Computable

whenLastInfo, whenNextInfo :: CheckEntity
whenLastInfo = KnownTerm whenLastBuiltin Computable
whenNextInfo = KnownTerm whenNextBuiltin Computable

valueAtInfo :: CheckEntity
valueAtInfo = KnownTerm valueAtBuiltin Computable

evalAsOfSystemTimeInfo :: CheckEntity
evalAsOfSystemTimeInfo = KnownTerm evalAsOfSystemTimeBuiltin Computable

evalUnderValidTimeInfo :: CheckEntity
evalUnderValidTimeInfo = KnownTerm evalUnderValidTimeBuiltin Computable

evalUnderRulesEffectiveAtInfo :: CheckEntity
evalUnderRulesEffectiveAtInfo = KnownTerm evalUnderRulesEffectiveAtBuiltin Computable

evalUnderRulesEncodedAtInfo :: CheckEntity
evalUnderRulesEncodedAtInfo = KnownTerm evalUnderRulesEncodedAtBuiltin Computable

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

splitInfo :: CheckEntity
splitInfo = KnownTerm splitBuiltin Computable

charAtInfo :: CheckEntity
charAtInfo = KnownTerm charAtBuiltin Computable

substringInfo :: CheckEntity
substringInfo = KnownTerm substringBuiltin Computable

replaceInfo :: CheckEntity
replaceInfo = KnownTerm replaceBuiltin Computable

toStringInfo :: CheckEntity
toStringInfo = KnownTerm toStringBuiltin Computable

toNumberInfo :: CheckEntity
toNumberInfo = KnownTerm toNumberBuiltin Computable

toDateInfo :: CheckEntity
toDateInfo = KnownTerm toDateBuiltin Computable

truncInfo :: CheckEntity
truncInfo = KnownTerm truncBuiltin Computable

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

-- forall a b. DEONTIC a b
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

-- forall a b. DEONTIC a b -> NUMBER -> [Event a b] -> DEONTIC a b
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
    , (rawName dateName,         [dateUnique        ])
    , (rawName stringName,       [stringUnique      ])
    , (rawName listName,         [listUnique        ])
    , (rawName emptyName,        [emptyUnique       ])
    , (rawName maybeName,        [maybeUnique       ])
    , (rawName nothingName,      [nothingUnique     ])
    , (rawName justName,         [justUnique        ])
    , (rawName eitherName,       [eitherUnique      ])
    , (rawName leftName,         [leftUnique        ])
    , (rawName rightName,        [rightUnique       ])
    , (rawName contractName,     [contractUnique    ])
    , (NormalName "PROVISION",   [contractUnique    ])  -- Deprecated alias for DEONTIC
    , (rawName eventName,        [eventUnique       ])
    , (rawName eventCName,       [eventCUnique      ])
    , (rawName evalContractName, [evalContractUnique])
    , (rawName fulfilName,       [fulfilUnique      ])
    , (rawName isIntegerName,    [isIntegerUnique ])
    , (rawName roundName,        [roundUnique     ])
    , (rawName ceilingName,      [ceilingUnique   ])
    , (rawName floorName,        [floorUnique     ])
    , (rawName sqrtName,         [sqrtUnique      ])
    , (rawName lnName,           [lnUnique        ])
    , (rawName log10Name,        [log10Unique     ])
    , (rawName sinName,          [sinUnique       ])
    , (rawName cosName,          [cosUnique       ])
    , (rawName tanName,          [tanUnique       ])
    , (rawName asinName,         [asinUnique      ])
    , (rawName acosName,         [acosUnique      ])
    , (rawName atanName,         [atanUnique      ])
    , (rawName exponentName,     [exponentUnique  ])
    , (rawName fetchName,        [fetchUnique     ])
    , (rawName envName,          [envUnique       ])
    , (rawName jsonEncodeName,   [jsonEncodeUnique])
    , (rawName jsonDecodeName,   [jsonDecodeUnique])
    , (rawName todaySerialName,  [todaySerialUnique])
    , (rawName nowSerialName,    [nowSerialUnique])
    , (rawName dateFromTextName, [dateFromTextUnique])
    , (rawName dateSerialName,   [dateSerialUnique])
    , (rawName dateFromSerialName, [dateFromSerialUnique])
    , (rawName dateFromDMYName,  [dateFromDMYUnique])
    , (rawName dateDayName,      [dateDayUnique])
    , (rawName dateMonthName,    [dateMonthUnique])
    , (rawName dateYearName,     [dateYearUnique])
    , (rawName timeValueFractionName, [timeValueFractionUnique])
    , (rawName everBetweenName,  [everBetweenUnique])
    , (rawName alwaysBetweenName, [alwaysBetweenUnique])
    , (rawName whenLastName,     [whenLastUnique])
    , (rawName whenNextName,     [whenNextUnique])
    , (rawName valueAtName,      [valueAtUnique])
    , (rawName evalAsOfSystemTimeName, [evalAsOfSystemTimeUnique])
    , (rawName evalUnderValidTimeName, [evalUnderValidTimeUnique])
    , (rawName evalUnderRulesEffectiveAtName, [evalUnderRulesEffectiveAtUnique])
    , (rawName evalUnderRulesEncodedAtName, [evalUnderRulesEncodedAtUnique])
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
    , (rawName splitName,        [splitUnique       ])
    , (rawName charAtName,       [charAtUnique      ])
    , (rawName substringName,    [substringUnique   ])
    , (rawName replaceName,      [replaceUnique     ])
    , (rawName toStringName,     [toStringUnique    ])
    , (rawName toNumberName,     [toNumberUnique    ])
    , (rawName toDateName,       [toDateUnique      ])
    , (rawName truncName,        [truncUnique       ])
    ]
      -- NOTE: we currently do not include the Cons constructor because it has special syntax

initialEntityInfo :: EntityInfo
initialEntityInfo =
  Map.fromList $
    [ (booleanUnique,      (booleanName,      booleanInfo     ))
    , (falseUnique,        (falseName,        falseInfo       ))
    , (trueUnique,         (trueName,         trueInfo        ))
    , (numberUnique,       (numberName,       numberInfo      ))
    , (dateUnique,         (dateName,         dateInfo        ))
    , (stringUnique,       (stringName,       stringInfo      ))
    , (listUnique,         (listName,         listInfo        ))
    , (emptyUnique,        (emptyName,        emptyInfo       ))
    , (maybeUnique,        (maybeName,        maybeInfo       ))
    , (nothingUnique,      (nothingName,      nothingInfo     ))
    , (justUnique,         (justName,         justInfo        ))
    , (eitherUnique,       (eitherName,       eitherInfo      ))
    , (leftUnique,         (leftName,         leftInfo        ))
    , (rightUnique,        (rightName,        rightInfo       ))
    , (contractUnique,     (contractName,     contractInfo    ))
    , (eventUnique,        (eventName,        eventInfo       ))
    , (eventCUnique,       (eventCName,       eventCInfo      ))
    , (evalContractUnique, (evalContractName, evalContractInfo))
    , (fulfilUnique,       (fulfilName,       fulfilInfo      ))
    , (isIntegerUnique,    (isIntegerName,    isIntegerInfo   ))
    , (roundUnique,        (roundName,        roundInfo       ))
    , (ceilingUnique,      (ceilingName,      ceilingInfo     ))
    , (floorUnique,        (floorName,        floorInfo       ))
    , (sqrtUnique,         (sqrtName,         sqrtInfo        ))
    , (lnUnique,           (lnName,           lnInfo          ))
    , (log10Unique,        (log10Name,        log10Info       ))
    , (sinUnique,          (sinName,          sinInfo         ))
    , (cosUnique,          (cosName,          cosInfo         ))
    , (tanUnique,          (tanName,          tanInfo         ))
    , (asinUnique,         (asinName,         asinInfo        ))
    , (acosUnique,         (acosName,         acosInfo        ))
    , (atanUnique,         (atanName,         atanInfo        ))
    , (exponentUnique,     (exponentName,     exponentInfo    ))
    , (fetchUnique,        (fetchName,        fetchInfo       ))
    , (envUnique,          (envName,          envInfo         ))
    , (postUnique,         (postName,         postInfo        ))
    , (jsonEncodeUnique,   (jsonEncodeName,   jsonEncodeInfo  ))
    , (jsonDecodeUnique,   (jsonDecodeName,   jsonDecodeInfo  ))
    , (todaySerialUnique,  (todaySerialName,  todayInfo       ))
    , (nowSerialUnique,    (nowSerialName,    nowInfo         ))
    , (dateFromTextUnique, (dateFromTextName, dateFromTextInfo))
    , (dateSerialUnique,   (dateSerialName,   dateSerialInfo  ))
    , (dateFromSerialUnique, (dateFromSerialName, dateFromSerialInfo))
    , (dateFromDMYUnique,  (dateFromDMYName,  dateFromDmyInfo))
    , (dateDayUnique,      (dateDayName,      dateDayInfo))
    , (dateMonthUnique,    (dateMonthName,    dateMonthInfo))
    , (dateYearUnique,     (dateYearName,     dateYearInfo))
    , (timeValueFractionUnique, (timeValueFractionName, timeValueInfo))
    , (everBetweenUnique,  (everBetweenName,  everBetweenInfo))
    , (alwaysBetweenUnique, (alwaysBetweenName, alwaysBetweenInfo))
    , (whenLastUnique,     (whenLastName,     whenLastInfo))
    , (whenNextUnique,     (whenNextName,     whenNextInfo))
    , (valueAtUnique,      (valueAtName,      valueAtInfo))
    , (evalAsOfSystemTimeUnique, (evalAsOfSystemTimeName, evalAsOfSystemTimeInfo))
    , (evalUnderValidTimeUnique, (evalUnderValidTimeName, evalUnderValidTimeInfo))
    , (evalUnderRulesEffectiveAtUnique, (evalUnderRulesEffectiveAtName, evalUnderRulesEffectiveAtInfo))
    , (evalUnderRulesEncodedAtUnique, (evalUnderRulesEncodedAtName, evalUnderRulesEncodedAtInfo))
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
    , (splitUnique,        (splitName,        splitInfo       ))
    , (charAtUnique,       (charAtName,       charAtInfo      ))
    , (substringUnique,    (substringName,    substringInfo   ))
    , (replaceUnique,      (replaceName,      replaceInfo     ))
    , (toStringUnique,     (toStringName,     toStringInfo    ))
    , (toNumberUnique,     (toNumberName,     toNumberInfo    ))
    , (toDateUnique,       (toDateName,       toDateInfo      ))
    , (truncUnique,        (truncName,        truncInfo       ))
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
