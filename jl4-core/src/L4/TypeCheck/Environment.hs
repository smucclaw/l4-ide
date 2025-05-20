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
  , "list"
  , "empty"
  , "contract"
  , "fulfil" `rename` "FULFILLED"
  , "evalContract"
  , "event"
  , "eventC" `rename` "EVENT"
  , "isInteger" `rename` "IS INTEGER"
  , "floor" `rename` "FLOOR"
  , "ceiling" `rename` "CEILING"
  , "round" `rename` "ROUND"
  , "plus" `rename` "__PLUS__"
  , "minus" `rename` "__MINUS__"
  , "times" `rename` "__TIMES__"
  , "divide" `rename` "__DIVIDE__"
  , "modulo" `rename` "__MODULO__"
  , "a", "b", "c", "d", "g", "h", "i"
  ]

boolean :: Type' Resolved
boolean = app booleanRef []

-- NUMBER

number :: Type' Resolved
number = app numberRef []

-- STRING

string :: Type' Resolved
string = TyApp emptyAnno stringRef []

-- LIST

list :: Type' Resolved -> Type' Resolved
list a = app listRef [a]

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

-- Basic Arithmetic

plusBuiltin :: Type' Resolved
plusBuiltin = fun_ [number, number] number

minusBuiltin :: Type' Resolved
minusBuiltin = fun_ [number, number] number

timesBuiltin :: Type' Resolved
timesBuiltin = fun_ [number, number] number

divideBuiltin :: Type' Resolved
divideBuiltin = fun_ [number, number] number

moduloBuiltin :: Type' Resolved
moduloBuiltin = fun_ [number, number] number

-- infos

-- Number conversion

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

listInfo :: CheckEntity
listInfo =
  KnownType 1 [aDef] Nothing

emptyInfo :: CheckEntity
emptyInfo =
  KnownTerm (forall' [aDef] (list (app aRef []))) Constructor

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
    , (rawName contractName,     [contractUnique    ])
    , (rawName eventName,        [eventUnique       ])
    , (rawName eventCName,       [eventCUnique      ])
    , (rawName evalContractName, [evalContractUnique])
    , (rawName fulfilName,       [fulfilUnique      ])
    , (rawName isIntegerName,    [isIntegerUnique ])
    , (rawName roundName,        [roundUnique     ])
    , (rawName ceilingName,      [ceilingUnique   ])
    , (rawName floorName,        [floorUnique     ])
    , (rawName plusName,         [plusUnique      ])
    , (rawName minusName,        [minusUnique     ])
    , (rawName timesName,        [timesUnique     ])
    , (rawName divideName,       [divideUnique    ])
    , (rawName moduloName,       [moduloUnique    ])
    ]
      -- NOTE: we currently do not include the Cons constructor because it has special syntax

initialEntityInfo :: EntityInfo
initialEntityInfo =
  Map.fromList
    [ (booleanUnique,      (booleanName,      booleanInfo     ))
    , (falseUnique,        (falseName,        falseInfo       ))
    , (trueUnique,         (trueName,         trueInfo        ))
    , (numberUnique,       (numberName,       numberInfo      ))
    , (stringUnique,       (stringName,       stringInfo      ))
    , (listUnique,         (listName,         listInfo        ))
    , (emptyUnique,        (emptyName,        emptyInfo       ))
    , (contractUnique,     (contractName,     contractInfo    ))
    , (eventUnique,        (eventName,        eventInfo       ))
    , (eventCUnique,       (eventCName,       eventCInfo      ))
    , (evalContractUnique, (evalContractName, evalContractInfo))
    , (fulfilUnique,       (fulfilName,       fulfilInfo      ))
    , (isIntegerUnique,    (isIntegerName,    isIntegerInfo  ))
    , (roundUnique,        (roundName,        roundInfo       ))
    , (ceilingUnique,      (ceilingName,      ceilingInfo     ))
    , (floorUnique,        (floorName,        floorInfo       ))
    , (plusUnique,         (plusName,         plusInfo        ))
    , (minusUnique,        (minusName,        minusInfo       ))
    , (timesUnique,        (timesName,        timesInfo       ))
    , (divideUnique,       (divideName,       divideInfo      ))
    , (moduloUnique,       (moduloName,       moduloInfo      ))
    ]
