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
  , "waitUntil" `rename`  "WAIT UNTIL"
  , "a'" `rename` "a", "b'" `rename` "b"
  , "a", "b", "c", "d", "g", "h", "i"
  -- NOTE: these are NOT supposed to be in the initial environment for the type/scope checker
  -- this is essentially the event that's always leading to a breach - it is only relevant
  -- for its timestamp
  , "neverMatchesParty", "neverMatchesAct"
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


-- infos

-- Number conversion

isInteger :: Type' Resolved
isInteger = fun_ [number] boolean

roundBuiltin :: Type' Resolved
roundBuiltin = fun_ [number] number

ceilingBuiltin :: Type' Resolved
ceilingBuiltin = fun_ [number] number

floorBuiltin :: Type' Resolved
floorBuiltin = fun_ [number] number

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
    [ (NormalName "BOOLEAN",      [booleanUnique     ])
    , (NormalName "FALSE",        [falseUnique       ])
    , (NormalName "TRUE",         [trueUnique        ])
    , (NormalName "NUMBER",       [numberUnique      ])
    , (NormalName "STRING",       [stringUnique      ])
    , (NormalName "LIST",         [listUnique        ])
    , (NormalName "EMPTY",        [emptyUnique       ])
    , (NormalName "PROVISION",    [contractUnique    ])
    , (NormalName "EVENT",        [eventUnique       ])
    , (NormalName "EVENT",        [eventCUnique      ])
    , (NormalName "EVALTRACE",[evalContractUnique])
    , (NormalName "FULFILLED",    [fulfilUnique      ])
    , (NormalName "IS INTEGER",   [isIntegerUnique ])
    , (NormalName "ROUND",        [roundUnique     ])
    , (NormalName "CEILING",      [ceilingUnique   ])
    , (NormalName "FLOOR",        [floorUnique     ])
    , (NormalName "WAIT UNTIL",   [waitUntilUnique])
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
    , (waitUntilUnique,    (waitUntilName,    waitUntilInfo   ))
    ]
