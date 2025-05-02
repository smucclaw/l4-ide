{-# OPTIONS_GHC -ddump-splices #-}
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
  , "a", "b", "c", "d", "e", "f", "g", "h", "i", "f'"
  ]

boolean :: Type' Resolved
boolean = TyApp emptyAnno booleanRef []

-- NUMBER

number :: Type' Resolved
number = TyApp emptyAnno numberRef []

-- STRING

string :: Type' Resolved
string = TyApp emptyAnno stringRef []

-- LIST

list :: Type' Resolved -> Type' Resolved
list a = TyApp emptyAnno listRef [a]

-- CONTRACT

contract :: Type' Resolved -> Type' Resolved -> Type' Resolved
contract party action = TyApp emptyAnno contractRef [party, action]


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

stringInfo :: CheckEntity
stringInfo =
  KnownType 0 [] Nothing

listInfo :: CheckEntity
listInfo =
  KnownType 1 [aDef] Nothing

emptyInfo :: CheckEntity
emptyInfo =
  KnownTerm (Forall emptyAnno [aDef] (list (TyApp emptyAnno aRef []))) Constructor

contractInfo :: CheckEntity
contractInfo =
  KnownType 2 [] Nothing

-- forall a b. CONTRACT a b
fulfilInfo :: CheckEntity
fulfilInfo = KnownTerm (Forall emptyAnno [hDef, iDef] (TyApp emptyAnno contractRef [TyApp emptyAnno hRef [], TyApp emptyAnno iRef []])) Constructor

eventInfo, eventCInfo :: CheckEntity
eventInfo = KnownType 2 [] Nothing
eventCInfo = KnownTerm (Forall emptyAnno [fDef, gDef] (Fun emptyAnno [mkOnt partyTy, mkOnt actTy, mkOnt number] (TyApp emptyAnno eventRef [partyTy, actTy]))) Constructor
  where
  actTy = mkTyVar gRef
  partyTy = mkTyVar fRef
  mkTyVar x = TyApp emptyAnno x []
  mkOnt = MkOptionallyNamedType emptyAnno Nothing

-- forall a b. CONTRACT a b -> [Event a b] -> CONTRACT a b
evalContractInfo :: CheckEntity
evalContractInfo = KnownTerm (Forall emptyAnno [bDef, cDef] (Fun emptyAnno [mkOnt ctrct, mkOnt (list eventTy), mkOnt number] ctrct)) Computable
  where
  ctrct = contract (mkTyVar bRef) (mkTyVar cRef)
  eventTy = TyApp emptyAnno eventRef [mkTyVar bRef, mkTyVar cRef]
  mkOnt = MkOptionallyNamedType emptyAnno Nothing
  mkTyVar r = TyApp emptyAnno r []


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
    , (NormalName "CONTRACT",     [contractUnique    ])
    , (NormalName "EVENT",        [eventUnique       ])
    , (NormalName "EVENT",        [eventCUnique      ])
    , (NormalName "EVALCONTRACT", [evalContractUnique])
    , (NormalName "FULFILLED",    [fulfilUnique      ])
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
    ]
