-- | Build the initial environment used during scope and type checking.
module L4.TypeCheck.Environment where

import Base
import qualified Base.Map as Map
import L4.Annotation
import L4.Lexer
import L4.Parser.SrcSpan
import L4.Syntax
import L4.TypeCheck.Types

-- | Helper function to create a predefined name.
preDef :: Text -> Name
preDef t =
  MkName
    (mkAnno
      [mkCluster
        (CsnCluster
          (ConcreteSyntaxNode [MkPosToken (MkSrcRange (MkSrcPos 0 0) (MkSrcPos 0 0) 0 builtinUri) (TIdentifier t)] Nothing Visible)
          (ConcreteSyntaxNode [] Nothing Hidden)
        )
      ]
    )
    (PreDef t)

builtinUri :: NormalizedUri
builtinUri = toNormalizedUri (Uri "jl4:builtin")

-- uniques of built-in / predefs are having the 'b' marker:
--
-- 10  BOOLEAN
-- 11  NUMBER
-- 12  STRING
-- 13  LIST
-- 14  CONTRACT
-- 30  FALSE
-- 31  TRUE
-- 32  EMPTY
-- 40  A (type variable in EMPTY)

-- BOOLEAN

booleanUnique :: Unique
booleanUnique = MkUnique 'b' 10 builtinUri

booleanName :: Name
booleanName = preDef "BOOLEAN"

booleanRef :: Resolved
booleanRef = Ref booleanName booleanUnique booleanName

boolean :: Type' Resolved
boolean = TyApp emptyAnno booleanRef []

falseUnique :: Unique
falseUnique = MkUnique 'b' 30 builtinUri

falseName :: Name
falseName = preDef "FALSE"

falseDef :: Resolved
falseDef = Def falseUnique falseName

falseRef :: Resolved
falseRef = Ref falseName falseUnique falseName

trueUnique :: Unique
trueUnique = MkUnique 'b' 31 builtinUri

trueName :: Name
trueName = preDef "TRUE"

trueDef :: Resolved
trueDef = Def trueUnique trueName

trueRef :: Resolved
trueRef = Ref trueName trueUnique trueName

-- NUMBER

numberUnique :: Unique
numberUnique = MkUnique 'b' 11 builtinUri

numberName :: Name
numberName = preDef "NUMBER"

numberRef :: Resolved
numberRef = Ref numberName numberUnique numberName

number :: Type' Resolved
number = TyApp emptyAnno numberRef []

-- STRING

stringUnique :: Unique
stringUnique = MkUnique 'b' 12 builtinUri

stringName :: Name
stringName = preDef "STRING"

stringRef :: Resolved
stringRef = Ref stringName stringUnique stringName

string :: Type' Resolved
string = TyApp emptyAnno stringRef []

-- LIST

listUnique :: Unique
listUnique = MkUnique 'b' 13 builtinUri

listName :: Name
listName = preDef "LIST"

listRef :: Resolved
listRef = Ref listName listUnique listName

list :: Type' Resolved -> Type' Resolved
list a = TyApp emptyAnno listRef [a]

emptyUnique :: Unique
emptyUnique = MkUnique 'b' 32 builtinUri

emptyName :: Name
emptyName = preDef "EMPTY"

emptyDef :: Resolved
emptyDef = Def emptyUnique emptyName

emptyRef :: Resolved
emptyRef = Ref emptyName emptyUnique emptyName

aUnique :: Unique
aUnique = MkUnique 'b' 40 builtinUri

aName :: Name
aName = MkName emptyAnno (NormalName "A")

aDef :: Resolved
aDef = Def aUnique aName

aRef :: Resolved
aRef = Ref aName aUnique aName

bUnique :: Unique
bUnique = MkUnique 'b' 41 builtinUri

bName :: Name
bName = MkName emptyAnno (NormalName "B")

bDef :: Resolved
bDef = Def bUnique bName

bRef :: Resolved
bRef = Ref bName bUnique bName

cUnique :: Unique
cUnique = MkUnique 'b' 43 builtinUri

cName :: Name
cName = MkName emptyAnno (NormalName "C")

cDef :: Resolved
cDef = Def cUnique cName

cRef :: Resolved
cRef = Ref cName cUnique cName

dUnique :: Unique
dUnique = MkUnique 'b' 44 builtinUri

dName :: Name
dName = MkName emptyAnno (NormalName "D")

dDef :: Resolved
dDef = Def dUnique dName

dRef :: Resolved
dRef = Ref dName dUnique dName

eUnique :: Unique
eUnique = MkUnique 'b' 45 builtinUri

eName :: Name
eName = MkName emptyAnno (NormalName "E")

eDef :: Resolved
eDef = Def eUnique eName

eRef :: Resolved
eRef = Ref eName eUnique eName

fUnique :: Unique
fUnique = MkUnique 'b' 46 builtinUri

fName :: Name
fName = MkName emptyAnno (NormalName "F")

fDef :: Resolved
fDef = Def fUnique fName

fRef :: Resolved
fRef = Ref fName fUnique fName

gUnique :: Unique
gUnique = MkUnique 'b' 47 builtinUri

gName :: Name
gName = MkName emptyAnno (NormalName "G")

gDef :: Resolved
gDef = Def gUnique gName

gRef :: Resolved
gRef = Ref gName gUnique gName

hUnique :: Unique
hUnique = MkUnique 'b' 48 builtinUri

hName :: Name
hName = MkName emptyAnno (NormalName "H")

hDef :: Resolved
hDef = Def hUnique hName

hRef :: Resolved
hRef = Ref hName hUnique hName

iUnique :: Unique
iUnique = MkUnique 'b' 49 builtinUri

iName :: Name
iName = MkName emptyAnno (NormalName "I")

iDef :: Resolved
iDef = Def iUnique iName

iRef :: Resolved
iRef = Ref iName iUnique iName

f'Unique :: Unique
f'Unique = MkUnique 'b' 50 builtinUri

f'Def :: Resolved
f'Def = Def f'Unique fName

f'Ref :: Resolved
f'Ref = Ref fName f'Unique fName

-- CONTRACT

contractUnique :: Unique
contractUnique = MkUnique 'b' 14 builtinUri

contractName :: Name
contractName = preDef "CONTRACT"

contractRef :: Resolved
contractRef = Ref contractName contractUnique contractName

contract :: Type' Resolved -> Type' Resolved -> Type' Resolved
contract party action = TyApp emptyAnno contractRef [party, action]

fulfilUnique :: Unique
fulfilUnique = MkUnique 'b' 15 builtinUri

fulfilName :: Name
fulfilName = preDef "FULFILLED"

fulfilRef :: Resolved
fulfilRef = Ref fulfilName fulfilUnique fulfilName

evalContractUnique :: Unique
evalContractUnique = MkUnique 'b' 16 builtinUri

evalContractName :: Name
evalContractName = preDef "EVALCONTRACT"

evalContractRef :: Resolved
evalContractRef = Ref evalContractName evalContractUnique evalContractName

-- EVENT

eventUnique, eventCUnique :: Unique
eventUnique = MkUnique 'b' 17 builtinUri
eventCUnique = MkUnique 'b' 18 builtinUri

eventName, eventCName :: Name
eventName = preDef "EVENT"
eventCName = eventName

eventRef, eventCRef :: Resolved
eventRef = Ref eventName eventUnique eventName
eventCRef = Ref eventCName eventCUnique eventCName

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
  actTy = mkTyVar fRef
  partyTy = mkTyVar gRef
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
