{-# LANGUAGE LambdaCase #-}

module L4.ExactPrint where

import L4.Annotation
import L4.Lexer
import L4.Syntax

import qualified Control.Monad.Extra as Extra
import Control.Monad.Trans.Except
import Data.Text
import qualified Data.Text as Text
import GHC.Stack

type EPM = Except EPError

data EPError
  = InsufficientHoleFit CallStack
  deriving (Show)

prettyEPError :: EPError -> Text
prettyEPError (InsufficientHoleFit cs) = "HoleFit requested but not enough given at: " <> Text.pack (prettyCallStack cs)

type HoleFit = EPM [PosToken]

exactprint :: Program Name -> Either EPError Text
exactprint =
  runExcept . fmap (Text.concat . fmap displayPosToken) . progToTokens

progToTokens :: Program Name -> HoleFit
progToTokens (MkProgram ann sections) =
  applyTokensWithHoles ann [Extra.concatMapM sectionToTokens sections]

sectionToTokens :: Section Name -> HoleFit
sectionToTokens (MkSection ann _lvl name decls) =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , Extra.concatMapM topdeclToTokens decls
    ]

topdeclToTokens :: TopDecl Name -> HoleFit
topdeclToTokens = \case
  Declare ann declare ->
    applyTokensWithHoles ann [declareToTokens declare]
  Decide ann decide ->
    applyTokensWithHoles ann [decideToTokens decide]
  Assume ann assume ->
    applyTokensWithHoles ann [assumeToTokens assume]

assumeToTokens :: Assume Name -> HoleFit
assumeToTokens (MkAssume ann appform type') =
  applyTokensWithHoles
    ann
    [ appFormToTokens appform
    , typeToTokens type'
    ]

declareToTokens :: Declare Name -> HoleFit
declareToTokens (MkDeclare ann appform tydecl) =
  applyTokensWithHoles
    ann
    [ appFormToTokens appform
    , typeDeclToTokens tydecl
    ]

typeDeclToTokens :: TypeDecl Name -> HoleFit
typeDeclToTokens = \case
  RecordDecl ann tns -> applyTokensWithHoles ann [Extra.concatMapM typedNameToTokens tns]
  EnumDecl ann cds -> applyTokensWithHoles ann [Extra.concatMapM conDeclToTokens cds]

conDeclToTokens :: ConDecl Name -> HoleFit
conDeclToTokens (MkConDecl ann n tns) =
  applyTokensWithHoles
    ann
      [ nameToTokens n
      , Extra.concatMapM typedNameToTokens tns
      ]

typeToTokens :: Type' Name -> HoleFit
typeToTokens = \case
  Type ann -> applyTokensWithHoles ann []
  TyApp ann n ts -> applyTokensWithHoles ann [nameToTokens n, Extra.concatMapM typeToTokens ts]
  Fun ann ts t -> applyTokensWithHoles ann [Extra.concatMapM typeToTokens ts, typeToTokens t]

typedNameToTokens :: TypedName Name -> HoleFit
typedNameToTokens (MkTypedName ann name type') =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

optionallyTypedNameToTokens :: OptionallyTypedName Name -> HoleFit
optionallyTypedNameToTokens (MkOptionallyTypedName ann name mType') =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , maybe (pure []) typeToTokens mType'
    ]

decideToTokens :: Decide Name -> HoleFit
decideToTokens (MkDecide ann typeSig appForm expr) =
  applyTokensWithHoles
    ann
    [ typeSigToTokens typeSig
    , appFormToTokens appForm
    , exprToTokens expr
    ]

appFormToTokens :: AppForm Name -> HoleFit
appFormToTokens (MkAppForm ann name names) =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , Extra.concatMapM nameToTokens names
    ]

clauseToTokens :: Clause Name -> HoleFit
clauseToTokens (GuardedClause ann e guard) =
  applyTokensWithHoles
    ann
    [ exprToTokens e
    , guardToTokens guard
    ]

guardToTokens :: Guard Name -> HoleFit
guardToTokens = \case
  PlainGuard ann e -> applyTokensWithHoles ann [exprToTokens e]
  Otherwise ann -> applyTokensWithHoles ann []

exprToTokens :: Expr Name -> HoleFit
exprToTokens = \case
  And ann e1 e2 ->
    applyTokensWithHoles
      ann
      [exprToTokens e1, exprToTokens e2]
  Or ann e1 e2 ->
    applyTokensWithHoles
      ann
      [exprToTokens e1, exprToTokens e2]
  Is ann e1 e2 ->
    applyTokensWithHoles
      ann
      [exprToTokens e1, exprToTokens e2]
  Not ann e ->
    applyTokensWithHoles
      ann
      [exprToTokens e]
  Proj ann e lbl ->
    applyTokensWithHoles
      ann
      [exprToTokens e, nameToTokens lbl]
  Var ann name ->
    applyTokensWithHoles
      ann
      [nameToTokens name]
  Lam ann given e ->
    applyTokensWithHoles
      ann
      [givenToTokens given, exprToTokens e]
  App ann n es ->
    applyTokensWithHoles
      ann
      [nameToTokens n, Extra.concatMapM exprToTokens es]
  IfThenElse ann e1 e2 e3 ->
    applyTokensWithHoles
      ann
      [exprToTokens e1, exprToTokens e2, exprToTokens e3]

typeSigToTokens :: TypeSig Name -> HoleFit
typeSigToTokens (MkTypeSig ann given mGiveth) =
  applyTokensWithHoles
    ann
    [ givenToTokens given
    , maybe (pure []) givethToTokens mGiveth
    ]

givethToTokens :: GivethSig Name -> HoleFit
givethToTokens (MkGivethSig ann type') =
  applyTokensWithHoles
    ann
    [ typeToTokens type'
    ]

givenToTokens :: GivenSig Name -> HoleFit
givenToTokens (MkGivenSig ann names) =
  applyTokensWithHoles
    ann
    [ Extra.concatMapM optionallyTypedNameToTokens names
    ]

nameToTokens :: Name -> HoleFit
nameToTokens (Name ann _) = applyTokensWithHoles ann []

applyTokensWithHoles :: (HasCallStack) => Anno -> [HoleFit] -> EPM [PosToken]
applyTokensWithHoles (Anno []) _ = pure []
applyTokensWithHoles (Anno (AnnoHole : cs)) holeFits = case holeFits of
  [] -> do
    throwE $ InsufficientHoleFit callStack
  (x : xs) -> do
    r <- x
    rs <- applyTokensWithHoles (Anno cs) xs
    pure (r <> rs)
applyTokensWithHoles (Anno (AnnoCsn m : cs)) xs = do
  let
    r = csnTokens m
  rs <- applyTokensWithHoles (Anno cs) xs
  pure (r <> rs)
