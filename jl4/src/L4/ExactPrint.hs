{-# LANGUAGE LambdaCase #-}

module L4.ExactPrint where

import L4.Annotation
import L4.Lexer
import L4.Syntax

import Data.List qualified as List
import Data.Text
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)

exactprint :: Program Name -> Text
exactprint =
  Text.concat . fmap displayPosToken . progToTokens

progToTokens :: Program Name -> [PosToken]
progToTokens (MkProgram ann sections) =
  applyTokensWithHoles ann [List.concatMap sectionToTokens sections]

sectionToTokens :: Section Name -> [PosToken]
sectionToTokens (MkSection ann _lvl name decls) =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , List.concatMap declToTokens decls
    ]

declToTokens :: Decl Name -> [PosToken]
declToTokens = \case
  Declare ann declare ->
    applyTokensWithHoles ann [declareToTokens declare]
  Decide ann decide ->
    applyTokensWithHoles ann [decideToTokens decide]

declareToTokens :: Declare Name -> [PosToken]
declareToTokens (MkDeclare ann name type') =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

typeToTokens :: Type' Name -> HoleFit
typeToTokens = \case
  NamedType ann named -> applyTokensWithHoles ann [nameToTokens named]
  Enum ann enum -> applyTokensWithHoles ann [List.concatMap nameToTokens enum]
  Record ann rcs -> applyTokensWithHoles ann [List.concatMap typedNameToTokens rcs]
  Boolean ann -> applyTokensWithHoles ann []

typedNameToTokens :: TypedName Name -> [PosToken]
typedNameToTokens (MkTypedName ann name type') =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

decideToTokens :: Decide Name -> [PosToken]
decideToTokens (MkDecide ann typeSig clauses) =
  applyTokensWithHoles
    ann
    [ typeSigToTokens typeSig
    , List.concatMap clauseToTokens clauses
    ]

clauseToTokens :: Clause Name -> [PosToken]
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

typeSigToTokens :: TypeSig Name -> HoleFit
typeSigToTokens (MkTypeSig ann given mGiveth) =
  applyTokensWithHoles
    ann
    [ givenToTokens given
    , maybe [] givethToTokens mGiveth
    ]

givethToTokens :: GivethSig Name -> HoleFit
givethToTokens (MkGivethSig ann typedName) =
  applyTokensWithHoles
    ann
    [ typedNameToTokens typedName
    ]

givenToTokens :: GivenSig Name -> HoleFit
givenToTokens (MkGivenSig ann names) =
  applyTokensWithHoles
    ann
    [ List.concatMap typedNameToTokens names
    ]

nameToTokens :: Name -> HoleFit
nameToTokens (Name ann _) = applyTokensWithHoles ann []

type HoleFit = [PosToken]

applyTokensWithHoles :: (HasCallStack) => Anno -> [HoleFit] -> [PosToken]
applyTokensWithHoles (Anno []) _ = []
applyTokensWithHoles (Anno (AnnoHole : cs)) holeFits = case holeFits of
  [] -> error $ "applyTokensWithHoles: HoleFit requested, but not enough Fits given."
  (x : xs) -> x <> applyTokensWithHoles (Anno cs) xs
applyTokensWithHoles (Anno (AnnoCsn m : cs)) xs = csnTokens m <> applyTokensWithHoles (Anno cs) xs
