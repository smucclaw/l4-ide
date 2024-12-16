{-# LANGUAGE LambdaCase #-}

module L4.ExactPrint where

import L4.Annotation
import L4.Lexer
import L4.Syntax

import Control.Monad.Extra qualified as Extra
import Control.Monad.Trans.Except
import Data.Text
import Data.Text qualified as Text
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
    , Extra.concatMapM declToTokens decls
    ]

declToTokens :: Decl Name -> HoleFit
declToTokens = \case
  Declare ann declare ->
    applyTokensWithHoles ann [declareToTokens declare]
  Decide ann decide ->
    applyTokensWithHoles ann [decideToTokens decide]

declareToTokens :: Declare Name -> HoleFit
declareToTokens (MkDeclare ann name type') =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

typeToTokens :: Type' Name -> HoleFit
typeToTokens = \case
  NamedType ann named -> applyTokensWithHoles ann [nameToTokens named]
  Enum ann enum -> applyTokensWithHoles ann [Extra.concatMapM nameToTokens enum]
  Record ann rcs -> applyTokensWithHoles ann [Extra.concatMapM typedNameToTokens rcs]
  Boolean ann -> applyTokensWithHoles ann []

typedNameToTokens :: TypedName Name -> HoleFit
typedNameToTokens (MkTypedName ann name type') =
  applyTokensWithHoles
    ann
    [ nameToTokens name
    , typeToTokens type'
    ]

decideToTokens :: Decide Name -> HoleFit
decideToTokens (MkDecide ann typeSig clauses) =
  applyTokensWithHoles
    ann
    [ typeSigToTokens typeSig
    , Extra.concatMapM clauseToTokens clauses
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

typeSigToTokens :: TypeSig Name -> HoleFit
typeSigToTokens (MkTypeSig ann given mGiveth) =
  applyTokensWithHoles
    ann
    [ givenToTokens given
    , maybe (pure []) givethToTokens mGiveth
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
    [ Extra.concatMapM typedNameToTokens names
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
