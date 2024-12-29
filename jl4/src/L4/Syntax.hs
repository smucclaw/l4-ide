{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module L4.Syntax where

import L4.Annotation

import Data.Text (Text)
import Data.TreeDiff (ToExpr)
import qualified GHC.Generics as GHC
import L4.Lexer (PosToken)

type Label = Name

data Name = Name Anno Text
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (HasAnno, ToExpr)

data Type' n =
    Type      Anno
  | TyApp     Anno n [Type' n]
  | Fun       Anno [Type' n] (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data TypedName n =
  MkTypedName Anno n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data OptionallyTypedName n =
  MkOptionallyTypedName Anno n (Maybe (Type' n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data TypeSig n =
  MkTypeSig Anno (GivenSig n) (Maybe (GivethSig n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data GivenSig n =
  MkGivenSig Anno [OptionallyTypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data GivethSig n =
  MkGivethSig Anno (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Decide n =
  MkDecide Anno (TypeSig n) (AppForm n) (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data AppForm n =
  MkAppForm Anno n [n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Declare n =
  MkDeclare Anno (AppForm n) (TypeDecl n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Assume n =
  MkAssume Anno (AppForm n) (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data TypeDecl n =
    RecordDecl Anno [TypedName n]
  | EnumDecl Anno [ConDecl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data ConDecl n =
  MkConDecl Anno n [TypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Expr n =
    And        Anno (Expr n) (Expr n)
  | Or         Anno (Expr n) (Expr n)
  | Implies    Anno (Expr n) (Expr n)
  | Equals     Anno (Expr n) (Expr n)
  | Not        Anno (Expr n)
  | Plus       Anno (Expr n) (Expr n)
  | Minus      Anno (Expr n) (Expr n)
  | Times      Anno (Expr n) (Expr n)
  | DividedBy  Anno (Expr n) (Expr n)
  | Cons       Anno (Expr n) (Expr n)
  | Proj       Anno (Expr n) Label
  | Var        Anno n
  | Lam        Anno (GivenSig n) (Expr n)
  | App        Anno n [Expr n]
  | IfThenElse Anno (Expr n) (Expr n) (Expr n)
  | Consider   Anno (Expr n) [Branch n]
  | ParenExpr  Anno (Expr n) -- temporary
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Branch n =
    When Anno (Pattern n) (Expr n)
  | Otherwise Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Pattern n =
    PatApp Anno n [Pattern n]
  | PatCons Anno (Pattern n) (Pattern n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Program n =
  MkProgram Anno [Section n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

data Section n =
  MkSection Anno SectionLevel n [TopDecl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

type SectionLevel = Int

data TopDecl n =
    Declare Anno (Declare n)
  | Decide  Anno (Decide n)
  | Assume  Anno (Assume n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (HasAnno, ToExpr)

-- ----------------------------------------------------------------------------
-- Source Annotations
-- ----------------------------------------------------------------------------

type Anno = Anno_ PosToken
type AnnoElement = AnnoElement_ PosToken
type CsnCluster = CsnCluster_ PosToken
