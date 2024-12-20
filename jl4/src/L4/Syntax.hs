{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module L4.Syntax where

import L4.Annotation

import Data.Text (Text)
import Data.TreeDiff (ToExpr)
import qualified GHC.Generics as GHC
import L4.Lexer (PosToken)
import Optics.Generic (gposition)
import Optics ((^.), set)

type Label = Name

data Name = Name Anno Text
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass ToExpr

instance HasAnno Name where
  type AnnoToken Name = PosToken

data Type' n =
    NamedType Anno n
  | Enum      Anno [n]
  | Record    Anno [TypedName n]
  | Boolean   Anno -- should perhaps just be a pre-defined NamedType
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Type' n) where
  type AnnoToken (Type' n) = PosToken

data TypedName n =
  MkTypedName Anno n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (ToExpr)

instance HasAnno (TypedName n) where
  type AnnoToken (TypedName n) = PosToken

data TypeSig n =
  MkTypeSig Anno (GivenSig n) (Maybe (GivethSig n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (TypeSig n) where
  type AnnoToken (TypeSig n) = PosToken
  getAnno e = e ^. gposition @1
  setAnno ann e = set (gposition @1) ann e

data GivenSig n =
  MkGivenSig Anno [TypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (GivenSig n) where
  type AnnoToken (GivenSig n) = PosToken

data GivethSig n =
  MkGivethSig Anno (TypedName n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (GivethSig n) where
  type AnnoToken (GivethSig n) = PosToken

data Decide n =
  MkDecide Anno (TypeSig n) [Clause n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Decide n) where
  type AnnoToken (Decide n) = PosToken

data Declare n =
  MkDeclare Anno n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Declare n) where
  type AnnoToken (Declare n) = PosToken

data Clause n =
  GuardedClause Anno (Expr n) (Guard n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Clause n) where
  type AnnoToken (Clause n) = PosToken

data Guard n =
    PlainGuard Anno (Expr n)
  | Otherwise  Anno
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Guard n) where
  type AnnoToken (Guard n) = PosToken

data Expr n =
    And  Anno (Expr n) (Expr n)
  | Or   Anno (Expr n) (Expr n)
  | Is   Anno (Expr n) (Expr n)
  | Not  Anno (Expr n)
  | Proj Anno (Expr n) Label
  | Var  Anno n
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Expr n) where
  type AnnoToken (Expr n) = PosToken

data Program n =
  MkProgram Anno [Section n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Section n =
  MkSection Anno SectionLevel n [Decl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

type SectionLevel = Int

instance HasAnno (Section n) where
  type AnnoToken (Section n) = PosToken

data Decl n =
    Declare Anno (Declare n)
  | Decide  Anno (Decide n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

instance HasAnno (Decl n) where
  type AnnoToken (Decl n) = PosToken

-- ----------------------------------------------------------------------------
-- Source Annotations
-- ----------------------------------------------------------------------------

type Anno = Anno_ PosToken
type AnnoElement = AnnoElement_ PosToken
type CsnCluster = CsnCluster_ PosToken
