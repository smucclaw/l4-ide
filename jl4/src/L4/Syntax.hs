{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module L4.Syntax where

import L4.Annotation

import Data.Text (Text)
import Data.TreeDiff (ToExpr)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import L4.Lexer (PosToken)

data Name = MkName Anno RawName
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data RawName =
    NormalName Text
  | PreDef Text
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Type' n =
    Type   Anno -- ^ the type of types
  | TyApp  Anno n [Type' n] -- ^ an application of a type constructor
  | Fun    Anno [OptionallyNamedType n] (Type' n) -- ^ a function type, with possibly named arguments
  | Forall Anno [n] (Type' n) -- ^ universally quantified type
  | InfVar Anno RawName Int -- ^ only used during type inference
  -- | ParenType Anno (Type' n) -- temporary
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data TypedName n =
  MkTypedName Anno n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data OptionallyTypedName n =
  MkOptionallyTypedName Anno n (Maybe (Type' n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data OptionallyNamedType n =
  MkOptionallyNamedType Anno (Maybe n) (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data TypeSig n =
  MkTypeSig Anno (GivenSig n) (Maybe (GivethSig n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data GivenSig n =
  MkGivenSig Anno [OptionallyTypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data GivethSig n =
  MkGivethSig Anno (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Decide n =
  MkDecide Anno (TypeSig n) (AppForm n) (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data AppForm n =
  MkAppForm Anno n [n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Declare n =
  MkDeclare Anno (AppForm n) (TypeDecl n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Assume n =
  MkAssume Anno (AppForm n) (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Directive n =
    Eval Anno (Expr n)
  | Check Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data TypeDecl n =
    RecordDecl Anno [TypedName n]
  | EnumDecl Anno [ConDecl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data ConDecl n =
  MkConDecl Anno n [TypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

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
  | Modulo     Anno (Expr n) (Expr n)
  | Cons       Anno (Expr n) (Expr n)
  | Leq        Anno (Expr n) (Expr n)
  | Geq        Anno (Expr n) (Expr n)
  | Proj       Anno (Expr n) n -- record projection, we could consider making this yet another function application syntax
  | Var        Anno n -- currently not really needed because subsumed by empty App
  | Lam        Anno (GivenSig n) (Expr n)
  | App        Anno n [Expr n]
  | AppNamed   Anno n [NamedExpr n]
  | IfThenElse Anno (Expr n) (Expr n) (Expr n)
  | Consider   Anno (Expr n) [Branch n]
  -- | ParenExpr  Anno (Expr n) -- temporary
  | Lit        Anno Lit
  | List       Anno [Expr n] -- list literal
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data NamedExpr n =
  MkNamedExpr Anno n (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Lit =
    NumericLit Anno Int
  | StringLit  Anno Text
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Branch n =
    When Anno (Pattern n) (Expr n)
  | Otherwise Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Pattern n =
    PatApp Anno n [Pattern n]
  | PatCons Anno (Pattern n) (Pattern n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Program n =
  MkProgram Anno [Section n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

data Section n =
  MkSection Anno SectionLevel (Maybe n) [TopDecl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

type SectionLevel = Int

data TopDecl n =
    Declare   Anno (Declare n)
  | Decide    Anno (Decide n)
  | Assume    Anno (Assume n)
  | Directive Anno (Directive n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr)

-- ----------------------------------------------------------------------------
-- Source Annotations
-- ----------------------------------------------------------------------------

type Anno = Anno_ PosToken
type AnnoElement = AnnoElement_ PosToken
type CsnCluster = CsnCluster_ PosToken

instance HasAnno Name where
  type AnnoToken Name = PosToken
instance HasAnno (Type' n) where
  type AnnoToken (Type' n) = PosToken
instance HasAnno (TypedName n) where
  type AnnoToken (TypedName n) = PosToken
instance HasAnno (OptionallyTypedName n) where
  type AnnoToken (OptionallyTypedName n) = PosToken
instance HasAnno (OptionallyNamedType n) where
  type AnnoToken (OptionallyNamedType n) = PosToken
instance HasAnno (TypeSig n) where
  type AnnoToken (TypeSig n) = PosToken
instance HasAnno (GivenSig n) where
  type AnnoToken (GivenSig n) = PosToken
instance HasAnno (GivethSig n) where
  type AnnoToken (GivethSig n) = PosToken
instance HasAnno (Decide n) where
  type AnnoToken (Decide n) = PosToken
instance HasAnno (AppForm n) where
  type AnnoToken (AppForm n) = PosToken
instance HasAnno (Declare n) where
  type AnnoToken (Declare n) = PosToken
instance HasAnno (Assume n) where
  type AnnoToken (Assume n) = PosToken
instance HasAnno (Directive n) where
  type AnnoToken (Directive n) = PosToken
instance HasAnno (TypeDecl n) where
  type AnnoToken (TypeDecl n) = PosToken
instance HasAnno (ConDecl n) where
  type AnnoToken (ConDecl n) = PosToken
instance HasAnno (Expr n) where
  type AnnoToken (Expr n) = PosToken
instance HasAnno (NamedExpr n) where
  type AnnoToken (NamedExpr n) = PosToken
instance HasAnno Lit where
  type AnnoToken Lit = PosToken
instance HasAnno (Branch n) where
  type AnnoToken (Branch n) = PosToken
instance HasAnno (Pattern n) where
  type AnnoToken (Pattern n) = PosToken
instance HasAnno (Program n) where
  type AnnoToken (Program n) = PosToken
instance HasAnno (Section n) where
  type AnnoToken (Section n) = PosToken
instance HasAnno (TopDecl n) where
  type AnnoToken (TopDecl n) = PosToken
