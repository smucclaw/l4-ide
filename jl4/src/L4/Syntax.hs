{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module L4.Syntax where

import L4.Annotation
import L4.Lexer (PosToken)

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.TreeDiff (ToExpr)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Optics.Generic

data Name = MkName Anno RawName
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data RawName =
    NormalName Text
  | PreDef Text
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Unique = MkUnique !Char !Int
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Resolved =
    Def Unique Name        -- ^ defining occurrence of name
  | Ref Name Unique Name   -- ^ referring occurrence of name, original occurrence of name
  | OutOfScope Unique Name -- ^ used to make progress for names where name resolution failed
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

getOriginal :: Resolved -> Name
getOriginal (Def _ n)        = n
getOriginal (Ref _ _ o)      = o
getOriginal (OutOfScope _ n) = n

getUnique :: Resolved -> Unique
getUnique (Def u _)        = u
getUnique (Ref _ u _)      = u
getUnique (OutOfScope u _) = u

getActual :: Resolved -> Name
getActual (Def _ n)        = n
getActual (Ref n _ _)      = n
getActual (OutOfScope _ n) = n

data Type' n =
    Type   Anno -- ^ the type of types
  | TyApp  Anno n [Type' n] -- ^ an application of a type constructor
  | Fun    Anno [OptionallyNamedType n] (Type' n) -- ^ a function type, with possibly named arguments
  | Forall Anno [n] (Type' n) -- ^ universally quantified type
  | InfVar Anno RawName Int -- ^ only used during type inference
  -- | ParenType Anno (Type' n) -- temporary
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data TypedName n =
  MkTypedName Anno n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data OptionallyTypedName n =
  MkOptionallyTypedName Anno n (Maybe (Type' n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data OptionallyNamedType n =
  MkOptionallyNamedType Anno (Maybe n) (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data TypeSig n =
  MkTypeSig Anno (GivenSig n) (Maybe (GivethSig n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data GivenSig n =
  MkGivenSig Anno [OptionallyTypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data GivethSig n =
  MkGivethSig Anno (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Decide n =
  MkDecide Anno (TypeSig n) (AppForm n) (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data AppForm n =
  MkAppForm Anno n [n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Declare n =
  MkDeclare Anno (TypeSig n) (AppForm n) (TypeDecl n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Assume n =
  MkAssume Anno (TypeSig n) (AppForm n) (Maybe (Type' n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Directive n =
    Eval Anno (Expr n)
  | Check Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data TypeDecl n =
    RecordDecl Anno [TypedName n]
  | EnumDecl Anno [ConDecl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data ConDecl n =
  MkConDecl Anno n [TypedName n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

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
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data NamedExpr n =
  MkNamedExpr Anno n (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Lit =
    NumericLit Anno Int
  | StringLit  Anno Text
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Branch n =
    When Anno (Pattern n) (Expr n)
  | Otherwise Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Pattern n =
    PatVar Anno n -- not used during parsing, but after scope-checking
  | PatApp Anno n [Pattern n]
  | PatCons Anno (Pattern n) (Pattern n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Program n =
  MkProgram Anno [Section n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Section n =
  MkSection Anno SectionLevel (Maybe n) [TopDecl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

type SectionLevel = Int

data TopDecl n =
    Declare   Anno (Declare n)
  | Decide    Anno (Decide n)
  | Assume    Anno (Assume n)
  | Directive Anno (Directive n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- ----------------------------------------------------------------------------
-- Source Annotations
-- ----------------------------------------------------------------------------

type Anno = Anno_ PosToken (Type' Resolved)
type AnnoElement = AnnoElement_ PosToken
type CsnCluster = CsnCluster_ PosToken

newtype L4Syntax a = MkL4Syntax a

instance {-# OVERLAPPING #-} Semigroup (Maybe (Type' Resolved)) where
  _ <> _ = Nothing

instance (GHC.Generic a, GPosition 1 a a Anno Anno) => HasAnno (L4Syntax a) where
  type AnnoToken (L4Syntax a) = PosToken
  type AnnoExtra (L4Syntax a) = Type' Resolved

  setAnno ann (MkL4Syntax a) = MkL4Syntax (genericSetAnno ann a)
  getAnno (MkL4Syntax a) = genericGetAnno a

deriving via L4Syntax Name
  instance HasAnno Name
deriving via L4Syntax (Type' n)
  instance HasAnno (Type' n)
deriving via L4Syntax (TypedName n)
  instance HasAnno (TypedName n)
deriving via L4Syntax (OptionallyTypedName n)
  instance HasAnno (OptionallyTypedName n)
deriving via L4Syntax (OptionallyNamedType n)
  instance HasAnno (OptionallyNamedType n)
deriving via L4Syntax (TypeSig n)
  instance HasAnno (TypeSig n)
deriving via L4Syntax (GivenSig n)
  instance HasAnno (GivenSig n)
deriving via L4Syntax (GivethSig n)
  instance HasAnno (GivethSig n)
deriving via L4Syntax (Decide n)
  instance HasAnno (Decide n)
deriving via L4Syntax (AppForm n)
  instance HasAnno (AppForm n)
deriving via L4Syntax (Declare n)
  instance HasAnno (Declare n)
deriving via L4Syntax (Assume n)
  instance HasAnno (Assume n)
deriving via L4Syntax (Directive n)
  instance HasAnno (Directive n)
deriving via L4Syntax (TypeDecl n)
  instance HasAnno (TypeDecl n)
deriving via L4Syntax (ConDecl n)
  instance HasAnno (ConDecl n)
deriving via L4Syntax (Expr n)
  instance HasAnno (Expr n)
deriving via L4Syntax (NamedExpr n)
  instance HasAnno (NamedExpr n)
deriving via L4Syntax Lit
  instance HasAnno Lit
deriving via L4Syntax (Branch n)
  instance HasAnno (Branch n)
deriving via L4Syntax (Pattern n)
  instance HasAnno (Pattern n)
deriving via L4Syntax (Program n)
  instance HasAnno (Program n)
deriving via L4Syntax (Section n)
  instance HasAnno (Section n)
deriving via L4Syntax (TopDecl n)
  instance HasAnno (TopDecl n)
