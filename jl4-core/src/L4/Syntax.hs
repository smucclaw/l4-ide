{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module L4.Syntax where

import Base
import L4.Annotation
import L4.Lexer (PosToken)

import Data.Default
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Optics
import qualified Base.Text as Text
import qualified Data.List.NonEmpty as NE

data Name = MkName Anno RawName
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data RawName =
    NormalName Text
  | QualifiedName (NonEmpty Text) Text
  -- ^ contains the actual name and a list of qualifiers, e.g.
  -- foo.bar becomes @'QualifiedName' bar [foo]@
  | PreDef Text
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Unique = MkUnique {sort :: !Char, unique :: !Int, moduleUri :: !NormalizedUri}
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)


data Resolved =
    Def Unique Name        -- ^ defining occurrence of name
  | Ref Name Unique Name   -- ^ referring occurrence of name, original occurrence of name
  | OutOfScope Unique Name -- ^ used to make progress for names where name resolution failed
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- | Extract the original name, i.e., for referring occurrences, the defining occurrence of the name.
getOriginal :: Resolved -> Name
getOriginal (Def _ n)        = n
getOriginal (Ref _ _ o)      = o
getOriginal (OutOfScope _ n) = n

-- | Extract the unique of a name, should be used for determining name equality.
getUnique :: Resolved -> Unique
getUnique (Def u _)        = u
getUnique (Ref _ u _)      = u
getUnique (OutOfScope u _) = u

-- | Extract the raw name from a name.
rawName :: Name -> RawName
rawName (MkName _ raw) = raw

-- | Return the unique and the original name for a resolved name.
getUniqueName :: Resolved -> (Unique, Name)
getUniqueName r = (getUnique r, getOriginal r)

-- | Extract the actual name, i.e., for referring occurrences, the actual referring occurrence.
getActual :: Resolved -> Name
getActual (Def _ n)        = n
getActual (Ref n _ _)      = n
getActual (OutOfScope _ n) = n

-- | Get the actual textual form of a raw name.
rawNameToText :: RawName -> Text
rawNameToText (NormalName n)       = n
rawNameToText (PreDef n)           = n
rawNameToText (QualifiedName qs n) = Text.intercalate "." (NE.toList qs <> [n])

nameToText :: Name -> Text
nameToText = rawNameToText . rawName

data Type' n =
    Type   Anno -- ^ the type of types
  | TyApp  Anno n [Type' n] -- ^ an application of a type constructor
  | Fun    Anno [OptionallyNamedType n] (Type' n) -- ^ a function type, with possibly named arguments
  | Forall Anno [n] (Type' n) -- ^ universally quantified type
  | InfVar Anno RawName Int -- ^ only used during type inference
  -- | ParenType Anno (Type' n) -- temporary
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- | A kind of a type is its arity.
--
-- (Currently not used in the surface syntax.)
--
type Kind = Int

data TypedName n =
  MkTypedName Anno n (Type' n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data OptionallyTypedName n =
  MkOptionallyTypedName Anno n (Maybe (Type' n))
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data OptionallyNamedType n =
  MkOptionallyNamedType Anno (Maybe n) (Type' n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data TypeSig n =
  MkTypeSig Anno (GivenSig n) (Maybe (GivethSig n))
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data GivenSig n =
  MkGivenSig Anno [OptionallyTypedName n]
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data GivethSig n =
  MkGivethSig Anno (Type' n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Decide n =
  MkDecide Anno (TypeSig n) (AppForm n) (Expr n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data AppForm n =
  MkAppForm Anno n [n] (Maybe (Aka n))
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Aka n =
  MkAka Anno [n]
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Declare n =
  MkDeclare Anno (TypeSig n) (AppForm n) (TypeDecl n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Assume n =
  MkAssume Anno (TypeSig n) (AppForm n) (Maybe (Type' n))
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Directive n =
    StrictEval Anno (Expr n)
  | LazyEval Anno (Expr n)
  | Check Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Import n =
  MkImport Anno n
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data TypeDecl n =
    RecordDecl Anno (Maybe n) [TypedName n]
  | EnumDecl Anno [ConDecl n]
  | SynonymDecl Anno (Type' n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data ConDecl n =
  MkConDecl Anno n [TypedName n]
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Expr n =
    And        Anno (Expr n) (Expr n)
    -- (var1 AND var3) AND {- Comment -}var2
    -- [AnnoHole, CSN "AND", CSN " ", CSN "{- Comment -}", AnnoHole]
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
  | Lt         Anno (Expr n) (Expr n)
  | Gt         Anno (Expr n) (Expr n)
  | Proj       Anno (Expr n) n -- record projection, we could consider making this yet another function application syntax
  | Var        Anno n -- currently not really needed because subsumed by empty App
  | Lam        Anno (GivenSig n) (Expr n)
  | App        Anno n [Expr n]
  | AppNamed   Anno n [NamedExpr n] (Maybe [Int]) -- we store the order of arguments during type checking
  | IfThenElse Anno (Expr n) (Expr n) (Expr n)
  | Consider   Anno (Expr n) [Branch n]
  -- | ParenExpr  Anno (Expr n) -- temporary
  | Lit        Anno Lit
  | List       Anno [Expr n] -- list literal
  | Where      Anno (Expr n) [LocalDecl n]
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data NamedExpr n =
  MkNamedExpr Anno n (Expr n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Lit =
    NumericLit Anno Int
  | StringLit  Anno Text
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Branch n =
    When Anno (Pattern n) (Expr n)
  | Otherwise Anno (Expr n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Pattern n =
    PatVar Anno n
    -- ^ not used during parsing, but after scope-checking
  | PatApp Anno n [Pattern n]
  | PatCons Anno (Pattern n) (Pattern n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- | A 'Program' is a module with some 'Module's it directly depends on
data Program n
  = MkProgram (Module n) [Module n]

data Module n =
  MkModule Anno NormalizedUri (Section n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Section n =
  MkSection Anno (Maybe n) (Maybe (Aka n)) [TopDecl n]
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)


data TopDecl n =
    Declare   Anno (Declare n)
  | Decide    Anno (Decide n)
  | Assume    Anno (Assume n)
  | Directive Anno (Directive n)
  | Import    Anno (Import n)
  | Section   Anno (Section n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data LocalDecl n =
    LocalDecide Anno (Decide n)
  | LocalAssume Anno (Assume n)
  deriving stock (GHC.Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- | Given a @'Module' n@, runs a 'foldMap' over all of
-- nodes of the specified type
_foldNodeType
  :: forall nodeType n m
   . (Monoid m, Optics.GPlate (nodeType n) (Module n))
  => (nodeType n -> m) -> Module n -> m
_foldNodeType = Optics.foldMapOf Optics.gplate

-- | Given a @'Module' n@, runs a 'foldMap' over 'Decide's at the toplevel
foldTopLevelDecides
  :: forall n m. (Monoid m) => (Decide n -> m) -> Module n -> m
foldTopLevelDecides = _foldNodeType

-- | Given a @'Module' n@, runs a 'foldMap' over 'TopDecl's at the toplevel
foldTopDecls
  :: forall n m. (Monoid m) => (TopDecl n -> m) -> Module n -> m
foldTopDecls = _foldNodeType


appFormHead :: Lens' (AppForm n) n
appFormHead = lensVL (\ wrap (MkAppForm ann n ns maka) -> (\ wn -> MkAppForm ann wn ns maka) <$> wrap n)

appFormHeads :: AppForm n -> [n]
appFormHeads (MkAppForm _ann n _ns maka) =
  n :
  case maka of
    Nothing           -> []
    Just (MkAka _ ns) -> ns

appFormArgs :: Lens' (AppForm n) [n]
appFormArgs = lensVL (\ wrap (MkAppForm ann n ns maka) -> (\ wns -> MkAppForm ann n wns maka) <$> wrap ns)

-- ----------------------------------------------------------------------------
-- Source Annotations
-- ----------------------------------------------------------------------------

data Extension = Extension
  { resolvedInfo :: Maybe Info
  , nlg          :: Maybe Nlg
  }
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Info =
    TypeInfo (Type' Resolved) (Maybe Nlg)
  | KindInfo Kind
  | KeywordInfo
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

instance Default Extension where
  def = Extension Nothing Nothing

annoOf :: HasAnno a => Lens' a (Anno' a)
annoOf = lens
  getAnno
  (flip setAnno)

annInfo :: Lens' Anno (Maybe Info)
annInfo = annoExtra % #resolvedInfo

annNlg :: Lens' Anno (Maybe Nlg)
annNlg = #extra % #nlg

setNlg :: Nlg -> Anno -> Anno
setNlg n a = a & annNlg ?~ n

type Anno = Anno_ PosToken Extension
type AnnoElement = AnnoElement_ PosToken
type CsnCluster = CsnCluster_ PosToken

newtype L4Syntax a = MkL4Syntax a

instance (GHC.Generic a, GPosition 1 a a Anno Anno) => HasAnno (L4Syntax a) where
  type AnnoToken (L4Syntax a) = PosToken
  type AnnoExtra (L4Syntax a) = Extension

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
deriving via L4Syntax (Aka n)
  instance HasAnno (Aka n)
deriving via L4Syntax (Declare n)
  instance HasAnno (Declare n)
deriving via L4Syntax (Assume n)
  instance HasAnno (Assume n)
deriving via L4Syntax (Directive n)
  instance HasAnno (Directive n)
deriving via L4Syntax (Import n)
  instance HasAnno (Import n)
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
deriving via L4Syntax (Module n)
  instance HasAnno (Module n)
deriving via L4Syntax (Section n)
  instance HasAnno (Section n)
deriving via L4Syntax (TopDecl n)
  instance HasAnno (TopDecl n)
deriving via L4Syntax (LocalDecl n)
  instance HasAnno (LocalDecl n)


-- Generic instance does not apply because we exclude the level.
instance ToConcreteNodes PosToken (Section Name) where
  toNodes (MkSection ann name maka decls) =
    flattenConcreteNodes ann [toNodes name, toNodes maka, toNodes decls]

deriving anyclass instance ToConcreteNodes PosToken (TopDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (Assume Name)
deriving anyclass instance ToConcreteNodes PosToken (Declare Name)
deriving anyclass instance ToConcreteNodes PosToken (TypeDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (ConDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (Type' Name)
deriving anyclass instance ToConcreteNodes PosToken (TypedName Name)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyTypedName Name)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyNamedType Name)
deriving anyclass instance ToConcreteNodes PosToken (Decide Name)
deriving anyclass instance ToConcreteNodes PosToken (AppForm Name)
deriving anyclass instance ToConcreteNodes PosToken (Aka Name)
deriving anyclass instance ToConcreteNodes PosToken (Expr Name)
deriving anyclass instance ToConcreteNodes PosToken (LocalDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (NamedExpr Name)
deriving anyclass instance ToConcreteNodes PosToken (Branch Name)
deriving anyclass instance ToConcreteNodes PosToken (Pattern Name)
deriving anyclass instance ToConcreteNodes PosToken (TypeSig Name)
deriving anyclass instance ToConcreteNodes PosToken (GivethSig Name)
deriving anyclass instance ToConcreteNodes PosToken (GivenSig Name)
deriving anyclass instance ToConcreteNodes PosToken (Directive Name)
deriving anyclass instance ToConcreteNodes PosToken (Import Name)

instance ToConcreteNodes PosToken (Module Name) where
  toNodes (MkModule ann _ secs) = flattenConcreteNodes ann [toNodes secs]

-- Generic instance does not apply because we exclude the level.
instance ToConcreteNodes PosToken (Section Resolved) where
  toNodes (MkSection ann name maka decls) =
    flattenConcreteNodes ann [toNodes name, toNodes maka, toNodes decls]

deriving anyclass instance ToConcreteNodes PosToken (TopDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Assume Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Declare Resolved)
deriving anyclass instance ToConcreteNodes PosToken (TypeDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (ConDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Type' Resolved)
deriving anyclass instance ToConcreteNodes PosToken (TypedName Resolved)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyTypedName Resolved)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyNamedType Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Decide Resolved)
deriving anyclass instance ToConcreteNodes PosToken (AppForm Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Aka Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Expr Resolved)
deriving anyclass instance ToConcreteNodes PosToken (LocalDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (NamedExpr Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Branch Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Pattern Resolved)
deriving anyclass instance ToConcreteNodes PosToken (TypeSig Resolved)
deriving anyclass instance ToConcreteNodes PosToken (GivethSig Resolved)
deriving anyclass instance ToConcreteNodes PosToken (GivenSig Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Directive Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Import Resolved)
instance ToConcreteNodes PosToken (Module Resolved) where
  toNodes (MkModule ann _ secs) = flattenConcreteNodes ann [toNodes secs]


data Comment = MkComment Anno [Text]
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- | Natural Language Generation Annotation type.
--
-- A NLG annotation has three distinct states:
--
-- * @Invalid@ 'MkInvalidNlg'.
-- * @Parsed@ 'MkParsedNlg'.
--   A valid annotation which we have processed to figure out the
--   'Name's that occur in it.
-- * @Resolved@ 'MkResolvedNlg'.
--   A resolved annotation is a 'MkParsedNlg' annotation which contained only
--   resolvable 'Name's.
data Nlg =
    MkInvalidNlg Anno
    -- ^ This is an invalid annotation, we failed to parse it further.
    -- This means it likely has mismatching '%' tokens or the 'name' parser failed.
  | MkParsedNlg Anno [NlgFragment Name]
    -- ^ An annotation where we extracted the 'Name's that are mentioned.
  | MkResolvedNlg Anno [NlgFragment Resolved]
    -- ^ Same as 'MkParsedNlg', but we have additionally typechecked and resolved the
    -- annotation.
    -- Typechecking merely means we have performed scope checking.
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data NlgFragment n
  = MkNlgText Anno Text
  | MkNlgRef  Anno n
  deriving stock (Show, Eq, GHC.Generic, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data Ref = MkRef Anno Text
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

deriving via L4Syntax Nlg
  instance HasAnno Nlg
deriving via L4Syntax (NlgFragment n)
  instance HasAnno (NlgFragment n)
deriving via L4Syntax Comment
  instance HasAnno Comment
deriving via L4Syntax Ref
  instance HasAnno Ref

instance ToConcreteNodes PosToken Comment where
  toNodes (MkComment ann _) = flattenConcreteNodes ann []

deriving anyclass instance ToConcreteNodes PosToken Nlg

instance ToConcreteNodes PosToken n => ToConcreteNodes PosToken (NlgFragment n) where
  toNodes = \case
    MkNlgText ann _ -> flattenConcreteNodes ann []
    MkNlgRef ann n -> flattenConcreteNodes ann [toNodes n]

instance ToConcreteNodes PosToken Int where
  toNodes _txt = pure []
  -- TODO: This instance is lossy, i.e., it drops information.
  --
  -- Ints currently appear in two places of the syntax tree:
  --
  -- In types, as inference variables.
  -- In names applications, to store the inferred order of applications.
  --
  -- In both cases, the Ints are only added after scope / type-checking,
  -- and should not be relevant for exact-printing.
  --
  -- We could either be more precise about what occurs in which phase,
  -- or we could also possibly be more precise about the types, and not
  -- use plain Int, but some newtype-wrapped type.

instance ToConcreteNodes PosToken Name where
  toNodes (MkName ann _) =
    flattenConcreteNodes ann []

instance ToConcreteNodes PosToken Resolved where
  toNodes r =
    toNodes (getActual r)

instance ToConcreteNodes PosToken RawName where
  toNodes _rn = pure []
  -- TODO: This is lossy and should be improved (but we should not need to
  -- exact-print inference variables).

instance ToConcreteNodes PosToken Lit where
  toNodes (NumericLit ann _) =
    flattenConcreteNodes ann []
  toNodes (StringLit ann _) =
    flattenConcreteNodes ann []

deriving anyclass instance HasSrcRange (Module a)
deriving anyclass instance HasSrcRange (Section a)
deriving anyclass instance HasSrcRange (TopDecl a)
deriving anyclass instance HasSrcRange (Assume a)
deriving anyclass instance HasSrcRange (Declare a)
deriving anyclass instance HasSrcRange (TypeDecl a)
deriving anyclass instance HasSrcRange (ConDecl a)
deriving anyclass instance HasSrcRange (Type' a)
deriving anyclass instance HasSrcRange (TypedName a)
deriving anyclass instance HasSrcRange (OptionallyTypedName a)
deriving anyclass instance HasSrcRange (OptionallyNamedType a)
deriving anyclass instance HasSrcRange (Decide a)
deriving anyclass instance HasSrcRange (AppForm a)
deriving anyclass instance HasSrcRange (Aka a)
deriving anyclass instance HasSrcRange (Expr a)
deriving anyclass instance HasSrcRange (LocalDecl a)
deriving anyclass instance HasSrcRange (NamedExpr a)
deriving anyclass instance HasSrcRange (Branch a)
deriving anyclass instance HasSrcRange (Pattern a)
deriving anyclass instance HasSrcRange (TypeSig a)
deriving anyclass instance HasSrcRange (GivethSig a)
deriving anyclass instance HasSrcRange (GivenSig a)
deriving anyclass instance HasSrcRange (Directive a)
deriving anyclass instance HasSrcRange (Import a)
deriving anyclass instance HasSrcRange Lit
deriving anyclass instance HasSrcRange Name
deriving anyclass instance HasSrcRange Nlg
deriving anyclass instance HasSrcRange (NlgFragment n)
deriving anyclass instance HasSrcRange Comment
deriving anyclass instance HasSrcRange Ref

instance HasSrcRange Resolved where
  rangeOf = rangeOf . getActual

-- | this can be used to get rid of annotations to clean up printing
clearAnno :: GPlate Anno a => a -> a
clearAnno = set (gplate @Anno) emptyAnno
