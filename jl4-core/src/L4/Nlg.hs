module L4.Nlg (
  simpleLinearizer,
  Linearize (..),
  lin,
) where

import Base
import qualified Base.Text as Text

import L4.Annotation
import L4.Lexer (PosToken)
import L4.Syntax
import L4.Utils.Ratio (prettyRatio)
import L4.Desugar
import Optics

-- | Convert a deontic modal to its text representation for NLG
deonticModalText :: DeonticModal -> Text
deonticModalText = \case
  DMust    -> "must"
  DMay     -> "may"
  DMustNot -> "must not"
  DDo      -> "do"

-- TODO: I would like to be able to attach meta information and
-- to be able to tell apart variables, parameters and global definitions.
-- So, perhaps we rather want a 'Doc' type?
-- Or, like GF, a typed LinTree type that performs pre-analysis steps
data LinToken = MkLinToken
  { payload :: Text
  , type' :: LinType
  }
  deriving stock (Show, Eq, Ord, Generic)

data LinType
  = LinText
  | LinVar
  | LinUser
  | LinPossessive
  | LinPunctuation
  deriving stock (Show, Eq, Ord, Generic)

newtype LinTree = MkLinTree
  { tokens :: [LinToken]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid)

instance IsString LinTree where
  fromString = text . Text.pack

-- | Linearize an expression into plain text.
-- This linearizer does not attempt to do any smart operations, such as capitalization.
simpleLinearizer :: Linearize a => a -> Text
simpleLinearizer a =
  let
    tree = linearize a

    sp :: Text
    sp = " "

    prettyLinTok :: LinToken -> Text
    prettyLinTok t = case t.type' of
      LinPossessive -> "'" <> t.payload
      LinPunctuation -> t.payload <> sp
      LinUser -> t.payload
      LinVar -> "`" <> t.payload <> "`"
      LinText -> t.payload
  in
    case tree.tokens of
      [] -> ""
      (x:xs) -> Text.stripStart (prettyLinTok x) <> mconcat (fmap prettyLinTok xs)

-- | Translate an 'a' to something that can be linearized.
class Linearize a where
  linearize :: a -> LinTree

instance Linearize (Expr Resolved) where
  linearize expr = case carameliseNode expr of
    And _ e1 e2 -> hcat
      [ lin e1
      , text "and"
      , lin e2
      ]
    Or _ e1 e2 -> hcat
      [ lin e1
      , text "or"
      , lin e2
      ]
    RAnd _ e1 e2 -> hcat
      [ lin e1
      , text "and"
      , lin e2
      ]
    ROr _ e1 e2 -> hcat
      [ lin e1
      , text "or"
      , lin e2
      ]
    Implies _ e1 e2 -> hcat
      [ lin e1
      , text "implies"
      , lin e2
      ]
    Equals _ e1 e2 -> hcat
      [ lin e1
      , text "is"
      , text "equal"
      , text "to"
      , lin e2
      ]
    Not _ e -> hcat
      [ text "not"
      , lin e
      ]
    Plus _ e1 e2 -> hcat
      [ text "the"
      , text "sum"
      , text "of"
      , lin e1
      , text "and"
      , lin e2
      ]
    Minus _ e1 e2 -> hcat
      [ text "the"
      , text "difference"
      , text "between"
      , lin e2
      , text "and"
      , lin e1
      ]
    Times _ e1 e2 -> hcat
      [ text "the"
      , text "product"
      , text "of"
      , lin e1
      , text "and"
      , lin e2
      ]
    DividedBy _ e1 e2 -> hcat
      [ text "the"
      , text "result"
      , text "of"
      , text "dividing"
      , lin e1
      , text "by"
      , lin e2
      ]
    Modulo _ e1 e2 -> hcat
      [ text "the"
      , text "result"
      , text "of"
      , lin e1
      , text "modulo"
      , lin e2
      ]
    Exponent _ e1 e2 -> hcat
      [ lin e1
      , text "to"
      , text "the"
      , text "power"
      , text "of"
      , lin e2
      ]
    Cons _ e1 e2 -> hcat
      [ lin e1
      , text "followed"
      , text "by"
      , lin e2
      ]
    Leq _ e1 e2 -> hcat
      [ lin e1
      , text "is"
      , text "at"
      , text "most"
      , lin e2
      ]
    Geq _ e1 e2 -> hcat
      [ lin e1
      , text "is"
      , text "at"
      , text "least"
      , lin e2
      ]
    Lt _ e1 e2 -> hcat
      [ lin e1
      , text "is"
      , text "less"
      , text "than"
      , lin e2
      ]
    Gt _ e1 e2 -> hcat
      [ lin e1
      , text "is"
      , text "greater"
      , text "than"
      , lin e2
      ]
    Proj _ e1 e2 -> hcat
      [ lin e1
      , possessive "s"
      , linearize e2
      ]
    Var _ v -> linearize v
    Lam _ sig e -> hcat
      [ lin sig
      , text "then"
      , lin e
      ]
    App _ n es -> hcat $
      [ linearize n
      ]
      <> ifNonEmpty es
            [ text "with"
            , enumerate (punctuate ",") (spaced $ text "and") (fmap lin es)
            ]
    AppNamed _ n es _order -> hcat
      [ linearize n
      , text "where"
      , enumerate (punctuate ",") (spaced $ text "and") (fmap lin es)
      ]
    IfThenElse _ cond then' else' -> hcat
      [ text "if"
      , lin cond
      , text "then"
      , lin then'
      , text "else"
      , lin else'
      ]
    MultiWayIf _ conds o -> hcat $
      foldMap (\(MkGuardedExpr _ c f) -> ["if", lin c, "then", lin f]) conds
      <> ["otherwise", lin o ]
    Regulative _ (MkObligation _ party (MkAction _ modal rule mprovided) mdeadline mfollowup mlest) -> hcat $
      [ text "party"
      , lin party
      , text (deonticModalText modal)
      , lin rule
      ]
      <> maybe [] (\ provided -> [ text "provided that", lin provided ]) mprovided
      <> maybe [] (\ deadline -> [ text "within", lin deadline ]) mdeadline
      <> maybe [] (\ followup -> [ text "hence",  lin followup ]) mfollowup
      <> maybe [] (\ lest -> [ text "lest",  lin lest ]) mlest
    Consider _ e br -> hcat
      [ text "consider"
      , text "the"
      , text "case"
      , text "distinctions"
      , text "of"
      , lin e
      , punctuate ":"
      , enumerate (punctuate ".") (punctuate ".") (fmap lin br)
      ]
    Lit _ l -> lin l
    Percent _ l -> hcat [lin l, punctuate "%"]
    List _ es -> hcat
      [ text "list"
      , text "of"
      , enumerate (punctuate ",") (spaced $ text "and") (fmap lin es)
      ]
    Where _ e lcl -> hcat
      [ lin e
      , text "where"
      , enumerate (punctuate ",") (spaced $ text "and") (fmap lin lcl)
      ]
    LetIn _ lcl e -> hcat
      [ text "let"
      , enumerate (punctuate ",") (spaced $ text "and") (fmap lin lcl)
      , text "in"
      , lin e
      ]
    Event _ ev -> lin ev
    Fetch _ e -> hcat [ text "fetch", lin e ]
    Env _ e -> hcat [ text "environment variable", lin e ]
    Post _ e1 e2 e3 -> hcat [ text "post", lin e1, lin e2, lin e3 ]
    Concat _ exprs -> hcat [ text "concatenate", enumerate (punctuate ",") (spaced $ text "and") (fmap lin exprs) ]
    AsString _ e -> hcat [ lin e, text "as", text "string" ]
    Breach _ mParty mReason -> hcat $
      [ text "breach" ]
      <> maybe [] (\p -> [ text "by", lin p ]) mParty
      <> maybe [] (\r -> [ text "because", lin r ]) mReason
    Inert _ txt _ctx -> text txt

instance Linearize (Event Resolved) where
  linearize (MkEvent _ p a t _) = hcat
    [ "party", lin p, "did", lin a, "at", lin t]

instance Linearize (Directive Resolved) where
  linearize = \ case
    LazyEval _ e -> linearize e
    LazyEvalTrace _ e -> linearize e
    Check _ e -> linearize e
    Contract _ e t es -> hcat $
      [ "executing contract", lin e, "at", lin t, "with the following events: " ]
      <> map lin es
    Assert _ e -> linearize e


instance Linearize (NamedExpr Resolved) where
  linearize = \ case
    MkNamedExpr _ n e -> hcat
      [ linearize n
      , text "is"
      , lin e
      ]

instance Linearize (LocalDecl Resolved) where
  linearize = \ case
    LocalDecide _ _decide -> mempty
    LocalAssume _ _assume -> mempty

instance Linearize Lit where
  linearize = \ case
    NumericLit _ num -> text (prettyRatio num)
    StringLit _ t -> text t

instance Linearize (Branch Resolved) where
  linearize = \ case
    MkBranch _ (When _ pat) e -> hcat
      [ text "when"
      , lin pat
      , text "then"
      , lin e
      ]
    MkBranch _ (Otherwise _) e -> hcat
      [ text "in"
      , text "any"
      , text "other"
      , text "case"
      , lin e
      ]

instance Linearize (Pattern Resolved) where
  linearize = \ case
    PatVar _ v ->
      -- Resolved can't use 'lin', as it doesn't have an 'Anno'
      linearize v
    PatApp _ constructor pats -> hcat
      [ -- Resolved can't use 'lin', as it doesn't have an 'Anno'
        linearize constructor
      , text "has"
      , enumerate (punctuate ",") (spaced $ text "and") (fmap lin pats)
      ]
    PatCons _ start rest -> hcat
      [ lin start
      , text "is"
      , text "followed"
      , text "by"
      , lin rest
      ]
    PatExpr _ expr -> hcat [ "is", "exactly", lin expr ]
    PatLit _ lit -> hcat [ lin lit ]

instance Linearize (GivenSig Resolved) where
  linearize = \ case
    MkGivenSig _ args -> hcat
      [ text "given"
      , enumerate (punctuate ",") (spaced $ text "and") (fmap lin args)
      ]

instance Linearize (OptionallyTypedName Resolved) where
  linearize = \ case
    MkOptionallyTypedName _ name _mty -> hcat
      [ linearize name
      ]

instance Linearize Name where
  linearize = var . nameToText

instance Linearize Resolved where
  linearize = \ case
    Def _ name -> lin name
    Ref ref _ original
      | hasNlgAnnotation original && not (hasNlgAnnotation ref) ->
          -- If the binding has an NLG annotation, but the use-site does not
          -- have an NLG annotation, we use the NLG annotation of the binding.
        lin original
      | otherwise ->
        -- In all other cases, we just use the NLG annotation of the use-site.
        -- This behaves correctly when there is no NLG annotation at all.
        lin ref
    OutOfScope _ n -> lin n
   where
    hasNlgAnnotation name = isJust $ name ^. annoOf % annNlg

instance Linearize Nlg where
  linearize = \ case
    MkInvalidNlg _ -> text "(internal error)"
    MkParsedNlg _ frags -> foldMap linParsedFragment frags
    MkResolvedNlg _ frags -> foldMap linResolvedFragment frags
   where
    linParsedFragment :: NlgFragment Name -> LinTree
    linParsedFragment = \ case
      MkNlgText _ t -> user t
      MkNlgRef  _ n -> linearize n

    linResolvedFragment :: NlgFragment Resolved -> LinTree
    linResolvedFragment = \ case
      MkNlgText _ t -> user t
      MkNlgRef  _ n -> linearize n

hcat :: [LinTree] -> LinTree
hcat = mconcat . intersperse space

space :: LinTree
space = text " "

ifNonEmpty :: Monoid m => [a] -> m -> m
ifNonEmpty [] _ = mempty
ifNonEmpty (_:_) f = f

-- | 'lin' is like 'linearize', but first checks whether the 'a' has any 'Nlg'
-- annotations associated with it. If it does, then the 'Nlg' annotations
-- replaces the linearization of 'a'.
--
-- 'Resolved' can't use 'lin', as it doesn't have an 'Anno'
lin :: (HasAnno a, AnnoExtra a ~ Extension, AnnoToken a ~ PosToken, Linearize a) => a -> LinTree
lin a
  | Just nlg <- a ^. annoOf % annNlg =
      linearize nlg
  | otherwise =
      linearize a

text :: Text -> LinTree
text t = MkLinTree
  [ MkLinToken
    { type' = LinText
    , payload = t
    }
  ]

var :: Text -> LinTree
var t = MkLinTree
  [ MkLinToken
    { type' = LinVar
    , payload = t
    }
  ]

user :: Text -> LinTree
user t = MkLinTree
  [ MkLinToken
    { type' = LinUser
    , payload = t
    }
  ]

possessive :: Text -> LinTree
possessive t = MkLinTree
  [ MkLinToken
    { type' = LinPossessive
    , payload = t
    }
  ]

punctuate :: Text -> LinTree
punctuate t = MkLinTree
  [ MkLinToken
    { type' = LinPunctuation
    , payload = t
    }
  ]

enumerate :: LinTree -> LinTree -> [LinTree] -> LinTree
enumerate _   lastSep [x, y] = mconcat [x, lastSep, y]
enumerate _   _       [x]    = x
enumerate _   _       []     = mempty
enumerate sep lastSep (x:xs) = x <> sep <> enumerate sep lastSep xs

spaced :: LinTree -> LinTree
spaced p = text " " <> p <> text " "
