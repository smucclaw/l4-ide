module L4.Nlg (
  simpleLinearizer,
  Linearize (..),
  lin,
) where

import Base
import qualified Base.Text as Text

import GHC.Generics (Generically (..))
import L4.Annotation
import L4.Lexer (PosToken)
import L4.Syntax
import Optics

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

data LinTree = MkLinTree
  { tokens :: [LinToken]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via Generically LinTree

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
  linearize = \case
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
    Lam _ _ _ -> mempty
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

instance Linearize (NamedExpr Resolved) where
  linearize = \case
    MkNamedExpr _ n e -> hcat
      [ linearize n
      , text "is"
      , lin e
      ]

instance Linearize (LocalDecl Resolved) where
  linearize = \case
    LocalDecide _ _decide -> mempty
    LocalAssume _ _assume -> mempty

instance Linearize Lit where
  linearize = \case
    NumericLit _ num -> text (Text.show num)
    StringLit _ t -> text t

instance Linearize (Branch Resolved) where
  linearize = \case
    When _ pat e -> hcat
      [ text "when"
      , lin pat
      , text "then"
      , lin e
      ]
    Otherwise _ e -> hcat
      [ text "in"
      , text "any"
      , text "other"
      , text "case"
      , lin e
      ]

instance Linearize (Pattern Resolved) where
  linearize = \case
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

instance Linearize Name where
  linearize = var . nameToText

instance Linearize Resolved where
  linearize = \case
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
  linearize = \case
    MkInvalidNlg _ -> text "(internal error)"
    MkParsedNlg _ frags -> foldMap linParsedFragment frags
    MkResolvedNlg _ frags -> foldMap linResolvedFragment frags
   where
    linParsedFragment :: NlgFragment Name -> LinTree
    linParsedFragment = \case
      MkNlgText _ t -> user t
      MkNlgRef  _ n -> linearize n

    linResolvedFragment :: NlgFragment Resolved -> LinTree
    linResolvedFragment = \case
      MkNlgText _ t -> user t
      MkNlgRef  _ n ->
        -- Don't linearize 'Resolved', since the we are reaching this
        -- code path by linearising the 'Nlg' annotation of an 'Resolved'.
        -- TODO: this might be wrong when referencing 'Name's within the 'Nlg' annotation.
        linearize (getActual n)

hcat :: [LinTree] -> LinTree
hcat = mconcat . intersperse (text " ")

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
