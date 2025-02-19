module L4.Nlg where

import Base

import L4.Syntax
import L4.Annotation
import Optics
import L4.Print
import L4.Lexer (PosToken)
import GHC.Generics (Generically(..))
import qualified Data.Text as Text
import qualified L4.Parser as Parser
import L4.TypeCheck
import qualified Data.Text.IO as Text

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

debugAllChecksAndEvals :: FilePath -> IO ()
debugAllChecksAndEvals file = do
  input <- Text.readFile file
  case Parser.execProgramParser file input of
    Left errs -> Text.putStrLn $ Text.unlines $ fmap (.message) $ toList errs
    Right (prog, _) ->
      case doCheckProgram prog of
        CheckResult {errors, program} -> do
          Text.putStrLn $ Text.unlines (map (\ err -> prettySrcRange file (rangeOf err) <> ":\n" <> prettyCheckErrorWithContext err) errors)

          let directives = toListOf (gplate @(Directive Resolved)) program
              checkExprs = mapMaybe (\case
                Eval _ e -> Just e
                Check _ e -> Just e
                ) directives

          traverse_ (Text.putStrLn . simpleLinearizer) checkExprs

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
      LinPunctuation -> t.payload
      LinUser -> sp <> "``" <> t.payload <> "``"
      LinVar -> sp <> "`" <> t.payload <> "`"
      LinText -> sp <> t.payload
  in
    case tree.tokens of
      [] -> ""
      (x:xs) -> Text.stripStart (prettyLinTok x) <> mconcat (fmap prettyLinTok xs)

-- | Translate an 'a' to something that can be linearized.
class Linearize a where
  linearize :: a -> LinTree

instance Linearize (Expr Resolved) where
  linearize = \case
    And _ e1 e2 -> mconcat
      [ lin e1
      , text "and"
      , lin e2
      ]
    Or _ e1 e2 -> mconcat
      [ lin e1
      , text "and"
      , lin e2
      ]
    Implies _ e1 e2 -> mconcat
      [ lin e1
      , text "and"
      , lin e2
      ]
    Equals _ e1 e2 -> mconcat
      [ lin e1
      , text "is"
      , text "equal"
      , text "to"
      , lin e2
      ]
    Not _ e -> mconcat
      [ text "not"
      , lin e
      ]
    Plus _ e1 e2 -> mconcat
      [ text "the"
      , text "sum"
      , text "of"
      , lin e1
      , text "and"
      , lin e2
      ]
    Minus _ e1 e2 -> mconcat
      [ text "the"
      , text "subtraction"
      , text "of"
      , lin e2
      , text "from"
      , lin e1
      ]
    Times _ e1 e2 -> mconcat
      [ text "the"
      , text "multiplication"
      , text "of"
      , lin e1
      , text "and"
      , lin e2
      ]
    DividedBy _ e1 e2 -> mconcat
      [ text "the"
      , text "division"
      , text "of"
      , lin e1
      , text "by"
      , lin e2
      ]
    Modulo _ e1 e2 -> mconcat
      [ text "the"
      , text "result"
      , text "of"
      , lin e1
      , text "modulo"
      , lin e2
      ]
    Cons _ e1 e2 -> mconcat
      [ lin e1
      , text "followed"
      , text "by"
      , lin e2
      ]
    Leq _ e1 e2 -> mconcat
      [ lin e1
      , text "is"
      , text "at"
      , text "most"
      , lin e2
      ]
    Geq _ e1 e2 -> mconcat
      [ lin e1
      , text "is"
      , text "at"
      , text "least"
      , lin e2
      ]
    Lt _ e1 e2 -> mconcat
      [ lin e1
      , text "is"
      , text "less"
      , text "than"
      , lin e2
      ]
    Gt _ e1 e2 -> mconcat
      [ lin e1
      , text "is"
      , text "greater"
      , text "than"
      , lin e2
      ]
    Proj _ e1 e2 -> mconcat
      [ lin e1
      , possessive "s"
      , -- Resolved can't use 'lin', as it doesn't have an 'Anno'
        linearize e2
      ]
    Var _ v -> linearize v
    Lam _ _ _ -> mempty
    App _ n es -> mconcat
      [ -- Resolved can't use 'lin', as it doesn't have an 'Anno'
        linearize n
      ]
      <> ifNonEmpty es (\ _ -> mconcat
            [ text "with"
            , enumerate (punctuate ",") (text "and") (fmap lin es)
            ])
    AppNamed _ n es _order -> mconcat
      [ -- Resolved can't use 'lin', as it doesn't have an 'Anno'
        linearize n
      , text "where"
      , enumerate (punctuate ",") (text "and") (fmap lin es)
      ]
    IfThenElse _ cond then' else' -> mconcat
      [ text "if"
      , lin cond
      , text "then"
      , lin then'
      , text "else"
      , lin else'
      ]
    Consider _ e br -> mconcat
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
    List _ es -> mconcat
      [ text "list"
      , text "of"
      , enumerate (punctuate ",") (text "and") (fmap lin es)
      ]
    Where _ e lcl -> mconcat
      [ lin e
      , text "where"
      , enumerate (punctuate ",") (text "and") (fmap lin lcl)
      ]

instance Linearize (NamedExpr Resolved) where
  linearize = \case
    MkNamedExpr _ n e -> mconcat
      [ -- Resolved can't use 'lin', as it doesn't have an 'Anno'
        linearize n
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
    When _ pat e -> mconcat
      [ text "when"
      , lin pat
      , text "then"
      , lin e
      ]
    Otherwise _ e -> mconcat
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
    PatApp _ constructor pats -> mconcat
      [ -- Resolved can't use 'lin', as it doesn't have an 'Anno'
        linearize constructor
      , text "has"
      , enumerate (punctuate ",") (text "and") (fmap lin pats)
      ]
    PatCons _ start rest -> mconcat
      [ lin start
      , text "is"
      , text "followed"
      , text "by"
      , lin rest
      ]

instance Linearize Name where
  linearize = \case
    MkName _ raw -> case raw of
      NormalName n -> var n
      PreDef n -> var n

instance Linearize Resolved where
  linearize = \case
    Def _ name -> lin name
    Ref actual _ original
      | Just _ <- actual ^. annoNlg
      , Nothing <- original ^. annoNlg
       -> lin actual
      | otherwise -> lin original
    OutOfScope _ n -> lin n

ifNonEmpty :: [a] -> (NonEmpty a -> LinTree) -> LinTree
ifNonEmpty [] _ = mempty
ifNonEmpty (x:xs) f = f (x :| xs)

-- | 'lin' is like 'linearize', but first checks whether the 'a' has any 'Nlg'
-- annotations associated with it. If it does, then the 'Nlg' annotations
-- replaces the linearization of 'a'.
lin :: (HasAnno a, AnnoExtra a ~ Extension, AnnoToken a ~ PosToken, Linearize a) => a -> LinTree
lin a
  | Just nlg <- (getAnno a) ^. annNlg =
      user (prettyLayout nlg)
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

