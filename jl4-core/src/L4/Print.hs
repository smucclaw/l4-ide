module L4.Print where

import Base
import qualified Base.Text as Text
import L4.Evaluate.ValueLazy as Lazy
import L4.Syntax

import Data.Char
import Data.Time (toGregorian)
import Prettyprinter
import Prettyprinter.Render.Text
import qualified Data.List.NonEmpty as NE
import L4.Utils.Ratio (prettyRatio)
import L4.Evaluate.Operators
import L4.Names
import L4.Desugar
import Control.Category ((>>>))
import System.FilePath (takeFileName)

prettyLayout :: LayoutPrinter a => a -> Text
prettyLayout a = docText $ printWithLayout a

docText :: Doc ann -> Text
docText = renderStrict . layoutPretty (LayoutOptions Unbounded)

-- | Hack to get the lines of a document as 'Doc'. Used for the trace printer
-- and in situations where we need to prefix all the lines with something else.
--
docLines :: Doc ann -> [Doc ann]
docLines = fmap pretty . Text.lines . docText

prettyLayout' :: LayoutPrinter a => a -> String
prettyLayout' = Text.unpack . prettyLayout

quotedName :: Name -> Text
quotedName n =
    renderStrict
  $ layoutPretty (LayoutOptions Unbounded)
  $ pretty . quote . rawNameToText . rawName $ n

class LayoutPrinter a where
  printWithLayout :: a -> Doc ann
  parensIfNeeded :: a -> Doc ann
  parensIfNeeded = printWithLayout

type LayoutPrinterWithName name = (LayoutPrinter name, HasName name)

instance LayoutPrinter Name where
  printWithLayout n = printWithLayout (rawName n)

instance LayoutPrinter Resolved where
  printWithLayout r = printWithLayout (getActual r)

instance LayoutPrinter RawName where
  printWithLayout = \ case
    NormalName t -> pretty $ quoteIfNeeded t
    QualifiedName qs t -> pretty t <+> parens ("qualified at section" <+> pretty (Text.intercalate "." $ NE.toList qs))
    PreDef t -> pretty $ quoteIfNeeded t

instance LayoutPrinterWithName a => LayoutPrinter (Type' a) where
  printWithLayout = \ case
    Type _ -> "TYPE"
    TyApp _ n ps -> printWithLayout n <> case ps of
      [] -> mempty
      params@(_:_) -> space <> "OF" <+> hsep (punctuate comma (fmap printWithLayout params))
    Fun _ args ty ->
      "FUNCTION FROM" <+> hsep (punctuate (space <> "AND") (fmap printWithLayout args))
        <+> "TO" <+> printWithLayout ty
    Forall _ vals ty -> "FOR ALL" <+> hsep (punctuate (space <> "AND") (fmap printWithLayout vals))
        <+> printWithLayout ty
    InfVar _ raw uniq -> printWithLayout raw <> pretty uniq

-- We currently have no syntax for actual names occurring here
instance LayoutPrinterWithName a => LayoutPrinter (OptionallyNamedType a) where
  printWithLayout = \ case
    MkOptionallyNamedType _ _ ty ->
      printWithLayout ty

instance LayoutPrinterWithName a => LayoutPrinter (OptionallyTypedName a) where
  printWithLayout = \ case
    MkOptionallyTypedName _ a ty ->
      printWithLayout a <> case ty of
        Nothing -> mempty
        Just ty' -> space <> "IS" <+> printWithLayout ty'

instance LayoutPrinterWithName a => LayoutPrinter (TypedName a) where
  printWithLayout = \ case
    MkTypedName _ a ty -> printWithLayout a <+> "IS" <+> printWithLayout ty

instance LayoutPrinterWithName a => LayoutPrinter (TypeSig a) where
  printWithLayout = \ case
    MkTypeSig _ given mGiveth ->
      case given of
        MkGivenSig _ [] -> case mGiveth of
          Just giveth -> printWithLayout giveth
          Nothing -> mempty
        MkGivenSig _ (_:_) ->
          printWithLayout given <> case mGiveth of
            Just giveth -> line <> printWithLayout giveth
            Nothing -> mempty

instance LayoutPrinterWithName a => LayoutPrinter (GivenSig a) where
  printWithLayout = \ case
    MkGivenSig _ ns -> case ns of
      [] -> mempty
      names@(_:_) -> "GIVEN" <+> align (vsep (fmap printWithLayout names))

instance LayoutPrinterWithName a => LayoutPrinter (GivethSig a) where
  printWithLayout = \ case
    MkGivethSig _ ty -> "GIVETH" <+> printWithLayout ty

instance LayoutPrinterWithName a => LayoutPrinter (Declare a) where
  printWithLayout = \ case
    MkDeclare _ tySig appForm tyDecl  ->
      fillCat
        [ printWithLayout tySig
        ]
      <>
      vcat
        [ "DECLARE" <+> printWithLayout appForm
        , indent 2 (printWithLayout tyDecl)
        ]

instance LayoutPrinterWithName a => LayoutPrinter (AppForm a) where
  printWithLayout = \ case
    MkAppForm _ n ns maka ->
      (printWithLayout n <> case ns of
        [] -> mempty
        _ -> space <> hsep (fmap printWithLayout ns)
      ) <>
      case maka of
        Nothing  -> mempty
        Just aka -> space <> printWithLayout aka

instance LayoutPrinterWithName a => LayoutPrinter (Aka a) where
  printWithLayout = \ case
    MkAka _ ns -> "AKA" <+> vsep (punctuate comma $ fmap printWithLayout ns)

instance LayoutPrinterWithName a => LayoutPrinter (TypeDecl a) where
  printWithLayout = \ case
    RecordDecl _ _ fields  ->
      vcat
        [ "HAS"
        , indent 2 (vsep (fmap printWithLayout fields))
        ]
    EnumDecl _ enums ->
      vcat
        [ "IS ONE OF"
        , indent 2 (vsep (fmap printWithLayout enums))
        ]
    SynonymDecl _ t ->
      vcat
        [ "IS"
        , indent 2 (printWithLayout t)
        ]

instance LayoutPrinterWithName a => LayoutPrinter (ConDecl a) where
  printWithLayout = \ case
    MkConDecl _ n fields  ->
      printWithLayout n <> case fields of
        [] -> mempty
        _:_ -> space <> "HAS" <+> vsep (punctuate comma $ fmap printWithLayout fields)

instance LayoutPrinterWithName a => LayoutPrinter (Assume a) where
  printWithLayout = \ case
    MkAssume _ tySig appForm ty ->
      fillCat
        [ printWithLayout tySig
        , "ASSUME" <+> printWithLayout appForm <> case ty of
            Nothing -> mempty
            Just ty' -> space <> "IS" <+> printWithLayout ty'
        ]

instance LayoutPrinterWithName a => LayoutPrinter (Decide a) where
  printWithLayout = \ case
    MkDecide _ tySig appForm expr ->
      vcat
        [ printWithLayout tySig
        , "DECIDE" <+> printWithLayout appForm <+> "IS"
        , indent 2 (printWithLayout expr)
        ]

instance LayoutPrinterWithName a => LayoutPrinter (Directive a) where
  printWithLayout = \ case
    LazyEval _ e ->
      "#EVAL" <+> printWithLayout e
    LazyEvalTrace _ e ->
      "#EVALTRACE" <+> printWithLayout e
    Check _ e ->
      "#CHECK" <+> printWithLayout e
    Contract _ e t stmts -> hsep $
      "#TRACE" <+> printWithLayout e <+> printWithLayout t :
      map printWithLayout stmts
    Assert _ e ->
      "#ASSERT" <+> printWithLayout e

instance LayoutPrinterWithName a => LayoutPrinter (Import a) where
  printWithLayout = \ case
    MkImport _ n _mr -> "IMPORT" <+> printWithLayout n

instance (LayoutPrinterWithName a, n ~ Int) => LayoutPrinter (n, Section a) where
  printWithLayout = \ case
    (i, MkSection _ Nothing _ ds)    ->
      vcat (map (printWithLayout . (i + 1 ,)) ds)
    (i, MkSection _ name maka ds) ->
      vcat $
        [ pretty (replicate i '§') <+>
          case maka of
            Nothing  -> maybe mempty printWithLayout name
            Just aka -> maybe mempty printWithLayout name <+> printWithLayout aka
        ]
        <> case ds of
          [] -> mempty
          _ -> map (printWithLayout . (i + 1 ,)) ds

instance LayoutPrinterWithName a => LayoutPrinter (Module  a) where
  printWithLayout = \ case
    MkModule _ _ sect -> printWithLayout (1, sect)

instance LayoutPrinterWithName a => LayoutPrinter (TopDecl a) where
  printWithLayout t = printWithLayout (1, t)

instance (LayoutPrinterWithName a, n ~ Int) => LayoutPrinter (n, TopDecl a) where
  printWithLayout = \ case
    (_, Declare   _ t) -> printWithLayout t
    (_, Decide    _ t) -> printWithLayout t
    (_, Assume    _ t) -> printWithLayout t
    (_, Directive _ t) -> printWithLayout t
    (_, Import    _ t) -> printWithLayout t
    (i, Section   _ t) -> printWithLayout (i, t)

instance LayoutPrinterWithName a => LayoutPrinter (Expr a) where
  printWithLayout :: LayoutPrinter a => Expr a -> Doc ann
  printWithLayout = carameliseNode >>> \ case
    e@And{} ->
      let
        conjunction = scanAnd e
      in
        prettyConj "AND" (fmap printWithLayout conjunction)
    e@Or{} ->
      let
        disjunction = scanOr e
      in
        prettyConj "OR" (fmap printWithLayout disjunction)
    e@RAnd{} ->
      let
        conjunction = scanRAnd e
      in
        prettyConj "RAND" (fmap printWithLayout conjunction)
    e@ROr{} ->
      let
        disjunction = scanROr e
      in
        prettyConj "ROR" (fmap printWithLayout disjunction)
    Implies    _ e1 e2 ->
      parensIfNeeded e1 <+> "IMPLIES" <+> parensIfNeeded e2
    Equals     _ e1 e2 ->
      parensIfNeeded e1 <+> "EQUALS" <+> parensIfNeeded e2
    Not        _ e1 ->
      "NOT" <+> printWithLayout e1
    Plus       _ e1 e2 ->
      parensIfNeeded e1 <+> "PLUS" <+> parensIfNeeded e2
    Minus      _ e1 e2 ->
      parensIfNeeded e1 <+> "MINUS" <+> parensIfNeeded e2
    Times      _ e1 e2 ->
      parensIfNeeded e1 <+> "TIMES" <+> parensIfNeeded e2
    DividedBy  _ e1 e2 ->
      parensIfNeeded e1 <+> "DIVIDED" <+> parensIfNeeded e2
    Modulo     _ e1 e2 ->
      parensIfNeeded e1 <+> "MODULO" <+> parensIfNeeded e2
    Exponent   _ e1 e2 ->
      parensIfNeeded e1 <+> "TO THE POWER OF" <+> parensIfNeeded e2
    Cons       _ e1 e2 ->
      parensIfNeeded e1 <+> "FOLLOWED BY" <+> parensIfNeeded e2
    Leq        _ e1 e2 ->
      parensIfNeeded e1 <+> "AT MOST" <+> parensIfNeeded e2
    Geq        _ e1 e2 ->
      parensIfNeeded e1 <+> "AT LEAST" <+> parensIfNeeded e2
    Lt         _ e1 e2 ->
      parensIfNeeded e1 <+> "LESS THAN" <+> parensIfNeeded e2
    Gt         _ e1 e2 ->
      parensIfNeeded e1 <+> "GREATER THAN" <+> parensIfNeeded e2
    Proj       _ e1 n ->
      parensIfNeeded e1 <> "'s" <+> printWithLayout n
    Var        _ n ->
      printWithLayout n
    Lam        _ given expr ->
      printWithLayout given <+> "YIELD" <+> printWithLayout expr
    App        _ n es -> printWithLayout n <> case es of
      [] -> mempty
      exprs@(_:_) -> space <> "OF" <+> hsep (punctuate comma (fmap parensIfNeeded exprs))
    AppNamed   _ n namedExpr _ ->
          printWithLayout n
      <+> "WITH"
      <+> hang 2 (align (vcat (fmap printWithLayout namedExpr)))
    IfThenElse _ cond then' else' ->
      -- Use single-line format to avoid layout/indentation issues when re-parsing
      "IF" <+> parensIfNeeded cond
        <+> "THEN" <+> parensIfNeeded then'
        <+> "ELSE" <+> parensIfNeeded else'
    MultiWayIf _ conds o ->
      vcat $
        [ "BRANCH" ]
        <> map (\(MkGuardedExpr _ a b) -> "IF" <+> printWithLayout a <+> "THEN" <+> printWithLayout b) conds
        <> [ "OTHERWISE" <+> printWithLayout o ]
    Regulative _ (MkDeonton _ p a t f l) -> prettyObligation p a t f l
    Consider   _ expr branches ->
      "CONSIDER" <+> printWithLayout expr <+> hang 2 (vsep $ punctuate comma (fmap printWithLayout branches))

    Lit        _ lit -> printWithLayout lit
    Percent    _ expr -> parensIfNeeded expr <+> "%"
    List       _ exprs ->
      "LIST" <+> hsep (punctuate comma (fmap parensIfNeeded exprs))
    Where      _ e1 decls ->
      vcat
        [ indent 2 (printWithLayout e1)
        , "WHERE"
        , indent 2 (vsep $ fmap printWithLayout decls)
        ]
    LetIn _ decls e1 ->
      vcat
        [ "LET"
        , indent 2 (vsep $ fmap printLetBinding decls)
        , "IN"
        , indent 2 (printWithLayout e1)
        ]
    Event _ MkEvent {timestamp, party, action} ->
      vcat
        [ "PARTY" <+> printWithLayout party
        , "DOES" <+> printWithLayout action
        , "AT" <+> printWithLayout timestamp -- TODO: better timestamp rendering
        ]
    Fetch _ e ->
      "FETCH" <+> printWithLayout e
    Env _ e ->
      "ENV" <+> printWithLayout e
    Post _ e1 e2 e3 ->
      "POST" <+> printWithLayout e1 <+> printWithLayout e2 <+> printWithLayout e3
    Concat _ exprs ->
      "CONCAT" <+> hsep (punctuate comma (fmap parensIfNeeded exprs))
    AsString _ e ->
      parensIfNeeded e <+> "AS STRING"
    Breach _ mParty mReason ->
      "BREACH" <>
        maybe mempty (\p -> " BY" <+> printWithLayout p) mParty <>
        maybe mempty (\r -> " BECAUSE" <+> printWithLayout r) mReason
    Inert _ txt _ctx ->
      "..." <+> dquotes (pretty txt)

  parensIfNeeded :: LayoutPrinter a => Expr a -> Doc ann
  parensIfNeeded e = case e of
    Lit{} -> printWithLayout e
    App _ _ [] -> printWithLayout e
    Var{} -> printWithLayout e
    _ -> surround (printWithLayout e) "(" ")"

prettyObligation
  :: (LayoutPrinter p, LayoutPrinter a, LayoutPrinter t,  LayoutPrinter f, LayoutPrinter l)
  => p -> a ->  Maybe t -> Maybe f -> Maybe l -> Doc ann
prettyObligation p a t f l =
  vcat $
    [ "PARTY" <+> printWithLayout p
    , printWithLayout a
    ]
    <> mprint "WITHIN" t
    <> mprint "HENCE" f
    <> mprint "LEST" l

mprint :: (Foldable t, LayoutPrinter a) => Doc ann -> t a -> [Doc ann]
mprint kw = foldMap \x -> [kw <+> printWithLayout x]

instance LayoutPrinterWithName n => LayoutPrinter (RAction n) where
  printWithLayout MkAction {modal, action, provided} = hsep $
    [ printDeonticModal modal, printWithLayout action
    ]
    <> mprint "PROVIDED" provided

-- | Print deontic modal keyword
printDeonticModal :: DeonticModal -> Doc ann
printDeonticModal = \case
  DMust -> "MUST"
  DMay -> "MAY"
  DMustNot -> "MUST NOT"
  DDo -> "DO"

instance LayoutPrinterWithName a => LayoutPrinter (NamedExpr a) where
  printWithLayout = \ case
    MkNamedExpr _ name e ->
      printWithLayout name <+> "IS" <+> printWithLayout e

-- | Print a LocalDecl in LET context (without DECIDE keyword and without type signature)
-- Uses "BE" as the binding keyword in honour of The Beatles' "Let It Be"
printLetBinding :: LayoutPrinterWithName a => LocalDecl a -> Doc ann
printLetBinding = \ case
  LocalDecide _ (MkDecide _ _tySig appForm expr) ->
    Prettyprinter.group $ printWithLayout appForm <+> "BE" <+> Prettyprinter.align (printWithLayout expr)
  LocalAssume _ t -> printWithLayout t

instance LayoutPrinterWithName a => LayoutPrinter (LocalDecl a) where
  printWithLayout = \ case
    LocalDecide _ t -> printWithLayout t
    LocalAssume _ t -> printWithLayout t

instance LayoutPrinter Lit where
  printWithLayout = \ case
    NumericLit _ t -> pretty (prettyRatio t)
    StringLit _ t -> surround (pretty $ escapeStringLiteral t) "\"" "\""

instance LayoutPrinterWithName a => LayoutPrinter (BranchLhs a) where
  printWithLayout = \ case
    (When _ pat) -> "WHEN" <+> printWithLayout pat <+> "THEN"
    (Otherwise _) -> "OTHERWISE"

instance LayoutPrinterWithName a => LayoutPrinter (Branch a) where
  printWithLayout (MkBranch _ c e) = printWithLayout c <+> printWithLayout e

instance LayoutPrinterWithName a => LayoutPrinter (Pattern a) where
  printWithLayout = \ case
    PatVar _ n -> printWithLayout n
    PatApp _ n pats -> printWithLayout n <> hang 2 case pats of
      [] -> mempty
      pats'@(_:_) -> space <> vsep (fmap printWithLayout pats')
    PatCons _ patHead patTail -> printWithLayout patHead <+> "FOLLOWED BY" <+> printWithLayout patTail
    PatExpr _ expr -> "EXACTLY" <+> printWithLayout expr
    PatLit _ lit -> printWithLayout lit

instance LayoutPrinter Nlg where
  printWithLayout = \ case
    MkInvalidNlg _ -> "Invalid Nlg"
    MkParsedNlg _ frags -> prettyNlgs frags
    MkResolvedNlg _ frags -> prettyNlgs frags
    where
      prettyNlgs [] = mempty
      prettyNlgs [x@MkNlgRef{}] = printWithLayout x
      prettyNlgs (x@MkNlgRef{}:xs) = printWithLayout x <+> prettyNlgs xs
      prettyNlgs (x:xs) = printWithLayout x <> prettyNlgs xs

instance LayoutPrinter Desc where
  printWithLayout = \ case
    MkDesc _ann n -> pretty n

instance LayoutPrinterWithName a => LayoutPrinter (NlgFragment a) where
  printWithLayout = \ case
    MkNlgText _ t -> pretty t
    MkNlgRef  _ n -> "%" <> printWithLayout n <> "%"

instance (LayoutPrinter a, LayoutPrinter b) => LayoutPrinter (Either a b) where
  printWithLayout = either printWithLayout printWithLayout

instance LayoutPrinter a => LayoutPrinter (Lazy.Value a) where
  printWithLayout = \ case
    Lazy.ValNumber i               -> pretty (prettyRatio i)
    Lazy.ValString t               -> surround (pretty $ escapeStringLiteral t) "\"" "\""
    Lazy.ValDate day               ->
      let (y, m, d) = toGregorian day
      in "DATE OF" <+> hsep [pretty d <> ",", pretty m <> ",", pretty y]
    Lazy.ValNil                    -> "EMPTY"
    Lazy.ValCons v1 v2             -> "(" <> printWithLayout v1 <> " FOLLOWED BY " <> printWithLayout v2 <> ")" -- TODO: parens
    Lazy.ValClosure{}              -> "<function>"
    Lazy.ValNullaryBuiltinFun{}    -> "<builtin-function>"
    Lazy.ValUnaryBuiltinFun{}      -> "<builtin-function>"
    Lazy.ValBinaryBuiltinFun{}     -> "<function>"
    Lazy.ValTernaryBuiltinFun{}    -> "<builtin-function>"
    Lazy.ValPartialTernary{}       -> "<partial-function>"
    Lazy.ValPartialTernary2{}      -> "<partial-function>"
    Lazy.ValAssumed r              -> printWithLayout r
    Lazy.ValUnappliedConstructor r -> printWithLayout r
    Lazy.ValConstructor r vs       -> printWithLayout r <> case vs of
      [] -> mempty
      vals@(_:_) -> space <> "OF" <+> hsep (punctuate comma (fmap parensIfNeeded vals))
    Lazy.ValEnvironment _env       -> "<environment>"
    Lazy.ValBreached reason        -> vcat
      [ "DEONTIC BREACHED:"
      , indent 2 $ printWithLayout reason
      ]
    Lazy.ValObligation _env p a t f l -> case t of
      Left te -> prettyObligation p a te (Just f) l
      Right tv -> prettyObligation p a (Just tv) (Just f) l
    Lazy.ValROp _env op l r -> hsep
      [ printWithLayout l
      , case op of ValROr -> "OR"; ValRAnd -> "AND"
      , printWithLayout r
      ]
  parensIfNeeded :: Lazy.Value a -> Doc ann
  parensIfNeeded v = case v of
    Lazy.ValNumber{}               -> printWithLayout v
    Lazy.ValString{}               -> printWithLayout v
    Lazy.ValDate{}                 -> printWithLayout v
    Lazy.ValNil                    -> "EMPTY"
    Lazy.ValClosure{}              -> printWithLayout v
    Lazy.ValUnappliedConstructor{} -> printWithLayout v
    Lazy.ValAssumed{}              -> printWithLayout v
    Lazy.ValConstructor r []       -> printWithLayout r
    _ -> surround (printWithLayout v) "(" ")"

instance LayoutPrinter BinOp where
  printWithLayout = \ case
    BinOpPlus -> "PLUS"
    BinOpMinus -> "MINUS"
    BinOpTimes -> "TIMES"
    BinOpDividedBy -> "DIVIDED"
    BinOpModulo -> "MODULO"
    BinOpExponent -> "TO THE POWER OF"
    BinOpTrunc -> "TRUNC"
    BinOpCons -> "FOLLOWED BY"
    BinOpEquals -> "EQUALS"
    BinOpLeq -> "AT MOST"
    BinOpGeq -> "AT LEAST"
    BinOpLt -> "LESS THAN"
    BinOpGt -> "GREATER THAN"
    BinOpContains -> "CONTAINS"
    BinOpStartsWith -> "STARTSWITH"
    BinOpEndsWith -> "ENDSWITH"
    BinOpIndexOf -> "INDEXOF"
    BinOpSplit -> "SPLIT"
    BinOpCharAt -> "CHARAT"
    BinOpWhenLast -> "WHEN LAST"
    BinOpWhenNext -> "WHEN NEXT"
    BinOpValueAt -> "VALUE AT"

instance LayoutPrinter a => LayoutPrinter (ReasonForBreach a) where
  printWithLayout = \ case
    DeadlineMissed ev'party ev'action ev'time party action deadline -> vcat
      [ "party"
      , i2 $ printWithLayout ev'party
      , "who did action"
      , i2 $ printWithLayout ev'action
      , "at"
      , i2 $ pretty (prettyRatio ev'time)
      , "surpassed the deadline of party"
      , i2 $ printWithLayout party
      , "who had to do obligatory action"
      , i2 $ printWithLayout action
      , "before their deadline, which was at"
      , i2 $ pretty (prettyRatio deadline)
      ]
      where i2 = indent 2
    ExplicitBreach mParty mReason -> vcat $
      [ "BREACH" ]
      <> maybe [] (\p -> [ "BY" <+> printWithLayout p ]) mParty
      <> maybe [] (\r -> [ "BECAUSE" <+> printWithLayout r ]) mReason

instance LayoutPrinter Lazy.NF where
  printWithLayout = \ case
    Lazy.Omitted -> "..."
    Lazy.MkNF (ValCons v1 v2) -> "LIST" <+> printList v1 v2
    Lazy.MkNF v -> printWithLayout v
    where
      printList v1 (Lazy.MkNF (ValNil))                          = printWithLayout v1
      printList v1 (Lazy.MkNF (ValCons v2 v3))                   = printWithLayout v1 <> comma <+> printList v2 v3
      printList Lazy.Omitted Lazy.Omitted                        = "..."
      printList v1 Lazy.Omitted                                  = printWithLayout v1 <> comma <+> "..."
      printList v1 v                                             = printWithLayout v1 <> comma <+> printWithLayout v -- fallback, should not happen

  parensIfNeeded :: Lazy.NF -> Doc ann
  parensIfNeeded v = case v of
    MkNF (Lazy.ValNumber{})               -> printWithLayout v
    MkNF (Lazy.ValString{})               -> printWithLayout v
    MkNF Lazy.ValNil                      -> printWithLayout v
    MkNF (Lazy.ValClosure{})              -> printWithLayout v
    MkNF (Lazy.ValUnappliedConstructor{}) -> printWithLayout v
    MkNF (Lazy.ValAssumed{})              -> printWithLayout v
    MkNF (Lazy.ValConstructor r [])       -> printWithLayout r
    _ -> surround (printWithLayout v) "(" ")"

instance LayoutPrinter Reference where
  printWithLayout rf = printWithLayout rf.address

instance LayoutPrinter Address where
  printWithLayout (MkAddress u a) =
    let fullUri = (fromNormalizedUri u).getUri
        -- Extract just filename for consistent output across platforms
        fileName = case Text.stripPrefix "file://" fullUri of
            Just path -> Text.pack $ takeFileName $ Text.unpack path
            Nothing -> Text.pack $ takeFileName $ Text.unpack fullUri
    in "&" <> pretty a <> "@" <> pretty fileName

quoteIfNeeded :: Text.Text -> Text.Text
quoteIfNeeded n = case Text.uncons n of
  Nothing -> n
  Just (firstChar, _)
    -- If the identifier doesn't start with alpha, it must be quoted
    -- (L4 lexer requires unquoted identifiers to start with alphabetic char)
    | not (isAlpha firstChar) -> quote n
    -- Otherwise check if all chars are valid identifier chars
    | Text.all isIdentChar n -> n
    | otherwise -> quote n
  where
    -- Match lexer: identifiers can contain alphanumeric chars and underscores
    isIdentChar x = isAlphaNum x || x == '_'

quote :: Text.Text -> Text.Text
quote n = "`" <> n <> "`"

scanOp :: (forall r. Expr a -> (r, Expr a -> Expr a -> r) -> r) -> Expr a -> [Expr a]
scanOp match e = match e ([e], \e1 e2 -> scanOp match e1 <> scanOp match e2)

scanOr :: Expr a -> [Expr a]
scanOr = scanOp \case
  Or _ e1 e2 -> \t -> snd t e1 e2
  _ -> fst

scanAnd :: Expr a -> [Expr a]
scanAnd = scanOp \case
  And _ e1 e2 -> \t -> snd t e1 e2
  _ -> fst

scanROr :: Expr a -> [Expr a]
scanROr = scanOp \case
  ROr _ e1 e2 -> \t -> snd t e1 e2
  _ -> fst

scanRAnd :: Expr a -> [Expr a]
scanRAnd = scanOp \case
  RAnd _ e1 e2 -> \t -> snd t e1 e2
  _ -> fst

prettyConj :: Text -> [Doc ann] -> Doc ann
prettyConj _ [] = mempty
prettyConj cnj (d:ds) =
  -- Use group with softline so it prefers single-line when possible
  -- This avoids layout issues when the output is re-parsed
  Prettyprinter.group $ d <> mconcat [softline <> pretty cnj <+> x | x <- ds]


escapeStringLiteral :: Text -> Text
escapeStringLiteral = Text.concatMap (\ case
  '\"' -> "\\\""
  '\\' -> "\\\\"
  c -> Text.singleton c
  )
