module L4.Print where

import L4.Syntax

import Base (Foldable (..))
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified L4.Parser as Parser
import Prettyprinter
import Prettyprinter.Render.Text

readAndPrint :: FilePath -> IO ()
readAndPrint fp = do
  input <- Text.readFile fp
  case Parser.execParser Parser.program fp input of
    Left errs -> Text.putStrLn $ Text.unlines $ fmap (.message) $ toList errs
    Right prog ->
      Text.putStrLn $ prettyLayout prog

prettyLayout :: LayoutPrinter a => a -> Text
prettyLayout a = renderStrict $ layoutPretty (LayoutOptions Unbounded) $ printWithLayout a

class LayoutPrinter a where
  printWithLayout :: a -> Doc ann

instance LayoutPrinter Name where
  printWithLayout (MkName _ rawName) = printWithLayout rawName

instance LayoutPrinter Resolved where
  printWithLayout r = printWithLayout (getActual r)

instance LayoutPrinter a => LayoutPrinter (Maybe a) where
  printWithLayout = \case
    Nothing -> mempty
    Just a -> printWithLayout a

instance LayoutPrinter RawName where
  printWithLayout = \case
    NormalName t -> pretty $ quoteIfNeeded t
    PreDef t -> pretty $ quoteIfNeeded t

instance LayoutPrinter a => LayoutPrinter (Type' a) where
  printWithLayout = \case
    Type _ -> "TYPE"
    TyApp _ n ps -> printWithLayout n <> case ps of
      [] -> mempty
      params@(_:_) -> space <> "OF" <+> hcat (punctuate comma (fmap printWithLayout params))
    Fun _ args ty ->
      "FUNCTION FROM" <> hcat (punctuate (space <> "AND") (fmap printWithLayout args))
        <+> "TO" <+> printWithLayout ty
    Forall _ vals ty -> "FOR ALL" <+> hcat (punctuate "AND" (fmap printWithLayout vals)) <+> "TO" <+> printWithLayout ty
    InfVar _ raw uniq -> printWithLayout raw <> pretty uniq

instance LayoutPrinter a => LayoutPrinter (OptionallyNamedType a) where
  printWithLayout = \case
    MkOptionallyNamedType _ a ty ->
      printWithLayout a <+> printWithLayout ty

instance LayoutPrinter a => LayoutPrinter (OptionallyTypedName a) where
  printWithLayout = \case
    MkOptionallyTypedName _ a ty ->
      printWithLayout a <> case ty of
        Nothing -> mempty
        Just ty' -> space <> "IS" <+> printWithLayout ty'

instance LayoutPrinter a => LayoutPrinter (TypedName a) where
  printWithLayout = \case
    MkTypedName _ a ty -> printWithLayout a <+> "IS" <+> printWithLayout ty

instance LayoutPrinter a => LayoutPrinter (TypeSig a) where
  printWithLayout = \case
    MkTypeSig _ given mGiveth ->
      case given of
        MkGivenSig _ [] -> case mGiveth of
          Just giveth -> printWithLayout giveth
          Nothing -> mempty
        MkGivenSig _ (_:_) ->
          printWithLayout given <> case mGiveth of
            Just giveth -> line <> printWithLayout giveth
            Nothing -> mempty

instance LayoutPrinter a => LayoutPrinter (GivenSig a) where
  printWithLayout = \case
    MkGivenSig _ ns -> case ns of
      [] -> mempty
      names@(_:_) -> "GIVEN" <+> align (vsep (fmap printWithLayout names))

instance LayoutPrinter a => LayoutPrinter (GivethSig a) where
  printWithLayout = \case
    MkGivethSig _ ty -> "GIVETH" <+> printWithLayout ty

instance LayoutPrinter a => LayoutPrinter (Declare a) where
  printWithLayout = \case
    MkDeclare _ tySig appForm tyDecl  ->
      fillCat
        [ printWithLayout tySig
        ]
      <>
      vcat
        [ "DECLARE" <+> printWithLayout appForm
        , indent 2 (printWithLayout tyDecl)
        ]

instance LayoutPrinter a => LayoutPrinter (AppForm a) where
  printWithLayout = \case
    MkAppForm _ n ns  ->
      printWithLayout n <> case ns of
        [] -> mempty
        _ -> space <> hsep (fmap printWithLayout ns)

instance LayoutPrinter a => LayoutPrinter (TypeDecl a) where
  printWithLayout = \case
    RecordDecl _ fields  ->
      vcat
        [ "HAS"
        , indent 2 (vsep (fmap printWithLayout fields))
        ]
    EnumDecl _ enums ->
      vcat
        [ "IS ONE OF"
        , indent 2 (vsep (fmap printWithLayout enums))
        ]
instance LayoutPrinter a => LayoutPrinter (ConDecl a) where
  printWithLayout = \case
    MkConDecl _ n fields  ->
      printWithLayout n <> case fields of
        [] -> mempty
        _:_ -> space <> "HAS" <+> vsep (punctuate comma $ fmap printWithLayout fields)

instance LayoutPrinter a => LayoutPrinter (Assume a) where
  printWithLayout = \case
    MkAssume _ tySig appForm ty ->
      fillCat
        [ printWithLayout tySig
        , "ASSUME" <+> printWithLayout appForm <> case ty of
            Nothing -> mempty
            Just ty' -> space <> "IS" <+> printWithLayout ty'
        ]

instance LayoutPrinter a => LayoutPrinter (Decide a) where
  printWithLayout = \case
    MkDecide _ tySig appForm expr ->
      vcat
        [ printWithLayout tySig
        , "DECIDE" <+> printWithLayout appForm <+> "IS"
        , indent 2 (printWithLayout expr)
        ]

instance LayoutPrinter a => LayoutPrinter (Directive a) where
  printWithLayout = \case
    Eval _ e ->
      "#EVAL" <+> printWithLayout e
    Check _ e ->
      "#CHECK" <+> printWithLayout e

instance LayoutPrinter a => LayoutPrinter (Section a) where
  printWithLayout = \case
    MkSection _ _  Nothing ds    ->
      vcat (fmap printWithLayout ds)
    MkSection _ ps name    ds ->
      vcat $
        [ plural mempty (pretty (replicate ps '§') <+> printWithLayout name) (0 :: Int)
        ]
        <> case ds of
          [] -> mempty
          _ -> fmap printWithLayout ds
instance LayoutPrinter a => LayoutPrinter (Program a) where
  printWithLayout = \case
    MkProgram _ sects ->
      vcat (fmap printWithLayout sects)

instance LayoutPrinter a => LayoutPrinter (TopDecl a) where
  printWithLayout = \case
    Declare   _ t -> printWithLayout t
    Decide    _ t -> printWithLayout t
    Assume    _ t -> printWithLayout t
    Directive _ t -> printWithLayout t

instance LayoutPrinter a => LayoutPrinter (Expr a) where
  printWithLayout :: LayoutPrinter a => Expr a -> Doc ann
  printWithLayout = \case
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
      <> line
      <> indent 2 (align (vcat (fmap printWithLayout namedExpr)))
    IfThenElse _ cond then' else' ->
      vcat
        [ "IF" <+> printWithLayout cond
        , "THEN" <+> printWithLayout then'
        , "ELSE" <+> printWithLayout else'
        ]
    Consider   _ expr branches ->
      vcat
        [ "CONSIDER" <+> printWithLayout expr
        , indent 2 (vsep $ punctuate comma (fmap printWithLayout branches))
        ]

    Lit        _ lit -> printWithLayout lit
    List       _ exprs ->
      "LIST" <+> hsep (punctuate comma (fmap parensIfNeeded exprs))
    Where      _ e1 decls ->
      vcat
        [ indent 2 (printWithLayout e1)
        , "WHERE"
        , indent 2 (vsep $ fmap printWithLayout decls)
        ]

instance LayoutPrinter a => LayoutPrinter (NamedExpr a) where
  printWithLayout = \case
    MkNamedExpr _ name e ->
      printWithLayout name <+> "IS" <+> printWithLayout e

instance LayoutPrinter a => LayoutPrinter (LocalDecl a) where
  printWithLayout = \case
    LocalDecide _ t -> printWithLayout t
    LocalAssume _ t -> printWithLayout t

instance LayoutPrinter Lit where
  printWithLayout = \case
    NumericLit _ t -> pretty t
    StringLit _ t -> surround (pretty $ escapeStringLiteral t) "\"" "\""

instance LayoutPrinter a => LayoutPrinter (Branch a) where
  printWithLayout = \case
    When _ pat e -> "WHEN" <+> printWithLayout pat <+> "THEN" <+> printWithLayout e
    Otherwise _ e -> "OTHERWISE" <+> printWithLayout e

instance LayoutPrinter a => LayoutPrinter (Pattern a) where
  printWithLayout = \case
    PatVar _ n -> printWithLayout n
    PatApp _ n pats -> printWithLayout n <> case pats of
      [] -> mempty
      pats'@(_:_) -> vsep (fmap printWithLayout pats')
    PatCons _ patHead patTail -> printWithLayout patHead <+> "FOLLOWED BY" <+> printWithLayout patTail

quoteIfNeeded :: Text.Text -> Text.Text
quoteIfNeeded n = case Text.uncons n of
  Nothing -> n
  Just (c, xs)
    | isAlpha c && Text.all isAlphaNum xs -> n
    | otherwise -> "`" <> n <> "`"

scanAnd :: Expr a -> [Expr a]
scanAnd (And _ e1 e2) =
  scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr a -> [Expr a]
scanOr (Or _ e1 e2) =
  scanOr e1 <> scanOr e2
scanOr e = [e]

prettyConj :: Text -> [Doc ann] -> Doc ann
prettyConj _ [] = mempty
prettyConj cnj (d:ds) =
  indent (Text.length cnj + 1) d <>
  case ds of
    [] -> mempty
    ds'@(_:_) -> go ds'
  where
    go [] = mempty
    go (x:xs) =
      line <> hang (Text.length cnj + 1) (pretty cnj <+> x) <> go xs

parensIfNeeded :: LayoutPrinter a => Expr a -> Doc ann
parensIfNeeded e = case e of
  Lit{} -> printWithLayout e
  App _ _ [] -> printWithLayout e
  Var{} -> printWithLayout e
  _ -> surround (printWithLayout e) "(" ")"

escapeStringLiteral :: Text -> Text
escapeStringLiteral = Text.concatMap (\case
  '\"' -> "\\\""
  '\\' -> "\\\\"
  c -> Text.singleton c
  )
