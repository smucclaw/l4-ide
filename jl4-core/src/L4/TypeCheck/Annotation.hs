module L4.TypeCheck.Annotation where

import Base
import L4.TypeCheck.Types
import L4.Annotation
import L4.Lexer
import L4.Syntax
import Optics

resolveNlgAnnotation ::
  (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension) =>
  a ->
  Check a
resolveNlgAnnotation a = do
  case a ^. annoOf % annNlg of
    Nothing -> pure a
    Just nlgM -> do
      resolvedNlg <- case nlgM of
        MkInvalidNlg{} -> pure nlgM
        MkResolvedNlg{} -> pure nlgM
        MkParsedNlg ann frags -> do
          resolvedFrags <- traverse resolveNlgFragment frags
          pure $ MkResolvedNlg ann resolvedFrags
      setAnnNlg resolvedNlg a

resolveNlgFragment :: NlgFragment Name -> Check (NlgFragment Resolved)
resolveNlgFragment = \ case
  MkNlgText ann t -> pure $ MkNlgText ann t
  MkNlgRef ann n ->
    MkNlgRef ann . fst <$> resolveTerm n

resolveNlgAnnotationInResolved :: Resolved -> Check Resolved
resolveNlgAnnotationInResolved = traverseResolved resolveNlgAnnotation

nlgDecide :: Decide Resolved -> Check (Decide Resolved)
nlgDecide (MkDecide ann tySig appForm body) =
  MkDecide ann
    <$> nlgTypeSig tySig
    <*> nlgAppForm appForm
    <*> nlgExpr body

nlgExpr :: Expr Resolved -> Check (Expr Resolved)
nlgExpr = \ case
    And ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ And ann e1' e2'
    Or ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Or ann e1' e2'
    RAnd ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ RAnd ann e1' e2'
    ROr ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ ROr ann e1' e2'
    Implies ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Implies ann e1' e2'
    Equals ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Equals ann e1' e2'
    Not ann e -> do
      e' <- nlgExpr e
      pure $ Not ann e'
    Plus ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Plus ann e1' e2'
    Minus ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Minus ann e1' e2'
    Times ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Times ann e1' e2'
    DividedBy ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ DividedBy ann e1' e2'
    Modulo ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Modulo ann e1' e2'
    Cons ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Cons ann e1' e2'
    Leq ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Leq ann e1' e2'
    Lt ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Lt ann e1' e2'
    Gt ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Gt ann e1' e2'
    Geq ann e1 e2 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Geq ann e1' e2'
    Proj ann e1 n -> do
      e1' <- nlgExpr e1
      n' <- resolveNlgAnnotationInResolved n
      pure $ Proj ann e1' n'
    Var ann v -> do
      v' <- resolveNlgAnnotationInResolved v
      pure $ Var ann v'
    Lam ann sig body -> do
      -- Since the parameters in the lambda bring new variables into
      -- scope, we have to resolve the annotations when checking the 'Lam'
      -- case. Thus, we don't need to traverse it here again.
      pure $ Lam ann sig body
    App ann n ns -> do
      n' <- resolveNlgAnnotationInResolved n
      ns' <- traverse nlgExpr ns
      pure $ App ann n' ns'
    AppNamed ann n ns order -> do
      n' <- resolveNlgAnnotationInResolved n
      ns' <- traverse nlgNamedExpr ns
      pure $ AppNamed ann n' ns' order
    IfThenElse ann b e1 e2 -> do
      b' <- nlgExpr b
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ IfThenElse ann b' e1' e2'
    MultiWayIf ann es e -> do
      es' <- for es \(MkGuardedExpr ann' c f) -> do
        c' <- nlgExpr c
        f' <- nlgExpr f
        pure $ MkGuardedExpr ann' c' f'
      e' <- nlgExpr e
      pure $ MultiWayIf ann es' e'
    Regulative ann (MkObligation ann'' party (MkAction ann' rule provided) deadline followup lest) -> do
      party' <- nlgExpr party
      rule' <- nlgPattern rule
      provided' <- traverse nlgExpr provided
      deadline' <- traverse nlgExpr deadline
      followup' <- traverse nlgExpr followup
      lest' <- traverse nlgExpr lest
      pure $ Regulative ann (MkObligation ann'' party' (MkAction ann' rule' provided') deadline' followup' lest')
    Consider ann e branches  -> do
      e' <- nlgExpr e
      -- Since the bindings in the branches bring new variables into
      -- scope, we have to resolve the annotations when checking the 'Consider'
      -- case. Thus, we don't need to traverse it here again.
      pure $ Consider ann e' branches
    expr@Lit{} -> do
      pure expr
    Percent ann expr -> do
      Percent ann <$> nlgExpr expr
    List ann es -> do
      es' <- traverse nlgExpr es
      pure $ List ann es'
    Where ann e lcl -> do
      -- Since the bindings in the 'LocalDecl' bring new variables into
      -- scope, we have to resolve the annotations when checking the 'Where'
      -- case. Thus, we don't need to traverse it here again.
      pure $ Where ann e lcl
    Event ann (MkEvent ann' e e1 e2 atFirst) -> do
      e' <- nlgExpr e
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      pure $ Event ann (MkEvent ann' e' e1' e2' atFirst)
    Fetch ann e ->
      Fetch ann <$> nlgExpr e
    Env ann e ->
      Env ann <$> nlgExpr e
    Post ann e1 e2 e3 -> do
      e1' <- nlgExpr e1
      e2' <- nlgExpr e2
      e3' <- nlgExpr e3
      pure $ Post ann e1' e2' e3'
    Concat ann es -> do
      es' <- traverse nlgExpr es
      pure $ Concat ann es'
    AsString ann e ->
      AsString ann <$> nlgExpr e

nlgPattern :: Pattern Resolved -> Check (Pattern Resolved)
nlgPattern = \ case
  PatVar ann n ->
    PatVar ann
      <$> resolveNlgAnnotationInResolved n
  PatApp ann n pats ->
    PatApp ann
      <$> resolveNlgAnnotationInResolved n
      <*> traverse nlgPattern pats
  PatCons ann pat pats ->
    PatCons ann
      <$> nlgPattern pat
      <*> nlgPattern pats
  PatExpr ann expr -> pure $ PatExpr ann expr
  PatLit ann lit -> pure $ PatLit ann lit
nlgLocalDecl :: LocalDecl Resolved -> Check (LocalDecl Resolved)
nlgLocalDecl = \ case
  LocalDecide ann decide ->
    LocalDecide ann
      <$> nlgDecide decide
  LocalAssume ann assume ->
    LocalAssume ann
      <$> nlgAssume assume

nlgAssume :: Assume Resolved -> Check (Assume Resolved)
nlgAssume = \ case
  MkAssume ann tySig appForm mTy ->
    MkAssume ann
      <$> nlgTypeSig tySig
      <*> nlgAppForm appForm
      <*> traverse nlgType mTy

nlgNamedExpr :: NamedExpr Resolved -> Check (NamedExpr Resolved)
nlgNamedExpr = \ case
  MkNamedExpr ann n expr ->
    MkNamedExpr ann
      <$> resolveNlgAnnotationInResolved n
      <*> nlgExpr expr

nlgAppForm :: AppForm Resolved -> Check (AppForm Resolved)
nlgAppForm (MkAppForm ann n ns maka) =
  MkAppForm ann
    <$> resolveNlgAnnotationInResolved n
    <*> traverse resolveNlgAnnotationInResolved ns
    <*> traverse resolveNlgAnnotation maka

nlgTypeSig :: TypeSig Resolved -> Check (TypeSig Resolved)
nlgTypeSig (MkTypeSig ann givenSig mGivethSig) =
  MkTypeSig ann
    <$> nlgGivenSig givenSig
    <*> traverse nlgGivethSig mGivethSig

nlgGivethSig :: GivethSig Resolved -> Check (GivethSig Resolved)
nlgGivethSig (MkGivethSig ann ty) =
  MkGivethSig ann
    <$> nlgType ty

nlgType :: Type' Resolved -> Check (Type' Resolved)
nlgType = \ case
  Type   ann ->
    pure $ Type ann
  TyApp  ann n tys ->
    TyApp ann
      <$> resolveNlgAnnotationInResolved n
      <*> traverse nlgType tys
  Fun    ann optNamedTypes ty ->
    Fun ann
      <$> traverse nlgOptionallyNamedType optNamedTypes
      <*> nlgType ty
  Forall ann ns ty ->
    Forall ann <$> traverse resolveNlgAnnotationInResolved ns <*> nlgType ty
  InfVar ann name uniq ->
    pure $ InfVar ann name uniq

nlgOptionallyNamedType :: OptionallyNamedType Resolved -> Check (OptionallyNamedType Resolved)
nlgOptionallyNamedType (MkOptionallyNamedType ann mName ty) =
  MkOptionallyNamedType ann
    <$> traverse resolveNlgAnnotationInResolved mName
    <*> nlgType ty

nlgGivenSig :: GivenSig Resolved -> Check (GivenSig Resolved)
nlgGivenSig (MkGivenSig ann ns) =
  MkGivenSig ann
    <$> traverse nlgOptionallyTypedName ns

nlgOptionallyTypedName :: OptionallyTypedName Resolved -> Check (OptionallyTypedName Resolved)
nlgOptionallyTypedName (MkOptionallyTypedName ann n mty) =
  MkOptionallyTypedName ann
    <$> resolveNlgAnnotationInResolved n
    <*> traverse nlgType mty

nlgDeclare :: Declare Resolved -> Check (Declare Resolved)
nlgDeclare (MkDeclare ann tysig appForm tydecl) =
  MkDeclare ann
    <$> nlgTypeSig tysig
    <*> nlgAppForm appForm
    <*> nlgTypeDecl tydecl

nlgTypeDecl :: TypeDecl Resolved -> Check (TypeDecl Resolved)
nlgTypeDecl = \ case
  RecordDecl ann mName typedNames ->
    RecordDecl ann
      <$> traverse resolveNlgAnnotationInResolved mName
      <*> traverse nlgTypedName typedNames
  EnumDecl ann condecls->
    EnumDecl ann
      <$> traverse nlgConDecl condecls
  SynonymDecl ann ty->
    SynonymDecl ann
      <$> nlgType ty

nlgConDecl :: ConDecl Resolved -> Check (ConDecl Resolved)
nlgConDecl (MkConDecl ann n typedName) =
  MkConDecl ann
    <$> resolveNlgAnnotationInResolved n
    <*> traverse nlgTypedName typedName

nlgTypedName :: TypedName Resolved -> Check (TypedName Resolved)
nlgTypedName (MkTypedName ann n ty) =
  MkTypedName ann
    <$> resolveNlgAnnotationInResolved n
    <*> nlgType ty
