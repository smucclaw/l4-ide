{-# LANGUAGE ApplicativeDo #-}

module L4.Parser.ResolveAnnotation (
  -- * main function
  HasNlg(..),
  addNlgCommentsToAst,
  -- * Annotate Syntax Nodes with definite 'SrcSpan's.
  WithSpan (..),
  NlgWithSpan,
  -- * Warnings and state
  NlgS(..),
  Warning (..),
  -- * NlgA / NlgM monad
  NlgA(..),
  NlgM(..),
  liftNlgA,
  extendNlgA,
  registerSrcSpanNlgA,
  hoistNlgA,
  registerNlgA,
  -- * Internals, helpful for testing
  LocRange(..),
  prettyLocRange,
  UpperBound(..),
  locRangeTo,
  LowerBound(..),
  locRangeFrom,
)
where

import Base

import qualified Generics.SOP as SOP
import L4.Annotation
import L4.Syntax
import L4.Parser.SrcSpan

-- | Warnings for attaching Nlg comments to the ast.
data Warning
  = NotAttached NlgWithSpan
  | UnknownLocation Nlg
  | Ambiguous Name [NlgWithSpan] -- Must be at least two
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

type NlgWithSpan = WithSpan Nlg

-- | Attach any payload with a 'SrcSpan'.
data WithSpan a = WithSpan
  { range :: SrcSpan
  , payload :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)

-- | Add the given 'Nlg' comments to the 'Program Name' based on
-- the 'SrcRange'. Modifies the 'Program Name'.
--
-- Note, the 'Program Name's exactprint annotations are not modified,
-- we merely add structured data to the ast node's respective 'Anno'.
addNlgCommentsToAst :: HasNlg a => [Nlg] -> a -> (a, NlgS)
addNlgCommentsToAst nlgs p = do
  let
    (nlgWithSpan, unfindable) = preprocessNlgs nlgs

    initialNlgS = NlgS
      { nlgs = nlgWithSpan
      , warnings = fmap UnknownLocation unfindable
      }

  runNlg initialNlgS $ do
    a <- addNlg p
    leftoversToWarnings
    pure a
 where
  locRange = MkLocRange StartOfFile EndOfFile

  runNlg initState act =
    runState
      (((.computation.runNlgM) act) locRange)
      initState

leftoversToWarnings :: NlgA ()
leftoversToWarnings = do
  liftNlgA $ do
    ns <- use #nlgs
    traverse_ addWarning $ fmap NotAttached ns

addWarning :: Warning -> NlgM ()
addWarning warn = do
  modifying' #warnings (warn:)

-- | Attach for each 'Nlg' its 'SrcSpan' for convenient access.
-- If a 'Nlg' doesn't have a 'SrcRange', then that's a bug.
-- We need to report such 'Nlg's as 'Warning's.
preprocessNlgs :: [Nlg] -> ([NlgWithSpan], [Nlg])
preprocessNlgs nlgs =
  -- TODO: this is a known space leak, a lazy accumulator with `foldl'`.
  foldl' go ([], []) nlgs
 where
  go (nlg, unprocessable) n = case rangeOf n of
    Nothing -> (nlg, n : unprocessable)
    Just r -> (WithSpan (fromSrcRange r) n : nlg, unprocessable)

-- ----------------------------------------------------------------------------
-- HasNlg Class and Instances
-- ----------------------------------------------------------------------------

-- | Add 'Nlg' annotations that are in "scope" for the abstract syntax node 'a'.
-- Any type that implements this type class declares that it can either
-- have 'Nlg' annotations, or that one of its children can be annotated with
-- an 'Nlg' comment.
class HasNlg a where
  -- | Add 'Nlg' annotations that are applicable to the current AST node 'a'
  -- based on the 'SrcSpan' of 'a' and its neighbours.
  addNlg :: a -> NlgA a

instance (HasSrcRange n, HasNlg n) => HasNlg (Module n) where
  addNlg a = extendNlgA a $ case a of
    MkModule uri ann sects -> do
      sects' <- traverse addNlg sects
      pure (MkModule uri ann sects')

instance (HasSrcRange n, HasNlg n) => HasNlg (Section n) where
  addNlg a = extendNlgA a $ case a of
    MkSection ann lvl lbl maka topDecls -> do
      lbl' <- traverse addNlg lbl
      maka' <- traverse addNlg maka
      topDecls' <- traverse addNlg topDecls
      pure (MkSection ann lvl lbl' maka' topDecls')

instance (HasSrcRange n, HasNlg n) => HasNlg (TopDecl n) where
  addNlg a = extendNlgA a $ case a of
    Declare ann declare -> do
      declare' <- addNlg declare
      pure $ Declare ann declare'
    Decide ann decide -> do
      decide' <- addNlg decide
      pure $ Decide ann decide'
    Assume ann assume -> do
      assume' <- addNlg assume
      pure $ Assume ann assume'
    Directive ann directive -> do
      directive' <- addNlg directive
      pure $ Directive ann directive'
    Import ann import_ -> do
      import_' <- addNlg import_
      pure $ Import ann import_'

instance (HasSrcRange n, HasNlg n) => HasNlg (Declare n) where
  addNlg a = extendNlgA a $ case a of
    MkDeclare ann tySig appFormAka tyDecl -> do
      tySig' <- addNlg tySig
      appFormAka' <- addNlg appFormAka
      tyDecl' <- addNlg tyDecl
      pure $ MkDeclare ann tySig' appFormAka' tyDecl'

instance (HasSrcRange n, HasNlg n) => HasNlg (Decide n) where
  addNlg a = extendNlgA a $ case a of
    MkDecide ann tySig appFormAka expr -> do
      tySig' <- addNlg tySig
      appFormAka' <- addNlg appFormAka
      expr' <- addNlg expr
      pure $ MkDecide ann tySig' appFormAka' expr'

instance (HasSrcRange n, HasNlg n) => HasNlg (Assume n) where
  addNlg a = extendNlgA a $ case a of
    MkAssume ann tySig appFormAka order -> do
      tySig' <- addNlg tySig
      appFormAka' <- addNlg appFormAka
      pure $ MkAssume ann tySig' appFormAka' order

instance (HasSrcRange n, HasNlg n) => HasNlg (Directive n) where
  addNlg a = extendNlgA a $ case a of
    StrictEval ann tr e -> do
      e' <- addNlg e
      pure $ StrictEval ann tr e'
    LazyEval ann tr e -> do
      e' <- addNlg e
      pure $ LazyEval ann tr e'
    Check ann e -> do
      e' <- addNlg e
      pure $ Check ann e'

instance (HasSrcRange n, HasNlg n) => HasNlg (Import n) where
  addNlg a = extendNlgA a $ case a of
    MkImport ann n -> do
      n' <- addNlg n
      pure $ MkImport ann n'

instance (HasSrcRange n, HasNlg n) => HasNlg (TypeDecl n) where
  addNlg a = extendNlgA a $ case a of
    RecordDecl ann mcon typedNames -> do
      typedNames' <- traverse addNlg typedNames
      pure $ RecordDecl ann mcon typedNames'
    EnumDecl ann conDecls -> do
      conDecls' <- traverse addNlg conDecls
      pure $ EnumDecl ann conDecls'
    SynonymDecl ann ty -> do
      ty' <- addNlg ty
      pure $ SynonymDecl ann ty'

instance (HasSrcRange n, HasNlg n) => HasNlg (TypedName n) where
  addNlg a = extendNlgA a $ case a of
    MkTypedName ann n ty -> do
      n' <- addNlg n
      ty' <- addNlg ty
      pure $ MkTypedName ann n' ty'

instance (HasSrcRange n, HasNlg n) => HasNlg (ConDecl n) where
  addNlg a = extendNlgA a $ case a of
    MkConDecl ann n typedNames -> do
      n' <- addNlg n
      typedNames' <- traverse addNlg typedNames
      pure $ MkConDecl ann n' typedNames'

instance (HasSrcRange n, HasNlg n) => HasNlg (TypeSig n) where
  addNlg a = extendNlgA a $ case a of
    MkTypeSig ann givenSig givethSig -> do
      givenSig' <- addNlg givenSig
      givethSig' <- traverse addNlg givethSig
      pure $ MkTypeSig ann givenSig' givethSig'

instance (HasSrcRange n, HasNlg n) => HasNlg (GivenSig n) where
  addNlg a = extendNlgA a $ case a of
    MkGivenSig ann tys -> do
      tys' <- traverse addNlg tys
      pure $ MkGivenSig ann tys'

instance (HasSrcRange n, HasNlg n) => HasNlg (OptionallyTypedName n) where
  addNlg a = extendNlgA a $ case a of
    MkOptionallyTypedName ann n mty -> do
      n' <- addNlg n
      tys' <- traverse addNlg mty
      pure $ MkOptionallyTypedName ann n' tys'

instance (HasSrcRange n, HasNlg n) => HasNlg (GivethSig n) where
  addNlg a = extendNlgA a $ case a of
    MkGivethSig ann mty -> do
      mty' <- addNlg mty
      pure $ MkGivethSig ann mty'

instance (HasSrcRange n, HasNlg n) => HasNlg (Type' n) where
  addNlg a = extendNlgA a $ case a of
    Type   ann -> do
      pure $ Type ann
    TyApp  ann n tys -> do
      n' <- addNlg n
      tys' <- traverse addNlg tys
      pure $ TyApp ann n' tys'
    Fun    ann names ty -> do
      names' <- traverse addNlg names
      ty' <- addNlg ty
      pure $ Fun ann names' ty'
    Forall ann ns ty -> do
      ns' <- traverse addNlg ns
      ty' <- addNlg ty
      pure $ Forall ann ns' ty'
    InfVar ann raw i -> do
      pure $ InfVar ann raw i

instance (HasSrcRange n, HasNlg n) => HasNlg (OptionallyNamedType n) where
  addNlg a = extendNlgA a $ case a of
    MkOptionallyNamedType ann mName ty -> do
      mName' <- traverse addNlg mName
      ty' <- addNlg ty
      pure $ MkOptionallyNamedType ann mName' ty'

instance (HasSrcRange n, HasNlg n) => HasNlg (AppForm n) where
  addNlg a = extendNlgA a $ case a of
    MkAppForm ann n ns maka -> do
      n' <- addNlg n
      ns' <- traverse addNlg ns
      maka' <- traverse addNlg maka
      pure $ MkAppForm ann n' ns' maka'

instance (HasSrcRange n, HasNlg n) => HasNlg (Aka n) where
  addNlg a = extendNlgA a $ case a of
    MkAka ann ns -> do
      ns' <- traverse addNlg ns
      pure $ MkAka ann ns'

instance HasNlg Name where
  addNlg a = extendNlgA a $ case a of
    MkName ann raw -> do
      ann' <- liftNlgA $ do
        nlgs <- takeNlgComments
        case nlgs of
          [nlg] -> do
            pure $ setNlg nlg.payload ann
          [] ->
            pure ann
          ns -> do
            addWarning $ Ambiguous a ns
            pure ann

      pure $ MkName ann' raw

instance (HasSrcRange n, HasNlg n) => HasNlg (Expr n) where
  addNlg expr = extendNlgA expr $ case expr of
    And ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ And ann e1' e2'
    Or ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Or ann e1' e2'
    Implies ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Implies ann e1' e2'
    Equals ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Equals ann e1' e2'
    Not ann e -> do
      e' <- addNlg e
      pure $ Not ann e'
    Plus ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Plus ann e1' e2'
    Minus ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Minus ann e1' e2'
    Times ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Times ann e1' e2'
    DividedBy ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ DividedBy ann e1' e2'
    Modulo ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Modulo ann e1' e2'
    Cons ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Cons ann e1' e2'
    Leq ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Leq ann e1' e2'
    Lt ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Lt ann e1' e2'
    Gt ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Gt ann e1' e2'
    Geq ann e1 e2 -> do
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ Geq ann e1' e2'
    Proj ann e1 n -> do
      e1' <- addNlg e1
      n' <- addNlg n
      pure $ Proj ann e1' n'
    Var ann v -> do
      v' <- addNlg v
      pure $ Var ann v'
    Lam ann sig body -> do
      sig' <- addNlg sig
      body' <- addNlg body
      pure $ Lam ann sig' body'
    App ann n ns -> do
      n' <- addNlg n
      ns' <- traverse addNlg ns
      pure $ App ann n' ns'
    AppNamed ann n ns order -> do
      n' <- addNlg n
      ns' <- traverse addNlg ns
      pure $ AppNamed ann n' ns' order
    IfThenElse ann b e1 e2 -> do
      b' <- addNlg b
      e1' <- addNlg e1
      e2' <- addNlg e2
      pure $ IfThenElse ann b' e1' e2'
    Consider ann e branches  -> do
      e' <- addNlg e
      branches' <- traverse addNlg branches
      pure $ Consider ann e' branches'
    Lit{} -> do
      pure expr
    List ann es -> do
      es' <- traverse addNlg es
      pure $ List ann es'
    Where ann e lcl -> do
      e' <- addNlg e
      lcl' <- traverse addNlg lcl
      pure $ Where ann e' lcl'

instance (HasSrcRange n, HasNlg n) => HasNlg (Branch n) where
  addNlg a = extendNlgA a $ case a of
    When ann pat e -> do
      pat' <- addNlg pat
      e' <- addNlg e
      pure $ When ann pat' e'
    Otherwise ann e -> do
      e' <- addNlg e
      pure $ Otherwise ann e'

instance (HasSrcRange n, HasNlg n) => HasNlg (Pattern n) where
  addNlg a = extendNlgA a $ case a of
    PatVar ann n -> do
      n' <- addNlg n
      pure $ PatVar ann n'
    PatApp ann n pats -> do
      n' <- addNlg n
      pats' <- traverse addNlg pats
      pure $ PatApp ann n' pats'
    PatCons ann patHead patTail -> do
      patHead' <- addNlg patHead
      patTail' <-addNlg patTail
      pure $ PatCons ann patHead' patTail'

instance (HasSrcRange n, HasNlg n) => HasNlg (NamedExpr n) where
  addNlg a = extendNlgA a $ case a of
    MkNamedExpr ann n e -> do
      n' <- addNlg n
      e' <- addNlg e
      pure $ MkNamedExpr ann n' e'

instance (HasSrcRange n, HasNlg n) => HasNlg (LocalDecl n) where
  addNlg a = extendNlgA a $ case a of
    LocalDecide ann decide -> do
      decide' <- addNlg decide
      pure $ LocalDecide ann decide'
    LocalAssume ann assume -> do
      assume' <- addNlg assume
      pure $ LocalAssume ann assume'

-- ----------------------------------------------------------------------------
-- NlgA Definition
-- ----------------------------------------------------------------------------

-- | 'NlgA' provides a way to propagate location information from surrounding
-- computations:
--
-- @
--   left_neighbour <*> NlgA inner_span inner_m <*> right_neighbour
-- @
--
-- Here, the following holds:
--
-- * the 'left_neighbour' will only see Nlg comments until 'bufSpanStart' of 'inner_span'
-- * the 'right_neighbour' will only see Nlg comments after 'bufSpanEnd' of 'inner_span'
-- * the 'inner_m' will only see Nlg comments between its 'left_neighbour' and its 'right_neighbour'
--
-- In other words, every computation:
--
--  * delimits the surrounding computations
--  * is delimited by the surrounding computations
--
-- Therefore, a 'NlgA' computation must be always considered in the context in
-- which it is used.
--
-- This implementation is taken from GHC, as Haddock comments have similar semantics to
-- our 'Nlg' comments.
-- Thus, we include the GHC note explaining the implementation in more detail.
-- See Note [Adding Haddock comments to the syntax tree].
-- However, our implementation is currently much simpler.
data NlgA a = MkNlgA
  { range :: !(Maybe SrcSpan)
  -- ^ 'SrcSpan' of the processed AST element.
  --
  -- @
  -- Just b  <=> BufSpan occupied by the processed AST element.
  --             The surrounding computations will not look inside.
  -- @
  --
  -- @
  -- Nothing <=> No BufSpan (e.g. when the HdkA is constructed by 'pure' or 'liftHdkA').
  --             The surrounding computations are not delimited.
  -- @
  , computation :: !(NlgM a)
  -- ^ The stateful computation that looks up 'Nlg' comments and
  -- adds them to the resulting AST node.
  }
  deriving (Functor, Generic)
  deriving anyclass (SOP.Generic)

instance Applicative NlgA where
  MkNlgA l1 m1 <*> MkNlgA l2 m2 =
    MkNlgA
      (l1 <> l2)
      (delim1 m1 <*> delim2 m2)
   where
    -- Delimit the LHS by the location information from the RHS
    delim1 = inLocRange (locRangeTo (fmap (.start) l2))
    -- Delimit the RHS by the location information from the LHS
    delim2 = inLocRange (locRangeFrom (fmap (.end) l1))

  pure a =
    liftNlgA (pure a)

liftNlgA :: NlgM a -> NlgA a
liftNlgA = MkNlgA mempty

extendNlgA :: (HasSrcRange e) => e -> NlgA a -> NlgA a
extendNlgA l' (MkNlgA l m) = MkNlgA (l2 <> l) m
 where
  l2 = fromSrcRange <$> rangeOf l'

registerSrcSpanNlgA :: Maybe SrcSpan -> NlgA ()
registerSrcSpanNlgA l = MkNlgA l (pure ())

-- | Modify the action of a NlgA computation.
hoistNlgA :: (NlgM a -> NlgM b) -> NlgA a -> NlgA b
hoistNlgA f (MkNlgA l m) = MkNlgA l (f m)

registerNlgA :: (HasSrcRange a) => a -> NlgA ()
registerNlgA a = registerSrcSpanNlgA (fromSrcRange <$> rangeOf a)

-- ----------------------------------------------------------------------------
-- NlgM Definition
-- ----------------------------------------------------------------------------

-- | The state of 'NlgM' contains a list of pending 'Nlg' comments. We go
-- over the AST, looking up these comments using 'takeNlgComments' and removing
-- them from the state. Also, using a state means we never use the same
-- 'Nlg' twice.
--
-- See Note [Adding Haddock comments to the syntax tree].
newtype NlgM a = MkNlgM {runNlgM :: LocRange -> (State NlgS) a}
  deriving (Functor, Applicative, Monad, MonadState NlgS, MonadReader LocRange) via (ReaderT LocRange (State NlgS))

data NlgS = NlgS
  { nlgs :: ![NlgWithSpan]
    -- ^ Nlg annotations that haven't been assigned to a specific 'Name' or
    -- other abstract syntax node.
  , warnings :: [Warning]
    -- ^ Warnings uncovered while trying to attach 'Nlg' annotations
    -- to the ast.
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

-- | Represents a predicate on SrcPos:
--
-- @
--   UpperLocBound |   SrcPos -> Bool
--   --------------+-----------------
--   EndOfFile     |   const True
--   EndPos p      |   (<= p)
-- @
--
--  The semigroup instance corresponds to (&&).
--
--  We don't use the  SrcPos -> Bool  representation
--  as it would lead to redundant checks.
--
--  That is, instead of
--
-- @
--    (pos <= 40) && (pos <= 30) && (pos <= 20)
-- @
--
--  We'd rather only do the (<=20) check. So we reify the predicate to make
--  sure we only check for the most restrictive bound.
data UpperBound = EndOfFile | EndPos !SrcPos
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

data LowerBound = StartOfFile | StartPos !SrcPos
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

instance Semigroup UpperBound where
  EndOfFile <> l = l
  l <> EndOfFile = l
  EndPos l <> EndPos r = EndPos (min l r) -- See the docs for 'UpperBound'

instance Semigroup LowerBound where
  StartOfFile <> l = l
  l <> StartOfFile = l
  StartPos l <> StartPos r = StartPos (max l r) -- See the docs for 'UpperBound'.

upperBoundToSrcSpan :: UpperBound -> SrcPos
upperBoundToSrcSpan = \case
  EndOfFile -> MkSrcPos maxBound maxBound
  EndPos p -> p

lowerBoundToSrcSpan :: LowerBound -> SrcPos
lowerBoundToSrcSpan = \case
  -- No position is lower than 1.
  -- Don't use 'minBound' because it is ugly during debugging.
  StartOfFile -> MkSrcPos 1 1
  StartPos p -> p

instance Monoid LowerBound where
  mempty = StartOfFile

data LocRange = MkLocRange
  { rangeFrom :: LowerBound
  , rangeTo :: UpperBound
  -- , column :: !Int
  -- Required indentation. Unused right now.
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

-- | The location range from the specified position to the end of the file.
locRangeFrom :: Maybe SrcPos -> LocRange
locRangeFrom (Just l) = mempty{rangeFrom = StartPos l}
locRangeFrom Nothing = mempty

-- | The location range from the start of the file to the specified position.
locRangeTo :: Maybe SrcPos -> LocRange
locRangeTo (Just l) = mempty{rangeTo = EndPos l}
locRangeTo Nothing = mempty

instance Semigroup LocRange where
  MkLocRange f1 t1 <> MkLocRange f2 t2 =
    MkLocRange
      (f1 <> f2)
      (t1 <> t2)

instance Monoid LocRange where
  mempty = MkLocRange StartOfFile EndOfFile

prettyLocRange :: LocRange -> Text
prettyLocRange locRange =
  prettySrcPos start <> "-" <> prettySrcPos end
 where
  end = case locRange.rangeTo of
    EndOfFile -> MkSrcPos maxBound maxBound
    EndPos p -> p
  start = case locRange.rangeFrom of
    -- No position is lower than 1, don't use 'minBound'
    StartOfFile -> MkSrcPos 1 1
    StartPos p -> p

-- ----------------------------------------------------------------------------
-- Workers
-- ----------------------------------------------------------------------------

-- | Restrict the range in which a NlgM computation will look up comments:
--
--   inLocRange r1 $
--   inLocRange r2 $
--     takeNlgComments ...  -- Only takes comments in the (r1 <> r2) location range.
--
-- Note that it does not blindly override the range but tightens it using (<>).
-- At many use sites, you will see something along the lines of:
--
--   inLocRange (locRangeTo end_pos) $ ...
--
-- And 'locRangeTo' defines a location range from the start of the file to
-- 'end_pos'. This does not mean that we now search for every comment from the
-- start of the file, as this restriction will be combined with other
-- restrictions. Somewhere up the callstack we might have:
--
--   inLocRange (locRangeFrom start_pos) $ ...
--
-- The net result is that the location range is delimited by 'start_pos' on
-- one side and by 'end_pos' on the other side.
--
-- In 'NlgA', every (<*>) may restrict the location range of its
-- subcomputations.
inLocRange :: LocRange -> NlgM a -> NlgM a
inLocRange r (MkNlgM m) = MkNlgM $ \r' -> m (r <> r')

-- | Get all the 'Nlg' comments that are within
-- the range of 'LocRange'.
--
-- @
--   'takeNlgsInRange' locRange nlgs
-- @
--
-- The first component of the result are the 'nlgs' within the 'LocRange',
-- and the second component are the nlgs that aren't within the 'LocRange'.
takeNlgsInRange :: LocRange -> [NlgWithSpan] -> ([NlgWithSpan], [NlgWithSpan])
takeNlgsInRange locRange nlgs =
  partition
    (\nlg -> nlg.range `subRangeOf` desiredRange)
    nlgs
 where
  desiredRange = MkSrcSpan start end
  end = upperBoundToSrcSpan locRange.rangeTo
  start = lowerBoundToSrcSpan locRange.rangeFrom

-- | Monadic version of 'takeNlgsInRange'.
-- Takes the 'Nlg's that are currently in scope and removes
-- them from the internal state.
--
takeNlgComments :: NlgM [NlgWithSpan]
takeNlgComments = do
  s <- get
  locRange <- ask
  let
    (taken, rest) = takeNlgsInRange locRange s.nlgs
  put (s{nlgs = rest})
  pure taken

{- Note [Adding Haddock comments to the syntax tree]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'addHaddock' traverses the AST in concrete syntax order, building a computation
(represented by HdkA) that reconstructs the AST but with Haddock comments
inserted in appropriate positions:

  addHaddock :: HasHaddock a => a -> HdkA a

Consider this code example:

  f :: Int  -- ^ comment on argument
    -> Bool -- ^ comment on result

In the AST, the "Int" part of this snippet is represented like this
(pseudo-code):

  L (BufSpan 6 8) (HsTyVar "Int") :: LHsType GhcPs

And the comments are represented like this (pseudo-code):

  L (BufSpan 11 35) (HdkCommentPrev "comment on argument")
  L (BufSpan 46 69) (HdkCommentPrev "comment on result")

So when we are traversing the AST and 'addHaddock' is applied to HsTyVar "Int",
how does it know to associate it with "comment on argument" but not with
"comment on result"?

The trick is to look in the space between syntactic elements. In the example above,
the location range in which we search for HdkCommentPrev is as follows:

  f :: Int████████████████████████
   ████Bool -- ^ comment on result

We search for comments after  HsTyVar "Int"  and until the next syntactic
element, in this case  HsTyVar "Bool".

Ignoring the "->" allows us to accommodate alternative coding styles:

  f :: Int ->   -- ^ comment on argument
       Bool     -- ^ comment on result

Sometimes we also need to take indentation information into account.
Compare the following examples:

    class C a where
      f :: a -> Int
      -- ^ comment on f

    class C a where
      f :: a -> Int
    -- ^ comment on C

Notice how "comment on f" and "comment on C" differ only by indentation level.

Therefore, in order to know the location range in which the comments are applicable
to a syntactic elements, we need three nuggets of information:
  1. lower bound on the BufPos of a comment
  2. upper bound on the BufPos of a comment
  3. minimum indentation level of a comment

This information is represented by the 'LocRange' type.

In order to propagate this information, we have the 'HdkA' applicative.
'HdkA' is defined as follows:

  data HdkA a = HdkA (Maybe BufSpan) (HdkM a)

The first field contains a 'BufSpan', which represents the location
span taken by a syntactic element:

  addHaddock (L bufSpan ...) = HdkA (Just bufSpan) ...

The second field, 'HdkM', is a stateful computation that looks up Haddock
comments in the specified location range:

  HdkM a ≈
       LocRange                  -- The allowed location range
    -> [PsLocated HdkComment]    -- Unallocated comments
    -> (a,                       -- AST with comments inserted into it
        [PsLocated HdkComment])  -- Leftover comments

The 'Applicative' instance for 'HdkA' is defined in such a way that the
location range of every computation is defined by its neighbours:

  addHaddock aaa <*> addHaddock bbb <*> addHaddock ccc

Here, the 'LocRange' passed to the 'HdkM' computation of  addHaddock bbb
is determined by the BufSpan recorded in  addHaddock aaa  and  addHaddock ccc.

This is why it's important to traverse the AST in the order of the concrete
syntax. In the example above we assume that  aaa, bbb, ccc  are ordered by location:

  * getBufSpan (getLoc aaa) < getBufSpan (getLoc bbb)
  * getBufSpan (getLoc bbb) < getBufSpan (getLoc ccc)

Violation of this assumption would lead to bugs, and care must be taken to
traverse the AST correctly. For example, when dealing with class declarations,
we have to use 'flattenBindsAndSigs' to traverse it in the correct order.
-}
