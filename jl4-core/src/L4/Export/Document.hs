-- | Deterministic L4 -> document IR for the "Render" tab.
--
-- Walks a typechecked 'Module' (plus its resolved import dependencies) into a
-- 'Document': a hierarchical, presentation-neutral IR that the TypeScript layer
-- (and the @l4 render@ CLI) renders to HTML, Akoma Ntoso XML and PDF.
--
-- The body of each rule is a recursive 'Clause' tree, so nested
-- @IF/THEN/ELSE@, @CONSIDER@ case-splits, @AND@/@OR@ chains and deontic
-- @PARTY … MUST …@ rules render as an indented outline rather than one
-- run-on sentence.
--
-- Other behaviours: directives are never rendered; unreachable imported
-- material and directive-only test fixtures are dropped (unless
-- 'dropUnused' is off); L4 @SECTION@ nesting is preserved; the document
-- title is taken from the leading section.
module L4.Export.Document
  ( -- * Public IR
    Document (..)
  , DocSection (..)
  , DocGroup (..)
  , Block (..)
  , Clause (..)
  , UnitKind (..)
  , RenderedAs (..)
    -- * Plan
  , ExportPlan (..)
  , PlanModule (..)
  , PlanUnit (..)
    -- * Configuration
  , ExportConfig (..)
  , Disposition (..)
  , defaultExportConfig
    -- * Entry points
  , buildDocument
  , buildPlan
  ) where

import Base

import Control.Applicative ((<|>))
import qualified Base.Text as Text
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Char (isLower, isUpper, toUpper)
import qualified Data.List as List
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import qualified Data.Set as Set
import Optics (gplate, (%), (^.))

import L4.Desugar (carameliseNode)
import L4.Export (isExportedDecide)
import L4.Nlg (simpleLinearizer)
import L4.Syntax
import L4.Utils.Ratio (prettyRatio)

-- ----------------------------------------------------------------------------
-- Configuration
-- ----------------------------------------------------------------------------

data Disposition = Inline | Reference | Exclude
  deriving stock (Eq, Show, Generic)

data ExportConfig = MkExportConfig
  { dropUnused         :: !Bool
  , moduleDispositions :: !(Map.Map NormalizedUri Disposition)
  , unitDispositions   :: !(Map.Map Unique Disposition)
  }
  deriving stock (Show, Generic)

defaultExportConfig :: ExportConfig
defaultExportConfig = MkExportConfig
  { dropUnused         = True
  , moduleDispositions = Map.empty
  , unitDispositions   = Map.empty
  }

-- ----------------------------------------------------------------------------
-- Public IR
-- ----------------------------------------------------------------------------

data UnitKind = RuleUnit | TypeUnit | AssumptionUnit
  deriving stock (Eq, Show, Generic)

data RenderedAs
  = RenderedInline
  | RenderedReference { forced :: !Bool }
  deriving stock (Eq, Show, Generic)

-- | The recursive body of a rule.
data Clause
  = CLeaf Text                                 -- ^ a terminal sentence (prose / formula)
  | CFields [(Text, Text)]                     -- ^ label/value rows (record decl or literal)
  | CAll [Clause]                              -- ^ all of the following hold (AND)
  | CAny [Clause]                              -- ^ any of the following holds (OR)
  | CIf [(Text, Clause)] (Maybe Clause)        -- ^ if c1 then …; else if c2 …; else …
  | CCases Text [(Text, Clause)]               -- ^ consider <subject>: when <pattern> -> …
  | CDeontic Text Text Text                    -- ^ party, modal, action
             (Maybe Text)                      -- ^ deadline
             (Maybe Text)                      -- ^ provided-that
             (Maybe Clause)                    -- ^ hence (on compliance)
             (Maybe Clause)                    -- ^ lest (on breach)
  | CChain Text [Text]                         -- ^ a long arithmetic chain
                                               --   (sum/product/…): a lead-in
                                               --   ("the sum of the following")
                                               --   and one pre-rendered line per
                                               --   operand
  | CTable [Text] [[Clause]]                   -- ^ a list of same-typed records:
                                               --   column headings + rows of cells
  | CWhere Clause [(Text, Text, Clause)]       -- ^ a rule body, plus its local
                                               --   WHERE / LET definitions, each a
                                               --   (heading, connector, body) triple
  deriving stock (Show, Generic)

data Block = MkBlock
  { blockId           :: !Text
  , blockKind         :: !UnitKind
  , blockHeading      :: !Text
  , blockConnector    :: !Text        -- ^ "means", "holds if", "provides that", "equals"
  , blockBody         :: !Clause
  , blockRenderedAs   :: !RenderedAs
  , blockCitation     :: !(Maybe Text)
  , blockSourceModule :: !Text
  , blockImported     :: !Bool
  }
  deriving stock (Show, Generic)

data DocGroup = MkDocGroup
  { groupLabel  :: !(Maybe Text)
  , groupBlocks :: ![Block]
  }
  deriving stock (Show, Generic)

data DocSection = MkDocSection
  { sectionNumber  :: !Text
  , sectionHeading :: !(Maybe Text)
  , sectionGroups  :: ![DocGroup]
  , sectionSubs    :: ![DocSection]
  }
  deriving stock (Show, Generic)

data Document = MkDocument
  { docTitle    :: !Text
  , docSections :: ![DocSection]
  }
  deriving stock (Show, Generic)

-- ----------------------------------------------------------------------------
-- Plan IR
-- ----------------------------------------------------------------------------

data PlanUnit = MkPlanUnit
  { planId        :: !Text
  , planKind      :: !UnitKind
  , planHeading   :: !Text
  , planReachable :: !Bool
  , planDefault   :: !Disposition
  }
  deriving stock (Show, Generic)

data PlanModule = MkPlanModule
  { planModuleUri    :: !Text
  , planModuleLabel  :: !Text
  , planModuleIsMain :: !Bool
  , planUnits        :: ![PlanUnit]
  }
  deriving stock (Show, Generic)

data ExportPlan = MkExportPlan
  { planMainModule :: !Text
  , planModules    :: ![PlanModule]
  }
  deriving stock (Show, Generic)

-- ----------------------------------------------------------------------------
-- Internal: renderable units
-- ----------------------------------------------------------------------------

data UnitDecl
  = UDecide  (Decide Resolved)
  | UDeclare (Declare Resolved)
  | UAssume  (Assume Resolved)

data Unit = MkUnit
  { uPrimary :: !Unique
  , uModule  :: !NormalizedUri
  , uKind    :: !UnitKind
  , uHeading :: !Text
  , uDefs    :: !(Set.Set Unique)
  , uRefs    :: !(Set.Set Unique)
  , uDecl    :: !UnitDecl
  }

extractUnits :: Module Resolved -> [Unit]
extractUnits (MkModule _ _ section) = goSection section
 where
  goSection (MkSection _ _ _ decls) = concatMap goDecl decls
  goDecl :: TopDecl Resolved -> [Unit]
  goDecl = \case
    Decide _ d  -> [mkUnit RuleUnit (decideApp d) (UDecide d)]
    Declare _ d -> [mkUnit TypeUnit (declareApp d) (UDeclare d)]
    Assume _ a  -> [mkUnit AssumptionUnit (assumeApp a) (UAssume a)]
    Section _ s -> goSection s
    Directive{} -> []
    Import{}    -> []
    Timezone{}  -> []
  mkUnit kind appHead decl =
    MkUnit
      { uPrimary = getUnique appHead
      , uModule  = (getUnique appHead).moduleUri
      , uKind    = kind
      , uHeading = resolvedText appHead
      , uDefs    = definedIn decl
      , uRefs    = referencedIn decl
      , uDecl    = decl
      }

-- | All renderable units across the open module and its dependencies, with
-- mixfix headings fixed up and @\@nlg@ calls expanded.
allUnits :: Module Resolved -> [Module Resolved] -> [Unit]
allUnits mainModule deps =
  let mods  = mainModule : deps
      canon = canonicalNames mods
      info  = nlgFnInfo mods
      ctors = recordCtorFields mods
  in map (substNlgInUnit info . mapUnitBody (normalizePositionalRecords ctors) . fixMixfixHeading canon)
       (extractUnits mainModule <> concatMap extractUnits deps)

-- | Apply a transform to a unit's DECIDE body (a no-op for other units).
mapUnitBody :: (Expr Resolved -> Expr Resolved) -> Unit -> Unit
mapUnitBody f u = case u.uDecl of
  UDecide (MkDecide a t af body) -> u { uDecl = UDecide (MkDecide a t af (f body)) }
  _ -> u

-- | The field-name references of each record, keyed by record name, so a
-- positional construction (@Bullet "A" "B"@) can be matched up with its fields.
-- Keyed by name, not 'Unique': a record's constructor occurrence and its
-- declaring type name do not share a 'Unique'.
recordCtorFields :: [Module Resolved] -> Map.Map Text [Resolved]
recordCtorFields mods = Map.fromList
  [ (resolvedText ctorName, [ n | MkTypedName _ n _ _ <- fields ])
  | m <- mods
  , Declare _ (MkDeclare _ _ (MkAppForm _ ctorName _ _) (RecordDecl _ _ fields)) <- topDeclsRec m
  ]

-- | All top-level declarations of a module, descending through @SECTION@s.
topDeclsRec :: Module Resolved -> [TopDecl Resolved]
topDeclsRec (MkModule _ _ sec) = go sec
 where
  go (MkSection _ _ _ ds) = concatMap (\d -> d : nested d) ds
  nested = \case Section _ s -> go s; _ -> []

-- | Rewrite a positional record construction (@App ctor [a, b]@) into the named
-- form (@AppNamed ctor [field IS a, …]@), so the table and record renderers —
-- which key off field names — recognise it the same as a @WITH@ literal.
normalizePositionalRecords :: Map.Map Text [Resolved] -> Expr Resolved -> Expr Resolved
normalizePositionalRecords ctorFields
  | Map.null ctorFields = id
  | otherwise = transformOf (gplate @(Expr Resolved)) $ \case
      App ann ctor args
        | Just fnames <- Map.lookup (resolvedText ctor) ctorFields
        , length fnames == length args ->
            AppNamed ann ctor [ MkNamedExpr ann f a | (f, a) <- zip fnames args ] Nothing
      e -> e

-- | For every function that carries an @\@nlg@ annotation: its authored
-- sentence and its GIVEN parameter uniques (in order), so a call's positional
-- arguments can be matched to the sentence's @%parameter%@ slots. The @\@nlg@
-- may attach to the declaration, the function name, or the body, so we look in
-- all three.
-- | Keyed by @(function name, arity)@ rather than 'Unique': a call site in the
-- importing module and the definition in a dependency module do not share
-- 'Unique's (each module is resolved independently), so a unique-based key
-- would never match across an @IMPORT@. The inner @%param%@ substitution stays
-- 'Unique'-based — those refs are self-consistent within the defining module.
nlgFnInfo :: [Module Resolved] -> Map.Map (Text, Int) (Nlg, [Unique])
nlgFnInfo mods = Map.fromList
  [ ((resolvedText headName, length appArgs), (nlg, [ getUnique a | a <- appArgs ]))
    -- Value parameters are the appform arguments, not the GIVEN names: a
    -- polymorphic function (@GIVEN a IS A TYPE@) lists its type parameter in
    -- GIVEN but never in the appform, so keying arity off GIVEN would not match
    -- the call's positional argument count.
  | m <- mods
  , d@(MkDecide _ _ (MkAppForm _ headName appArgs _) _) <- foldTopLevelDecides (: []) m
  , Just nlg <- [ decideNlg d ]
  ]

-- | The @\@nlg@ annotation attached to a DECIDE, wherever it landed: authors
-- place it differently for the @MEANS@ shorthand (lands on an appform argument)
-- vs the @DECIDE … IF@ form (lands on the head name), so we look in all of the
-- declaration's annotation-bearing positions.
decideNlg :: Decide Resolved -> Maybe Nlg
decideNlg (MkDecide decAnno (MkTypeSig _ (MkGivenSig _ names) _) (MkAppForm afAnno headName appArgs _) body) =
  foldr (<|>) Nothing $
       [ decAnno ^. annNlg
       , afAnno ^. annNlg
       , body ^. annoOf % annNlg
       , getOriginal headName ^. annoOf % annNlg
       ]
    <> [ getOriginal a ^. annoOf % annNlg | a <- appArgs ]
    <> [ getOriginal r ^. annoOf % annNlg | MkOptionallyTypedName _ r _ <- names ]

substNlgInUnit :: Map.Map (Text, Int) (Nlg, [Unique]) -> Unit -> Unit
substNlgInUnit info u = case u.uDecl of
  UDecide (MkDecide a t af body) ->
    u { uDecl = UDecide (MkDecide a t af (substituteNlgCalls info body)) }
  _ -> u

-- | Replace a call to an @\@nlg@-annotated function with its authored sentence,
-- splicing the call's arguments into the @%parameter%@ slots. Bottom-up, so
-- nested @\@nlg@ calls inside the arguments are expanded first.
substituteNlgCalls :: Map.Map (Text, Int) (Nlg, [Unique]) -> Expr Resolved -> Expr Resolved
substituteNlgCalls info = transformOf (gplate @(Expr Resolved)) $ \case
  App ann n args
    | Just (nlg, params) <- Map.lookup (resolvedText n, length args) info
    , length params == length args ->
        Inert ann (renderNlgWith (Map.fromList (zip params args)) nlg) InertCtxNone
  e -> e

-- | Render an @\@nlg@ annotation, substituting each parameter reference with the
-- corresponding call argument.
renderNlgWith :: Map.Map Unique (Expr Resolved) -> Nlg -> Text
renderNlgWith argMap = \case
  MkResolvedNlg _ frags -> normalizeWs (Text.concat (map frag frags))
  other                 -> simpleLinearizer other
 where
  frag (MkNlgText _ t) = t
  -- An unsubstituted reference (definition view, or a name with no matching
  -- argument) renders as the bare parameter name — NOT via 'simpleLinearizer',
  -- which would re-expand that parameter's own @\@nlg@ and recurse when the
  -- annotation is attached to a parameter it also references.
  frag (MkNlgRef _ r)  = case Map.lookup (getUnique r) argMap of
    Just a  -> simpleLinearizer a
    Nothing -> resolvedText r

-- | Map a mixfix function's 'Unique' to its canonical name (the one with @_@
-- holes). The defining occurrence keeps only the first keyword, but every
-- /referring/ occurrence carries the full canonical name, so we harvest it from
-- the use sites.
canonicalNames :: [Module Resolved] -> Map.Map Unique Text
canonicalNames mods = Map.fromList
  [ (getUnique r, nm)
  | m <- mods, r <- toList m, isRef r
  , let nm = nameToText (getActual r), Text.elem '_' nm
  ]
 where
  isRef Ref{} = True
  isRef _     = False

-- | Give a mixfix definition a readable heading by interleaving its GIVEN
-- parameter names back into the canonical pattern (@"the prepared form of _ at
-- order _"@ + params @the window@/@the order@ → @"the prepared form of the
-- window at order the order"@).
fixMixfixHeading :: Map.Map Unique Text -> Unit -> Unit
fixMixfixHeading canon u = case u.uDecl of
  UDecide (MkDecide _ tysig _ _)
    | Just pat <- Map.lookup u.uPrimary canon
    , Just h <- interleaveHoles pat (givenParamTexts tysig) ->
        u { uHeading = h }
  _ -> u

givenParamTexts :: TypeSig Resolved -> [Text]
givenParamTexts (MkTypeSig _ (MkGivenSig _ names) _) =
  [ resolvedText r | MkOptionallyTypedName _ r _ <- names ]

-- | Fill the @_@ holes of a space-separated pattern with the given texts.
interleaveHoles :: Text -> [Text] -> Maybe Text
interleaveHoles pat fillers =
  let toks  = Text.words pat
      holes = length (filter (== "_") toks)
  in if holes >= 1 && holes == length fillers
       then Just (Text.unwords (go toks fillers))
       else Nothing
 where
  go [] _ = []
  go (t : ts) fs
    | t == "_" = case fs of
        (f : rest) -> f : go ts rest
        []         -> t : go ts fs
    | otherwise = t : go ts fs

decideApp :: Decide Resolved -> Resolved
decideApp (MkDecide _ _ (MkAppForm _ n _ _) _) = n

declareApp :: Declare Resolved -> Resolved
declareApp (MkDeclare _ _ (MkAppForm _ n _ _) _) = n

assumeApp :: Assume Resolved -> Resolved
assumeApp (MkAssume _ _ (MkAppForm _ n _ _) _) = n

definedIn :: UnitDecl -> Set.Set Unique
definedIn = collectUniques (\case Def{} -> True; _ -> False)

referencedIn :: UnitDecl -> Set.Set Unique
referencedIn = collectUniques (\case Def{} -> False; _ -> True)

collectUniques :: (Resolved -> Bool) -> UnitDecl -> Set.Set Unique
collectUniques keep ud =
  Set.fromList [ getUnique r | r <- resolveds, keep r ]
 where
  resolveds = case ud of
    UDecide d  -> toList d
    UDeclare d -> toList d
    UAssume a  -> toList a

-- ----------------------------------------------------------------------------
-- Reachability
-- ----------------------------------------------------------------------------

data UnitGraph = MkUnitGraph
  { ugUnits :: !(Map.Map Int Unit)
  , ugEdges :: !(Map.Map Int (Set.Set Int))
  , ugMain  :: !(Set.Set Int)
  }

buildGraph :: NormalizedUri -> [Unit] -> UnitGraph
buildGraph mainUri units =
  MkUnitGraph
    { ugUnits = Map.fromList indexed
    , ugEdges = Map.fromList [ (i, depIndices u) | (i, u) <- indexed ]
    , ugMain  = Set.fromList [ i | (i, u) <- indexed, u.uModule == mainUri ]
    }
 where
  indexed = zip [0 ..] units
  owner = Map.fromList [ (u', i) | (i, u) <- indexed, u' <- Set.toList u.uDefs ]
  depIndices u = Set.fromList [ j | r <- Set.toList u.uRefs, Just j <- [Map.lookup r owner] ]

closure :: Map.Map Int (Set.Set Int) -> Set.Set Int -> Set.Set Int
closure edges = go Set.empty
 where
  go seen frontier
    | Set.null frontier = seen
    | otherwise =
        let seen'     = Set.union seen frontier
            next      = Set.unions [ Map.findWithDefault Set.empty i edges | i <- Set.toList frontier ]
            frontier' = next Set.\\ seen'
        in go seen' frontier'

reachableUnits :: UnitGraph -> Set.Set Int
reachableUnits g = closure g.ugEdges g.ugMain

-- | A define-before-use rank: a unit's transitive-dependency count. In a DAG a
-- dependency always has a strictly smaller count than its dependents, so
-- sorting by this yields a valid topological order.
rankByBlockId :: UnitGraph -> Map.Map Text Int
rankByBlockId g =
  Map.fromList
    [ (uniqueId u.uPrimary, Set.size (closure g.ugEdges (Set.singleton i)))
    | (i, u) <- Map.toList g.ugUnits ]

resolveDisposition :: ExportConfig -> Unit -> Disposition
resolveDisposition cfg u =
  case Map.lookup u.uPrimary cfg.unitDispositions of
    Just d  -> d
    Nothing -> Map.findWithDefault Inline u.uModule cfg.moduleDispositions

-- ----------------------------------------------------------------------------
-- Document construction
-- ----------------------------------------------------------------------------

buildDocument :: ExportConfig -> Module Resolved -> [Module Resolved] -> Document
buildDocument cfg mainModule deps =
  MkDocument
    { docTitle    = title
    -- Number after pruning, so dropped (e.g. test-fixture) sections never
    -- consume a number — numbering always starts at § 1 and stays contiguous.
    , docSections = renumberSections "" (mainSections <> importedSections)
    }
 where
  (title, bodySection) = documentTitleAndBody mainModule
  mainUri = moduleUri' mainModule
  units   = allUnits mainModule deps
  graph   = buildGraph mainUri units
  reach   = reachableUnits graph
  ranks   = rankByBlockId graph
  rankOf b = Map.findWithDefault 0 b.blockId ranks
  indexed = Map.toList graph.ugUnits
  isMain i = Set.member i graph.ugMain
  kept i = isMain i || not cfg.dropUnused || Set.member i reach

  inlineSeeds =
    Set.union graph.ugMain $
      Set.fromList
        [ i | (i, u) <- indexed, not (isMain i), kept i, resolveDisposition cfg u == Inline ]
  required = closure graph.ugEdges inlineSeeds

  dirRefs = Set.unions (map directiveRefsOf (mainModule : deps))
  -- Everything reachable, through the dependency graph, from a unit that is not
  -- itself a bare instance value — i.e. the operative rules and every
  -- definition they actually use. A value object that falls outside this
  -- closure is referenced only by directives (and other such objects).
  operativeClosure = closure graph.ugEdges
    (Set.fromList [ i | (i, u) <- indexed, not (isInstanceValueUnit u) ])

  isFixture i u =
       cfg.dropUnused && isMain i
    && not (Set.disjoint u.uDefs dirRefs)
    && not (Set.member i operativeClosure)
    && isInstanceValueUnit u
    && not (isExportUnit u)

  renderUnit i u
    | isMain i =
        if isFixture i u || isComputedSelector u
          then Nothing
          else Just (inlineBlock False u)
    | not (kept i) = Nothing
    | otherwise =
        case resolveDisposition cfg u of
          Inline    -> Just (inlineBlock True u)
          Reference -> Just (referenceBlock False u)
          Exclude
            | Set.member i required -> Just (referenceBlock True u)
            | otherwise             -> Nothing

  blockByUnique :: Map.Map Unique Block
  blockByUnique = Map.fromList [ (u.uPrimary, b) | (i, u) <- indexed, Just b <- [renderUnit i u] ]

  -- The open document's own material. A flat module (no SECTION markers) still
  -- gets proper numbered sections: its definitions, assumptions and operative
  -- rules each become a top-level section ("§ 1 Definitions", "§ 2 Provisions",
  -- …) so they appear in the table of contents under a heading, rather than as
  -- unlabelled prose under the title.
  mainSections = mapMaybe pruneSection (bodyToSections rankOf blockByUnique bodySection)

  -- An imported module that carries its own section title renders directly as a
  -- top-level section (its own heading). Imports with no section title of their
  -- own — only a file name to go by — are collected under a single "Imported
  -- definitions" appendix, so that generic header appears only when it is
  -- actually needed to label otherwise-untitled material.
  modByUri = Map.fromList [ (moduleUri' md, md) | md <- mainModule : deps ]
  importedRendered =
    [ (importHasSectionTitle md, s)
    | uri <- importedModuleOrder
    , Just md <- [Map.lookup uri modByUri]
    , Just s  <- [pruneSection (buildImportSection rankOf blockByUnique uri md)]
    ]
  importedSections =
    [ s | (True, s) <- importedRendered ]
    <> case [ s | (False, s) <- importedRendered ] of
         []   -> []
         secs -> [ MkDocSection
                     { sectionNumber  = ""
                     , sectionHeading = Just "Imported definitions"
                     , sectionGroups  = []
                     , sectionSubs    = secs
                     } ]
  importedModuleOrder = List.nub [ u.uModule | (i, u) <- indexed, not (isMain i) ]

-- | Build one section subtree, leaving 'sectionNumber' blank — numbering is a
-- separate post-pass ('renumberSections') run after empty sections are pruned.
toDocSection :: (Block -> Int) -> Map.Map Unique Block -> Section Resolved -> DocSection
toDocSection rankOf bmap (MkSection _ mName _ decls) =
  MkDocSection
    { sectionNumber  = ""
    , sectionHeading = fmap resolvedText mName
    , sectionGroups  = groupByKind rankOf (orderedBlocks bmap decls)
    , sectionSubs    = [ toDocSection rankOf bmap s | Section _ s <- decls ]
    }

-- | Turn the open document's body into top-level sections. Its directly-held
-- definitions / assumptions / rules each become a headed section (so they
-- carry a § number and a TOC entry); any explicit @SECTION@ markers follow as
-- further top-level sections.
bodyToSections :: (Block -> Int) -> Map.Map Unique Block -> Section Resolved -> [DocSection]
bodyToSections rankOf bmap (MkSection _ _ _ decls) =
  let groupSecs =
        [ MkDocSection
            { sectionNumber  = ""
            , sectionHeading = Just (groupSectionHeading g)
            , sectionGroups  = [ g { groupLabel = Nothing } ]
            , sectionSubs    = []
            }
        | g <- groupByKind rankOf (orderedBlocks bmap decls) ]
      subSecs = [ toDocSection rankOf bmap s | Section _ s <- decls ]
  in groupSecs <> subSecs

-- | Section heading for a promoted body group: the group's own label, or
-- "Provisions" for the (unlabelled) operative-rules group.
groupSectionHeading :: DocGroup -> Text
groupSectionHeading g = fromMaybe "Provisions" g.groupLabel

orderedBlocks :: Map.Map Unique Block -> [TopDecl Resolved] -> [Block]
orderedBlocks bmap decls =
  [ b | td <- decls, Just u <- [primaryOfTopDecl td], Just b <- [Map.lookup u bmap] ]

primaryOfTopDecl :: TopDecl Resolved -> Maybe Unique
primaryOfTopDecl = \case
  Decide _ d  -> Just (getUnique (decideApp d))
  Declare _ d -> Just (getUnique (declareApp d))
  Assume _ a  -> Just (getUnique (assumeApp a))
  _           -> Nothing

-- | Assign contiguous section numbers top-down: "1", "2", … at the top level
-- and "1.1", "1.2", … within. Run after 'pruneSection' so dropped sections
-- leave no gaps.
renumberSections :: Text -> [DocSection] -> [DocSection]
renumberSections parent secs =
  [ s { sectionNumber = num, sectionSubs = renumberSections num s.sectionSubs }
  | (i, s) <- zip [(1 :: Int) ..] secs
  , let num = childNumber parent i
  ]

childNumber :: Text -> Int -> Text
childNumber parent i
  | Text.null parent = Text.pack (show i)
  | otherwise        = parent <> "." <> Text.pack (show i)

-- | Group into /Definitions/ (types, sorted define-before-use), /Assumptions/,
-- then operative rules. Empty groups are dropped.
--
-- The operative-rules group is normally unlabelled — when it is the section's
-- only material the section heading already frames it. But when it shares a
-- section with labelled Definitions/Assumptions groups it would otherwise
-- render as a second, header-less clause list whose numbering restarts at 1
-- (two "1." items with nothing between them). In that case we give it a
-- "Provisions" heading so each numbered list sits under its own label.
groupByKind :: (Block -> Int) -> [Block] -> [DocGroup]
groupByKind rankOf blocks =
  case groups of
    (_ : _ : _) -> map labelRules groups
    _           -> groups
 where
  groups =
    [ MkDocGroup { groupLabel = lbl, groupBlocks = bs }
    | (lbl, k, sortIt) <- [ (Just "Definitions", TypeUnit, True)
                          , (Just "Assumptions", AssumptionUnit, False)
                          , (Nothing, RuleUnit, False)
                          ]
    , let raw = filter (\b -> b.blockKind == k) blocks
          bs  = if sortIt then List.sortOn rankOf raw else raw
    , not (null bs)
    ]
  labelRules g = case g.groupLabel of
    Nothing -> g { groupLabel = Just "Provisions" }
    Just _  -> g

pruneSection :: DocSection -> Maybe DocSection
pruneSection s =
  let subs' = mapMaybe pruneSection s.sectionSubs
  in if null s.sectionGroups && null subs'
       then Nothing
       else Just s { sectionSubs = subs' }

directiveRefsOf :: Module Resolved -> Set.Set Unique
directiveRefsOf (MkModule _ _ section) = goSection section
 where
  goSection (MkSection _ _ _ decls) = Set.unions (map goDecl decls)
  goDecl = \case
    Directive _ d -> Set.fromList (map getUnique (toList d))
    Section _ s   -> goSection s
    _             -> Set.empty

-- | A parameterless definition whose body is constant /data/ — a record or
-- list literal, a constructor application (e.g. @Date 15 5 2025@), or a bare
-- literal — as opposed to an operative rule (which has parameters or a body
-- with control flow / logic). Such units are the only ones eligible to be
-- dropped as directive-only fixtures.
isInstanceValueUnit :: Unit -> Bool
isInstanceValueUnit u = case u.uDecl of
  UDecide (MkDecide _ (MkTypeSig _ (MkGivenSig _ params) _) _ body)
    | null params -> case carameliseNode body of
        AppNamed{} -> True
        List{}     -> True
        App{}      -> True   -- a constructor application: Date d m y, an enum case, …
        Lit{}      -> True   -- a bare numeric / string literal
        _          -> False
  _ -> False

isExportUnit :: Unit -> Bool
isExportUnit u = case u.uDecl of
  UDecide d -> isExportedDecide d
  _         -> False

-- | A lifted computed-field selector — its GIVEN parameter is the implicit
-- @_self@. These are shown under their record, not as standalone rules.
isComputedSelector :: Unit -> Bool
isComputedSelector u = case u.uDecl of
  UDecide (MkDecide _ (MkTypeSig _ (MkGivenSig _ params) _) _ _) ->
    any (\(MkOptionallyTypedName _ r _) -> nameToText (getActual r) == "_self") params
  _ -> False

-- ----------------------------------------------------------------------------
-- Block rendering
-- ----------------------------------------------------------------------------

inlineBlock :: Bool -> Unit -> Block
inlineBlock imported u =
  let (connector, body) = unitRendering u
  in MkBlock
    { blockId           = uniqueId u.uPrimary
    , blockKind         = u.uKind
    , blockHeading      = u.uHeading
    , blockConnector    = connector
    , blockBody         = body
    , blockRenderedAs   = RenderedInline
    , blockCitation     = Nothing
    , blockSourceModule = uriText u.uModule
    , blockImported     = imported
    }

referenceBlock :: Bool -> Unit -> Block
referenceBlock isForced u =
  MkBlock
    { blockId           = uniqueId u.uPrimary
    , blockKind         = u.uKind
    , blockHeading      = u.uHeading
    , blockConnector    = ""
    , blockBody         = CLeaf ""
    , blockRenderedAs   = RenderedReference isForced
    , blockCitation     = Just ("as defined in " <> moduleLabel u.uModule)
    , blockSourceModule = uriText u.uModule
    , blockImported     = True
    }

unitRendering :: Unit -> (Text, Clause)
unitRendering u = case u.uDecl of
  UDecide d@(MkDecide _ tysig _ body)
    -- A function with an @\@nlg@ annotation renders AS its authored description
    -- (with parameter names filling the @%slots%@) rather than its
    -- implementation — so e.g. @filter@ reads "the items of list for which f
    -- holds", not its recursive @CONSIDER@ body.
    | Just nlg <- decideNlg d ->
        ( if returnsBoolean tysig then "holds if" else "means"
        , CLeaf (renderNlgWith Map.empty nlg) )
    -- Otherwise rewrite the body up front (mixfix/maths/dates/numbers/params ->
    -- Inert leaves) so formula mode survives arithmetic that contains a mixfix
    -- call or a function like @exp@/@ln@. A top-level @WHERE@/@LET@ is peeled off
    -- first so its local definitions (which 'rewriteExpr' would otherwise drop)
    -- render as an attached "where:" block.
    | otherwise ->
        let subst              = paramSubst tysig
            (inner, locals)    = peelWhere body
            (conn, mainClause) = decideRendering tysig (rewriteExpr (substParams subst inner))
        in case mapMaybe (localDefClause subst) locals of
             []   -> (conn, mainClause)
             defs -> (conn, CWhere mainClause defs)
  UAssume _ -> ("is assumed to be given", CLeaf "")
  UDeclare (MkDeclare _ _ _ tyDecl) ->
    case tyDecl of
      RecordDecl _ _ (_ : _) -> ("means a record with", declareClause tyDecl)
      EnumDecl _ cons
        | longEnum cons -> ("means one of", CFields [ (conName c, "") | c <- cons ])
      _                      -> ("means", declareClause tyDecl)
 where
  conName (MkConDecl _ n _) = resolvedText n
  -- Bullet a long enum, or one whose cases are long phrases, rather than
  -- running them all into one "one of …" sentence.
  longEnum cons =
    length cons > 5 || any (\c -> Text.length (conName c) > 24) cons

decideRendering :: TypeSig Resolved -> Expr Resolved -> (Text, Clause)
decideRendering tysig body
  | Just t <- nlgOf body =
      (connectorFor tysig body, CLeaf t)   -- an authored @nlg sentence wins
  | Regulative _ deon <- body =
      ("provides that", deonticClause deon)
  | Just (cols, rows) <- bodyAsTable body =
      ("means", tableClause cols rows)
  | Just items <- bodyAsValueList body =
      ("means", CChain "a list of the following" items)
  | Just (ctor, fields) <- bodyAsRecordLiteral body =
      ("is a " <> ctor <> " with", CFields (map fieldPair fields))
  | Just c <- arithChainClause body =
      ("equals", c)
  | Just f <- formulaText body =
      ("equals", CLeaf (normalizeWs f))
  | otherwise =
      (connectorFor tysig body, toClause body)
 where
  fieldPair (MkNamedExpr _ f v) = (resolvedText f, leafText v)

-- | Peel a top-level @WHERE@ / @LET … IN@ wrapper off a rule body, returning the
-- inner body and its local definitions. Only the rule's own top-level locals are
-- surfaced; @WHERE@s nested deeper in the body are still flattened away by
-- 'rewriteExpr'.
peelWhere :: Expr Resolved -> (Expr Resolved, [LocalDecl Resolved])
peelWhere = \case
  Where _ e ds -> (e, ds)
  LetIn _ ds e -> (e, ds)
  e            -> (e, [])

-- | Render one local @WHERE@/@LET@ binding as a (heading, connector, body)
-- triple. The enclosing rule's GIVEN parameters are in scope inside the binding,
-- so its parameter substitution is threaded in alongside the binding's own.
localDefClause :: Map.Map Unique Text -> LocalDecl Resolved -> Maybe (Text, Text, Clause)
localDefClause outerSubst = \case
  LocalDecide _ d@(MkDecide _ ltysig _ lbody) ->
    let subst          = Map.union (paramSubst ltysig) outerSubst
        (conn, clause) = decideRendering ltysig (rewriteExpr (substParams subst lbody))
    in Just (resolvedText (decideApp d), conn, clause)
  LocalAssume{} -> Nothing

-- | Map each GIVEN parameter to a readable noun phrase: a record/enum-typed
-- parameter becomes "the <type>" (so the bound name @p@ reads as "the
-- payment"); a scalar parameter keeps its own name with underscores spaced.
-- Two parameters of the same type are disambiguated by their name.
paramSubst :: TypeSig Resolved -> Map.Map Unique Text
paramSubst (MkTypeSig _ (MkGivenSig _ names) _) =
  let params = [ (getUnique r, nameToText (getActual r), mty >>= userTypeName)
               | MkOptionallyTypedName _ r mty <- names ]
      dupTypes = Map.fromListWith (+) [ (tn, 1 :: Int) | (_, _, Just tn) <- params ]
      phraseOf pname = \case
        Just tn
          | Map.findWithDefault 0 tn dupTypes > 1 -> "the " <> noun tn <> " " <> cleanParam pname
          | otherwise                             -> "the " <> noun tn
        Nothing -> cleanParam pname
      noun = Text.toLower . spaceCamel
  in Map.fromList [ (u, phraseOf pname mtn) | (u, pname, mtn) <- params ]

substParams :: Map.Map Unique Text -> Expr Resolved -> Expr Resolved
substParams pm
  | Map.null pm = id
  | otherwise = transformOf (gplate @(Expr Resolved)) $ \case
      App ann n [] | Just phrase <- Map.lookup (getUnique n) pm -> Inert ann phrase InertCtxNone
      e -> e

-- | The name of a user-defined (non-builtin) nullary type constructor.
userTypeName :: Type' Resolved -> Maybe Text
userTypeName = \case
  TyApp _ n [] ->
    let t = nameToText (getActual n)
    in if t `elem` builtinTypeNames then Nothing else Just t
  _ -> Nothing

builtinTypeNames :: [Text]
builtinTypeNames =
  ["NUMBER", "INTEGER", "STRING", "BOOLEAN", "DATE", "TIME", "MONEY", "PERCENT", "LIST", "MAYBE"]

cleanParam :: Text -> Text
cleanParam = Text.map (\c -> if c == '_' then ' ' else c)

-- | Insert a space at lower->upper boundaries so @LoanState@ becomes
-- @Loan State@ and @ProductionOrder@ becomes @Production Order@.
spaceCamel :: Text -> Text
spaceCamel = Text.pack . go . Text.unpack
 where
  go (a : b : rest) | isLower a, isUpper b = a : ' ' : go (b : rest)
  go (a : rest)     = a : go rest
  go []             = []

connectorFor :: TypeSig Resolved -> Expr Resolved -> Text
connectorFor tysig body
  | returnsBoolean tysig = "holds if"
  | otherwise = case carameliseNode body of
      Consider{} -> "is determined by"
      _          -> "means"

returnsBoolean :: TypeSig Resolved -> Bool
returnsBoolean (MkTypeSig _ _ mGiveth) = case mGiveth of
  Just (MkGivethSig _ ty) -> typeNameText ty == "BOOLEAN"
  Nothing                 -> False

typeNameText :: Type' Resolved -> Text
typeNameText = \case
  TyApp _ n _ -> nameToText (getActual n)
  _           -> ""

bodyAsRecordLiteral :: Expr Resolved -> Maybe (Text, [NamedExpr Resolved])
bodyAsRecordLiteral = \case
  AppNamed _ ctor nes _ -> Just (resolvedText ctor, nes)
  _                     -> Nothing

-- | A @LIST@ of record literals that all share the same field columns: the
-- column headings and the per-row field-value expressions. Renders as a table.
bodyAsTable :: Expr Resolved -> Maybe ([Text], [[Expr Resolved]])
bodyAsTable e = case carameliseNode e of
  List _ items
    | Just recs@(firstRec : _) <- traverse asRecord items
    , let cols = map fst firstRec
    , not (null cols)
    , all (\r -> map fst r == cols) recs ->
        Just (cols, [ map snd r | r <- recs ])
  _ -> Nothing
 where
  asRecord ex = case carameliseNode ex of
    AppNamed _ _ nes _ -> Just [ (resolvedText f, v) | MkNamedExpr _ f v <- nes ]
    _                  -> Nothing

tableClause :: [Text] -> [[Expr Resolved]] -> Clause
tableClause cols rows = CTable cols [ map cellClause row | row <- rows ]

-- | A literal list of more than three (non-record) values — e.g. strings,
-- numbers, dates — renders as a bullet list rather than a comma-run sentence.
-- Lists of records are handled by 'bodyAsTable' (checked first), and short
-- lists stay inline.
bodyAsValueList :: Expr Resolved -> Maybe [Text]
bodyAsValueList e = case carameliseNode e of
  List _ items | length items >= arithChainMinTerms -> Just (map leafText items)
  _                                                  -> Nothing

-- | Render a table cell: a nested list-of-records becomes its own (nested)
-- table, a nested record a small field list, anything else a value — and a
-- reference to another defined list links to that list's table via the normal
-- cross-reference mechanism.
cellClause :: Expr Resolved -> Clause
cellClause v
  | Just (cols, rows) <- bodyAsTable v = tableClause cols rows
  | Just (_, fields) <- bodyAsRecordLiteral v =
      CFields [ (resolvedText f, leafText fv) | MkNamedExpr _ f fv <- fields ]
  | otherwise = CLeaf (leafText v)

declareClause :: TypeDecl Resolved -> Clause
declareClause = \case
  RecordDecl _ _ []     -> CLeaf "an empty record"
  RecordDecl _ _ fields -> CFields (map recordField fields)
  EnumDecl _ [c]        -> CLeaf (conName c)
  EnumDecl _ cons       -> CLeaf ("one of " <> oxford "or" (map conName cons))
  SynonymDecl _ ty      -> CLeaf (typeText ty)
 where
  recordField (MkTypedName _ n ty Nothing)     = (resolvedText n, typeText ty)
  recordField (MkTypedName _ n ty (Just expr)) =
    (resolvedText n, typeText ty <> ", computed as " <> leafText expr)
  conName (MkConDecl _ n _) = resolvedText n

-- ----------------------------------------------------------------------------
-- Expression -> Clause tree
-- ----------------------------------------------------------------------------

toClause :: Expr Resolved -> Clause
toClause e0
  | Just t <- nlgOf e0 = CLeaf t   -- an authored @nlg sentence wins
toClause e0 = case carameliseNode e0 of
  IfThenElse _ c t f -> buildIf [(condText c, toClause t)] f
  MultiWayIf _ gs o  ->
    CIf [ (condText gc, toClause gf) | MkGuardedExpr _ gc gf <- gs ] (Just (toClause o))
  Consider _ s brs   -> CCases (inlineProse s) (map branchPair brs)
  Regulative _ deon  -> deonticClause deon
  And{}              -> CAll (map toClause (flattenAnd e0))
  Or{}               -> CAny (map toClause (flattenOr e0))
  -- A function applied to a single control-flow argument distributes over that
  -- argument's branches: "the day before (CONSIDER … THEN x …)" lays out as a
  -- case tree whose leaves are "the day before x" — a readable outline rather
  -- than one run-on line that inlines the whole nested CONSIDER.
  App ann n [arg]
    | isControlFlowExpr (carameliseNode arg) ->
        toClause (distributeIntoBranches (\leaf -> App ann n [leaf]) arg)
  e | Just c <- arithChainClause e -> c
    | Just f <- formulaText e       -> CLeaf (normalizeWs f)
    | otherwise                     -> CLeaf (condText e)
 where
  buildIf acc f = case carameliseNode f of
    IfThenElse _ c2 t2 f2 -> buildIf (acc <> [(condText c2, toClause t2)]) f2
    MultiWayIf _ gs o     ->
      CIf (acc <> [ (condText gc, toClause gf) | MkGuardedExpr _ gc gf <- gs ]) (Just (toClause o))
    _                     -> CIf acc (Just (toClause f))
  branchPair (MkBranch _ lhs body) = (branchLhsText lhs, toClause body)

-- | Whether an expression is a branching construct (so it should lay out as a
-- structured clause rather than inline prose).
isControlFlowExpr :: Expr Resolved -> Bool
isControlFlowExpr = \case
  Consider{}   -> True
  IfThenElse{} -> True
  MultiWayIf{} -> True
  _            -> False

-- | Push a wrapper (e.g. an enclosing function application) down to the result
-- leaves of a control-flow expression, preserving its branch structure. Sound
-- because L4 is pure: @f (IF c THEN x ELSE y) = IF c THEN f x ELSE f y@.
distributeIntoBranches :: (Expr Resolved -> Expr Resolved) -> Expr Resolved -> Expr Resolved
distributeIntoBranches wrap e = case carameliseNode e of
  Consider ann s brs ->
    Consider ann s [ MkBranch a lhs (distributeIntoBranches wrap b) | MkBranch a lhs b <- brs ]
  IfThenElse ann c t f ->
    IfThenElse ann c (distributeIntoBranches wrap t) (distributeIntoBranches wrap f)
  MultiWayIf ann gs o ->
    MultiWayIf ann
      [ MkGuardedExpr a gc (distributeIntoBranches wrap g) | MkGuardedExpr a gc g <- gs ]
      (distributeIntoBranches wrap o)
  leaf -> wrap leaf

branchLhsText :: BranchLhs Resolved -> Text
branchLhsText = \case
  When _ pat  -> patternText pat
  Otherwise _ -> "otherwise"

flattenAnd :: Expr Resolved -> [Expr Resolved]
flattenAnd e = case carameliseNode e of
  And _ a b  -> flattenAnd a <> flattenAnd b
  RAnd _ a b -> flattenAnd a <> flattenAnd b
  x          -> [x]

flattenOr :: Expr Resolved -> [Expr Resolved]
flattenOr e = case carameliseNode e of
  Or _ a b  -> flattenOr a <> flattenOr b
  ROr _ a b -> flattenOr a <> flattenOr b
  x         -> [x]

-- | Render a boolean condition in plain legal English: comparisons use words
-- ("is", "is at least"); an OR of equalities on the same value collapses to
-- "… is one of A, B or C".
condText :: Expr Resolved -> Text
condText e
  | Just t <- nlgOf e = t
condText e = case carameliseNode e of
  Equals _ a b         -> side a <> " is " <> side b
  Not _ inner          -> case carameliseNode inner of
    Equals _ a b -> side a <> " is not " <> side b
    App _ _ []   -> side inner <> " is false"   -- a boolean variable
    Proj{}       -> side inner <> " is false"   -- a boolean field access
    _            -> "it is not the case that " <> condText inner
  Geq _ a b            -> side a <> " is at least " <> side b
  Leq _ a b            -> side a <> " is at most " <> side b
  Gt _ a b             -> side a <> " is more than " <> side b
  Lt _ a b             -> side a <> " is less than " <> side b
  orChain@Or{}         -> case oneOfText orChain of
                            Just t  -> t
                            Nothing -> joinCond " or " (flattenOr orChain)
  ROr _ _ _            -> joinCond " or " (flattenOr e)
  And{}                -> joinCond " and " (flattenAnd e)
  RAnd _ _ _           -> joinCond " and " (flattenAnd e)
  e'                   -> inlineProse e'
 where
  side = inlineProse
  joinCond sep parts = Text.intercalate sep (map condText parts)

-- | Detect @x = A OR x = B OR …@ (same left side) and collapse to a membership.
oneOfText :: Expr Resolved -> Maybe Text
oneOfText e = do
  pairs <- orEquals e
  case pairs of
    ((lhs0, r0) : rest)
      | all ((== lhs0) . fst) rest ->
          Just (lhs0 <> " is one of " <> oxford "or" (r0 : map snd rest))
    _ -> Nothing
 where
  orEquals x = case carameliseNode x of
    Or _ a b     -> (<>) <$> orEquals a <*> orEquals b
    ROr _ a b    -> (<>) <$> orEquals a <*> orEquals b
    Equals _ a b -> Just [(inlineProse a, inlineProse b)]
    _            -> Nothing

-- | A pattern as English: unwrap @JUST@, drop the @… has@ scaffolding for
-- nullary constructors.
patternText :: Pattern Resolved -> Text
patternText = \case
  PatApp _ con []  -> resolvedText con
  PatApp _ con [p] | nameToText (getActual con) == "JUST" -> patternText p
  PatApp _ con ps  -> resolvedText con <> " (" <> oxford "and" (map patternText ps) <> ")"
  PatVar _ v       -> resolvedText v
  PatLit _ l       -> litText l
  PatCons _ a b    -> patternText a <> " followed by " <> patternText b
  PatExpr _ ex     -> inlineProse ex
 where
  litText (NumericLit _ r) = prettyRatio r
  litText (StringLit _ t)  = t

-- ----------------------------------------------------------------------------
-- Deontic rules
-- ----------------------------------------------------------------------------

deonticClause :: Deonton Resolved -> Clause
deonticClause (MkDeonton _ party (MkAction _ modal actPat mprov) mdue mhence mlest) =
  CDeontic
    (inlineProse party)
    (modalWord modal)
    (patternText actPat)
    (dueText mdue)
    (fmap inlineProse mprov)
    (mhence >>= henceClause)
    (fmap consequence mlest)
 where
  dueText (Just d) | not (isZeroLit d) = Just (inlineProse d)
  dueText _ = Nothing
  henceClause h
    | isFulfilled h = Nothing
    | otherwise     = Just (consequence h)
  -- A BREACH branch already says "breach"; render only the (quoted) reason
  -- rather than the redundant "breach by <party>".
  consequence e = case e of
    Breach _ _ mReason ->
      CLeaf ("the obligation is breached"
               <> maybe "" (\r -> " because “" <> inlineProse r <> "”") mReason)
    _ -> toClause e

modalWord :: DeonticModal -> Text
modalWord = \case
  DMust    -> "must"
  DMay     -> "may"
  DMustNot -> "must not"
  DDo      -> "shall"

isZeroLit :: Expr Resolved -> Bool
isZeroLit = \case
  Lit _ (NumericLit _ r) -> r == 0
  _                      -> False

isFulfilled :: Expr Resolved -> Bool
isFulfilled = \case
  App _ n [] -> nameToText (getActual n) == "FULFILLED"
  _          -> False

-- | A non-structured leaf value: a formula if it is pure arithmetic, otherwise
-- prose.
leafText :: Expr Resolved -> Text
leafText e = case formulaText (rewriteExpr e) of
  Just f  -> normalizeWs f
  Nothing -> inlineProse e

-- | A top-level arithmetic chain (a sum/product/… whose operator repeats) with
-- this many operands or more renders as a bulleted list rather than one long
-- formula line.
arithChainMinTerms :: Int
arithChainMinTerms = 4

-- | If an expression is a top-level additive (@+@/@−@) or multiplicative
-- (@×@/@÷@) chain of at least 'arithChainMinTerms' operands, lay it out as a
-- list: a lead-in plus one pre-rendered line per operand. A uniform chain (all
-- @+@, or all @×@) gets a "the sum of" / "the product of" lead and plain lines;
-- a mixed chain gets a neutral lead and an operator word ("plus", "minus",
-- "times", "divided by") in front of each line after the first.
arithChainClause :: Expr Resolved -> Maybe Clause
arithChainClause e = case carameliseNode e of
  Plus{}      -> chain (flattenChain additiveStep "+" e) "+" "the sum of the following" addWord
  Minus{}     -> chain (flattenChain additiveStep "+" e) "+" "the sum of the following" addWord
  Times{}     -> chain (flattenChain multStep "×" e) "×" "the product of the following" mulWord
  DividedBy{} -> chain (flattenChain multStep "×" e) "×" "the product of the following" mulWord
  _           -> Nothing
 where
  chain terms firstOp uniformLead word
    | length terms < arithChainMinTerms = Nothing
    | otherwise =
        let uniform = all ((== firstOp) . fst) terms
            lead    = if uniform then uniformLead else "the following"
            line i (op, t)
              | i == (0 :: Int) || uniform = leafText t
              | otherwise                  = word op <> " " <> leafText t
        in Just (CChain lead (zipWith line [0 ..] terms))
  addWord o = if o == "-" then "minus" else "plus"
  mulWord o = if o == "÷" then "divided by" else "times"

-- | Walk the left spine of an operator chain into @(operator, operand)@ pairs,
-- leftmost first (its operator is the chain's identity, "+" or "×"). Right
-- operands are kept whole — only the same-precedence left spine is flattened, so
-- the result is faithful regardless of nesting.
flattenChain :: (Expr Resolved -> Maybe (Text, Expr Resolved, Expr Resolved))
             -> Text -> Expr Resolved -> [(Text, Expr Resolved)]
flattenChain step identityOp = go []
 where
  go acc e = case step (carameliseNode e) of
    Just (op, a, b) -> go ((op, b) : acc) a
    Nothing         -> (identityOp, e) : acc

additiveStep :: Expr Resolved -> Maybe (Text, Expr Resolved, Expr Resolved)
additiveStep = \case
  Plus _ a b  -> Just ("+", a, b)
  Minus _ a b -> Just ("-", a, b)
  _           -> Nothing

multStep :: Expr Resolved -> Maybe (Text, Expr Resolved, Expr Resolved)
multStep = \case
  Times _ a b     -> Just ("×", a, b)
  DividedBy _ a b -> Just ("÷", a, b)
  _               -> Nothing

-- ----------------------------------------------------------------------------
-- Formula mode (pure arithmetic -> symbols)
-- ----------------------------------------------------------------------------

-- | If the expression is /entirely/ arithmetic over numbers, variables and
-- projections — /and/ actually contains an arithmetic operator at the top (so a
-- bare variable or projection is left as prose) — render it with operator
-- symbols and minimal parentheses.
formulaText :: Expr Resolved -> Maybe Text
formulaText e
  | arithTop (carameliseNode e) = fmap snd (go e)
  | otherwise                   = Nothing
 where
  arithTop = \case
    Plus{} -> True; Minus{} -> True; Times{} -> True
    DividedBy{} -> True; Modulo{} -> True; Exponent{} -> True
    _ -> False
  go :: Expr Resolved -> Maybe (Int, Text)
  go expr = case carameliseNode expr of
    Plus _ a b      -> bin 1 " + " a b
    Minus _ a b     -> bin 1 " − " a b
    Times _ a b     -> bin 2 " × " a b
    DividedBy _ a b -> bin 2 " ÷ " a b
    Modulo _ a b    -> bin 2 " mod " a b
    Exponent _ a b  -> bin 3 " ^ " a b
    Lit _ (NumericLit _ r) -> Just (4, numText r)
    Percent _ x     -> do (_, t) <- go x; Just (4, t <> "%")
    App _ n []      -> Just (4, resolvedText n)
    Inert _ t _     -> Just (4, t)   -- a substituted parameter phrase
    Proj _ a nm     -> do (_, t) <- go a; Just (4, t <> "'s " <> resolvedText nm)
    -- A non-arithmetic operand (a function call, projection chain, parameter …)
    -- becomes an inline atom rendered as prose, so a formula that mixes
    -- arithmetic with such terms still reads as "a + b × f(x)" rather than
    -- collapsing the whole expression back to nested "the sum of …" prose.
    other           -> Just (4, normalizeWs (simpleLinearizer other))
  bin p op a b = do
    (pa, ta) <- go a
    (pb, tb) <- go b
    Just (p, wrap p pa ta <> op <> wrap p pb tb)
  wrap parentP childP t = if childP < parentP then "(" <> t <> ")" else t

numText :: Rational -> Text
numText r = fromMaybe (prettyRatio r) (groupedInt r)

-- ----------------------------------------------------------------------------
-- AST rewrites for readability (dates, large numbers, implicit self)
-- ----------------------------------------------------------------------------

inlineProse :: Expr Resolved -> Text
inlineProse e = case nlgOf e of
  Just t  -> t
  Nothing -> normalizeWs (simpleLinearizer (rewriteExpr e))

-- | An authored @\@nlg@ sentence on an expression, if present, takes precedence
-- over the structural rendering.
nlgOf :: Expr Resolved -> Maybe Text
nlgOf e = (normalizeWs . simpleLinearizer) <$> (e ^. annoOf % annNlg)

-- | A single bottom-up rewrite of an expression for readability. One pass (not
-- a chain) so an inner transform's 'Inert' result is in place before an outer
-- transform linearizes it.
rewriteExpr :: Expr Resolved -> Expr Resolved
rewriteExpr = transformOf (gplate @(Expr Resolved)) rw
 where
  inert ann t = Inert ann t InertCtxNone
  rw = \case
    -- Drop WHERE / LET … IN binding wrappers; the body still reads.
    Where _ e _ -> e
    LetIn _ _ e -> e
    -- The implicit computed-field self.
    App ann n [] | nameIs "_self" n -> inert ann "it"
    -- A date constructor.
    App ann n args | nameIs "Date" n, Just t <- dateFromArgs args -> inert ann t
    -- Unary maths read as f(x), not "f with x".
    App ann n [x] | Just fn <- Map.lookup (nameToText (getActual n)) mathUnaryFns ->
      inert ann (fn <> "(" <> simpleLinearizer x <> ")")
    -- Unary date/time component primitives read as "the year of x", not
    -- "DATE_YEAR with x". These appear in library bodies (and inlined calls).
    App ann n [x] | Just phrase <- Map.lookup (nameToText (getActual n)) builtinUnaryOf ->
      inert ann ("the " <> phrase <> " of " <> simpleLinearizer x)
    -- Mixfix: interleave the arguments into the @_@ holes.
    App ann n args@(_ : _) | Just txt <- interleaveMixfix n args -> inert ann txt
    -- A function whose name ends in a preposition reads "minimum of x and y",
    -- not the linearizer's "minimum of with x and y".
    App ann n args@(_ : _) | endsInPreposition n ->
      inert ann (resolvedText n <> " " <> oxford "and" (map simpleLinearizer args))
    -- A boolean field access drops the possessive: "the car has four wheels".
    Proj ann a n | predicateField n ->
      inert ann (simpleLinearizer a <> " " <> resolvedText n)
    -- Comma-group large integers.
    Lit ann (NumericLit _ r) | Just t <- groupedInt r -> inert ann t
    e -> e

mathUnaryFns :: Map.Map Text Text
mathUnaryFns = Map.fromList
  [ ("exp", "exp"), ("ln", "ln"), ("log", "log"), ("sqrt", "sqrt")
  , ("abs", "abs"), ("floor", "floor"), ("ceiling", "ceil"), ("round", "round") ]

-- | Unary date/time-component builtins, rendered as "the <word> of <x>".
builtinUnaryOf :: Map.Map Text Text
builtinUnaryOf = Map.fromList
  [ ("DATE_YEAR", "year"), ("DATE_MONTH", "month"), ("DATE_DAY", "day")
  , ("DATETIME_DATE", "date"), ("DATETIME_TIME", "time")
  , ("DATETIME_TZ", "time zone")
  , ("TIME_HOUR", "hour"), ("TIME_MINUTE", "minute"), ("TIME_SECOND", "second")
  , ("DATETIME_SERIAL", "serial number"), ("TIME_SERIAL", "serial number")
  , ("DATE_SERIAL", "serial number") ]

interleaveMixfix :: Resolved -> [Expr Resolved] -> Maybe Text
interleaveMixfix n args =
  let toks  = Text.words (nameToText (getActual n))
      holes = length (filter (== "_") toks)
  in if holes >= 1 && holes == length args
       then Just (Text.unwords (fill toks args))
       else Nothing
 where
  fill [] _ = []
  fill (t : ts) as
    | t == "_" = case as of
        (a : rest) -> simpleLinearizer a : fill ts rest
        []         -> t : fill ts as
    | otherwise = t : fill ts as

nameIs :: Text -> Resolved -> Bool
nameIs t r = nameToText (getActual r) == t

-- | A function whose name ends in a preposition, where "f with x" reads worse
-- than "f x" (e.g. "minimum of", "subject to CPF for").
endsInPreposition :: Resolved -> Bool
endsInPreposition r = case reverse (Text.words (nameToText (getActual r))) of
  (w : _) -> Text.toLower w `Set.member` prepositionWords
  []      -> False

prepositionWords :: Set.Set Text
prepositionWords = Set.fromList
  ["of", "for", "to", "from", "into", "onto", "on", "in", "by", "at", "over"
  , "under", "between", "than", "as"]

-- | A field name that reads as a predicate, so its projection drops the
-- possessive: "the car has four wheels" rather than "the car's has four wheels".
predicateField :: Resolved -> Bool
predicateField r = case Text.words (nameToText (getActual r)) of
  (w : _) -> Text.toLower w `Set.member` predicateLeaders
  []      -> False

predicateLeaders :: Set.Set Text
predicateLeaders = Set.fromList
  ["is", "are", "was", "were", "has", "have", "had", "may", "must", "shall"
  , "can", "could", "should", "will", "does", "did", "do"]

dateFromArgs :: [Expr Resolved] -> Maybe Text
dateFromArgs [d, m, y]
  | Just di <- intLit d, Just mi <- intLit m, Just yi <- intLit y, mi >= 1, mi <= 12 =
      Just (showInt di <> " " <> monthName mi <> " " <> showInt yi)
dateFromArgs _ = Nothing

intLit :: Expr Resolved -> Maybe Integer
intLit (Lit _ (NumericLit _ r)) | denominator r == 1 = Just (numerator r)
intLit _ = Nothing

groupedInt :: Rational -> Maybe Text
groupedInt r
  | denominator r == 1, abs (numerator r) >= 10000 = Just (groupInt (numerator r))
  | otherwise = Nothing

groupInt :: Integer -> Text
groupInt n =
  let ds = show (abs n)
      grouped = reverse (List.intercalate "," (chunksOf 3 (reverse ds)))
  in Text.pack ((if n < 0 then "-" else "") <> grouped)

showInt :: Integer -> Text
showInt = Text.pack . show

monthName :: Integer -> Text
monthName = \case
  1 -> "January"; 2 -> "February"; 3 -> "March"; 4 -> "April"
  5 -> "May"; 6 -> "June"; 7 -> "July"; 8 -> "August"
  9 -> "September"; 10 -> "October"; 11 -> "November"; 12 -> "December"
  m -> "month " <> showInt m

-- ----------------------------------------------------------------------------
-- Text tidying
-- ----------------------------------------------------------------------------

normalizeWs :: Text -> Text
normalizeWs t0 =
  let t1 = Text.replace "it 's" "its" t0
      t2 = Text.replace " 's"  "'s"  t1
      t3 = Text.replace " %"   "%"   t2
      t4 = Text.replace " ,"   ","   t3
      t5 = Text.replace " ."   "."   t4
      t6 = Text.replace " :"   ":"   t5
      t7 = Text.replace " ;"   ";"   t6
      -- Collapse empty list slots that produce ",," / ", ,".
      t8 = Text.replace ", ," "," (Text.replace ",," "," t7)
      t9 = Text.replace "is equal to" "is" t8
  in Text.unwords (Text.words t9)

oxford :: Text -> [Text] -> Text
oxford conj = \case
  []     -> ""
  [a]    -> a
  [a, b] -> a <> " " <> conj <> " " <> b
  xs     -> Text.intercalate ", " (init xs) <> ", " <> conj <> " " <> last xs

-- ----------------------------------------------------------------------------
-- Plan construction
-- ----------------------------------------------------------------------------

buildPlan :: ExportConfig -> Module Resolved -> [Module Resolved] -> ExportPlan
buildPlan cfg mainModule deps =
  MkExportPlan
    { planMainModule = uriText mainUri
    , planModules    = mainPlanModule : importedPlanModules
    }
 where
  mainUri = moduleUri' mainModule
  units   = allUnits mainModule deps
  graph   = buildGraph mainUri units
  reach   = reachableUnits graph
  indexed = Map.toList graph.ugUnits
  isMain i = Set.member i graph.ugMain

  planUnitOf i u =
    MkPlanUnit
      { planId        = uniqueId u.uPrimary
      , planKind      = u.uKind
      , planHeading   = u.uHeading
      , planReachable = isMain i || Set.member i reach
      , planDefault   = if isMain i then Inline else resolveDisposition cfg u
      }

  mainPlanModule =
    MkPlanModule
      { planModuleUri    = uriText mainUri
      , planModuleLabel  = fst (documentTitleAndBody mainModule)
      , planModuleIsMain = True
      , planUnits        = [ planUnitOf i u | (i, u) <- indexed, isMain i ]
      }

  importedPlanModules =
    [ MkPlanModule
        { planModuleUri    = uriText uri
        , planModuleLabel  = moduleLabel uri
        , planModuleIsMain = False
        , planUnits        = us
        }
    | uri <- importedModuleOrder
    , let us = [ planUnitOf i u | (i, u) <- indexed, not (isMain i), u.uModule == uri ]
    , not (null us)
    ]
  importedModuleOrder = List.nub [ u.uModule | (i, u) <- indexed, not (isMain i) ]

-- ----------------------------------------------------------------------------
-- Title / small helpers
-- ----------------------------------------------------------------------------

-- | The document title and the section to render as its body. When the module
-- has no name of its own but wraps everything in a single leading @SECTION@,
-- that section's name becomes the title and its content is spliced up a level
-- (so a @§ Part 4@ wrapper titles the document and its @§§@ children become
-- the top-level sections). Otherwise the title is the title-cased file name.
documentTitleAndBody :: Module Resolved -> (Text, Section Resolved)
documentTitleAndBody m@(MkModule _ _ root@(MkSection _ mName _ _)) =
  case mName of
    Just n  -> (resolvedText n, root)
    Nothing -> case leadingSection root of
      Just hs -> hs
      Nothing -> (titleCase (moduleLabel (moduleUri' m)), root)

-- | Heading + body for an imported module: its leading section marker (when
-- present before any code), otherwise the file name. Mirrors
-- 'documentTitleAndBody' but with a file-name (not title-cased) fallback.
importHeadingAndBody :: NormalizedUri -> Module Resolved -> (Text, Section Resolved)
importHeadingAndBody uri (MkModule _ _ root@(MkSection _ mName _ _)) =
  case mName of
    Just n  -> (resolvedText n, root)
    Nothing -> case leadingSection root of
      Just hs -> hs
      Nothing -> (moduleLabel uri, root)

-- | Whether an imported module carries its own section title — a named module
-- root or a single leading section marker — as opposed to having nothing but a
-- file name to label it by. Drives whether it is wrapped under the generic
-- "Imported definitions" appendix.
importHasSectionTitle :: Module Resolved -> Bool
importHasSectionTitle (MkModule _ _ root@(MkSection _ mName _ _)) =
  isJust mName || isJust (leadingSection root)

-- | If a section is just a single named subsection with no loose declarations
-- before it, return that subsection's name and the subsection itself — the
-- "first section marker" used as a title/heading.
leadingSection :: Section Resolved -> Maybe (Text, Section Resolved)
leadingSection (MkSection _ _ _ decls) =
  case (any isUnitDecl decls, [ s | Section _ s <- decls ]) of
    (False, [s@(MkSection _ (Just sn) _ _)]) -> Just (resolvedText sn, s)
    _ -> Nothing
 where
  isUnitDecl = \case
    Decide{}  -> True
    Declare{} -> True
    Assume{}  -> True
    _         -> False

-- | Render an imported module as a section, preserving its own section markers.
buildImportSection
  :: (Block -> Int) -> Map.Map Unique Block -> NormalizedUri -> Module Resolved -> DocSection
buildImportSection rankOf bmap uri m =
  let (heading, MkSection _ _ _ decls) = importHeadingAndBody uri m
  in MkDocSection
    { sectionNumber  = ""
    , sectionHeading = Just heading
    , sectionGroups  = groupByKind rankOf (orderedBlocks bmap decls)
    , sectionSubs    = [ toDocSection rankOf bmap s | Section _ s <- decls ]
    }

titleCase :: Text -> Text
titleCase =
    Text.unwords
  . map cap
  . Text.words
  . Text.map (\c -> if c == '-' || c == '_' then ' ' else c)
 where
  cap w = case Text.uncons w of
    Just (c, rest) -> Text.cons (toUpper c) rest
    Nothing        -> w

resolvedText :: Resolved -> Text
resolvedText = nameToText . getActual

moduleUri' :: Module Resolved -> NormalizedUri
moduleUri' (MkModule _ uri _) = uri

uriText :: NormalizedUri -> Text
uriText u = (fromNormalizedUri u).getUri

moduleLabel :: NormalizedUri -> Text
moduleLabel u =
  let full = uriText u
      lastSeg = List.last (Text.splitOn "/" full)
  in fromMaybe lastSeg (Text.stripSuffix ".l4" lastSeg)

uniqueId :: Unique -> Text
uniqueId mu =
  Text.pack [mu.sort] <> ":" <> Text.pack (show mu.unique) <> ":" <> uriText mu.moduleUri

typeText :: Type' Resolved -> Text
typeText = \case
  TyApp _ n args ->
    let base = resolvedText n
    in if null args then base else base <> " of " <> Text.intercalate ", " (map typeText args)
  Fun{}         -> "a function"
  Forall _ _ ty -> typeText ty
  Type{}        -> "a type"
  InfVar{}      -> "an inferred type"

-- ----------------------------------------------------------------------------
-- JSON
-- ----------------------------------------------------------------------------

instance Aeson.ToJSON UnitKind where
  toJSON = \case
    RuleUnit       -> Aeson.String "rule"
    TypeUnit       -> Aeson.String "type"
    AssumptionUnit -> Aeson.String "assumption"

instance Aeson.ToJSON Disposition where
  toJSON = \case
    Inline    -> Aeson.String "inline"
    Reference -> Aeson.String "reference"
    Exclude   -> Aeson.String "exclude"

instance Aeson.ToJSON RenderedAs where
  toJSON RenderedInline = Aeson.object ["kind" .= ("inline" :: Text)]
  toJSON (RenderedReference f) = Aeson.object ["kind" .= ("reference" :: Text), "forced" .= f]

instance Aeson.ToJSON Clause where
  toJSON = \case
    CLeaf t -> Aeson.object ["$type" .= ("leaf" :: Text), "text" .= t]
    CFields fs -> Aeson.object
      ["$type" .= ("fields" :: Text), "fields" .= map pair fs]
    CAll cs -> Aeson.object ["$type" .= ("all" :: Text), "clauses" .= cs]
    CAny cs -> Aeson.object ["$type" .= ("any" :: Text), "clauses" .= cs]
    CIf chain els -> Aeson.object
      ["$type" .= ("if" :: Text), "branches" .= map cond chain, "otherwise" .= els]
    CCases subj brs -> Aeson.object
      ["$type" .= ("cases" :: Text), "subject" .= subj, "cases" .= map cond brs]
    CDeontic party modal action due provided hence lest -> Aeson.object
      [ "$type" .= ("deontic" :: Text), "party" .= party, "modal" .= modal
      , "action" .= action, "deadline" .= due, "provided" .= provided
      , "hence" .= hence, "lest" .= lest ]
    CChain lead items -> Aeson.object
      [ "$type" .= ("chain" :: Text), "lead" .= lead, "items" .= items ]
    CTable cols rows -> Aeson.object
      [ "$type" .= ("table" :: Text), "columns" .= cols, "rows" .= rows ]
    CWhere body defs -> Aeson.object
      [ "$type" .= ("where" :: Text), "body" .= body, "definitions" .= map whereDef defs ]
   where
    pair (k, v) = Aeson.object ["label" .= k, "value" .= v]
    cond (c, cl) = Aeson.object ["condition" .= c, "clause" .= cl]
    whereDef (h, c, cl) = Aeson.object ["heading" .= h, "connector" .= c, "clause" .= cl]

instance Aeson.ToJSON Block where
  toJSON b = Aeson.object
    [ "id"           .= b.blockId
    , "kind"         .= b.blockKind
    , "heading"      .= b.blockHeading
    , "connector"    .= b.blockConnector
    , "body"         .= b.blockBody
    , "renderedAs"   .= b.blockRenderedAs
    , "citation"     .= b.blockCitation
    , "sourceModule" .= b.blockSourceModule
    , "imported"     .= b.blockImported
    ]

instance Aeson.ToJSON DocGroup where
  toJSON g = Aeson.object ["label" .= g.groupLabel, "blocks" .= g.groupBlocks]

instance Aeson.ToJSON DocSection where
  toJSON s = Aeson.object
    [ "number"      .= s.sectionNumber
    , "heading"     .= s.sectionHeading
    , "groups"      .= s.sectionGroups
    , "subsections" .= s.sectionSubs
    ]

instance Aeson.ToJSON Document where
  toJSON d = Aeson.object ["title" .= d.docTitle, "sections" .= d.docSections]

instance Aeson.ToJSON PlanUnit where
  toJSON p = Aeson.object
    [ "id"                 .= p.planId
    , "kind"               .= p.planKind
    , "heading"            .= p.planHeading
    , "reachable"          .= p.planReachable
    , "defaultDisposition" .= p.planDefault
    ]

instance Aeson.ToJSON PlanModule where
  toJSON p = Aeson.object
    [ "uri"    .= p.planModuleUri
    , "label"  .= p.planModuleLabel
    , "isMain" .= p.planModuleIsMain
    , "units"  .= p.planUnits
    ]

instance Aeson.ToJSON ExportPlan where
  toJSON p = Aeson.object ["mainModule" .= p.planMainModule, "modules" .= p.planModules]
