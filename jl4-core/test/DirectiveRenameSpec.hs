{-# LANGUAGE OverloadedRecordDot #-}
module DirectiveRenameSpec (spec) where

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (for_)
import Data.List (sort)
import Test.Hspec

import L4.API.VirtualFS (vfsFromList, checkWithImports)
import L4.Import.Resolution (TypeCheckWithDepsResult(..), ResolvedImport(..))
import L4.TypeCheck (CheckResult(..))
import L4.TypeCheck.Types (toResolved)
import L4.FindReferences (buildReferenceMapping, lookupReference, ReferenceMapping(..))
import L4.Syntax (Resolved(..), Module(..), getActual, getUnique, nameToText)
import L4.Parser.SrcSpan (SrcRange(..), SrcPos(..))

-- Pick out uniques of every Resolved named "Foo".
fooUniques :: [Resolved] -> [String]
fooUniques rs = sort
  [ show (getUnique r)
  | r <- rs, nameToText (getActual r) == Text.pack "Foo"
  ]

spec :: Spec
spec = describe "Cross-module rename via #EVAL directive" $ do
  let domainSrc = "DECLARE Foo HAS\n    `x` IS A NUMBER\n"

  it "Foo inside a top-level use in user.l4 shares the domain.l4 Foo unique" $ do
    let userSrc = "IMPORT domain\n\n`f1` MEANS Foo WITH `x` IS 1\n"
        vfs = vfsFromList [("domain", domainSrc)]
    case checkWithImports vfs userSrc of
      Left errs -> fail (show errs)
      Right r -> do
        let userFoos = fooUniques (toResolved r.tcdModule)
            domainFoos = case r.tcdResolvedImports of
              [ri] -> fooUniques (toResolved ri.riTypeChecked.program)
              _    -> []
        -- Print to stdout for inspection
        for_ userFoos   $ \u -> putStrLn ("  user   Foo unique: " <> u)
        for_ domainFoos $ \u -> putStrLn ("  domain Foo unique: " <> u)
        -- The defining unique in domain must appear among user's uniques
        userFoos `shouldSatisfy` \us -> any (`elem` us) domainFoos

  it "Foo inside #EVAL directive in user.l4 shares the domain.l4 Foo unique" $ do
    let userSrc = "IMPORT domain\n\n#EVAL Foo WITH `x` IS 1\n"
        vfs = vfsFromList [("domain", domainSrc)]
    case checkWithImports vfs userSrc of
      Left errs -> fail (show errs)
      Right r -> do
        let userFoos = fooUniques (toResolved r.tcdModule)
            domainFoos = case r.tcdResolvedImports of
              [ri] -> fooUniques (toResolved ri.riTypeChecked.program)
              _    -> []
        for_ userFoos   $ \u -> putStrLn ("  user   Foo unique: " <> u)
        for_ domainFoos $ \u -> putStrLn ("  domain Foo unique: " <> u)
        userFoos `shouldSatisfy` \us -> any (`elem` us) domainFoos

  it "Foo at top-level AND inside #EVAL — both share the domain.l4 Foo unique" $ do
    let userSrc = "IMPORT domain\n\n`f1` MEANS Foo WITH `x` IS 1\n\n#EVAL Foo WITH `x` IS 2\n"
        vfs = vfsFromList [("domain", domainSrc)]
    case checkWithImports vfs userSrc of
      Left errs -> fail (show errs)
      Right r -> do
        let userFoos = fooUniques (toResolved r.tcdModule)
            domainFoos = case r.tcdResolvedImports of
              [ri] -> fooUniques (toResolved ri.riTypeChecked.program)
              _    -> []
        for_ userFoos   $ \u -> putStrLn ("  user   Foo unique: " <> u)
        for_ domainFoos $ \u -> putStrLn ("  domain Foo unique: " <> u)
        -- Every Foo in user.l4 should resolve to the same defining unique
        userFoos `shouldSatisfy` \us -> all (`elem` domainFoos) us

  it "Transitive chain (4 levels: domain → calcs → exports → scenario): #EVAL still resolves to domain's Foo unique" $ do
    -- Mirrors the margin-account workspace shape.
    let calcsSrc    = "IMPORT domain\n\n`q` MEANS Foo WITH `x` IS 1\n"
        exportsSrc  = "IMPORT calcs\n\n`r` MEANS `q`\n"
        scenarioSrc = "IMPORT exports\n\n`s1` MEANS Foo WITH `x` IS 2\n\n#EVAL Foo WITH `x` IS 3\n"
        vfs = vfsFromList [("domain", domainSrc), ("calcs", calcsSrc), ("exports", exportsSrc)]
    case checkWithImports vfs scenarioSrc of
      Left errs -> fail (show errs)
      Right r -> do
        let scenarioFoos = fooUniques (toResolved r.tcdModule)
        for_ scenarioFoos $ \u -> putStrLn ("  scenario Foo unique: " <> u)
        putStrLn $ "  (loaded " <> show (length r.tcdResolvedImports) <> " dep modules)"
        -- Every Foo unique seen in scenario must already exist as a defining
        -- occurrence somewhere in the dep chain — otherwise it's an
        -- out-of-scope freshly-minted unique.
        let allDepResolvedFoos =
              concatMap (\ri -> fooUniques (toResolved ri.riTypeChecked.program)) r.tcdResolvedImports
        scenarioFoos `shouldSatisfy` \us -> all (`elem` allDepResolvedFoos) us

  it "lookupReference at DECLARE Foo finds BOTH type and constructor uses across the chain" $ do
    -- Cursor sits on the type name in @DECLARE Foo HAS …@. The references
    -- handler should return:
    --  - the type-position uses (@IS A Foo@, @GIVETH A Foo@)
    --  - the constructor-position uses (@Foo WITH …@, including inside #EVAL)
    -- Both live under different uniques (type vs constructor) but both
    -- Defs share the source range of the DECLARE head, so a cursor there
    -- must surface both.
    let domainBody = "DECLARE Foo HAS\n    `x` IS A NUMBER\n"
        -- Mix of constructor-position uses (Foo WITH ...) and type-position
        -- uses (IS A Foo / GIVETH A Foo).
        calcsBody = Text.unlines
          [ "IMPORT domain"
          , ""
          , "GIVEN  accum IS A Foo"
          , "GIVETH A Foo"
          , "`bump` accum MEANS Foo WITH `x` IS 1"
          ]
        userBody = Text.unlines
          [ "IMPORT calcs"
          , ""
          , "`go` MEANS `bump` (Foo WITH `x` IS 0)"
          , ""
          , "#EVAL Foo WITH `x` IS 2"
          ]
        vfs = vfsFromList [("domain", domainBody), ("calcs", calcsBody)]
    case checkWithImports vfs userBody of
      Left errs -> fail (show errs)
      Right r -> do
        let userMod = r.tcdModule
            depMods = map (\ri -> ri.riTypeChecked.program) r.tcdResolvedImports
            unioned = buildReferenceMapping userMod <> foldMap buildReferenceMapping depMods
            -- Find the URI + position of the @Foo@ in domain.l4's DECLARE.
            -- domain.l4 begins with "DECLARE Foo HAS" — Foo starts at col 9
            -- (1-indexed) on line 1; aim mid-token.
            domainUri = case [ri.riUri | ri <- r.tcdResolvedImports, ri.riModuleName == Text.pack "domain"] of
              (u : _) -> u
              _       -> error ("no `domain` in tcdResolvedImports; have: "
                                  <> show [ri.riModuleName | ri <- r.tcdResolvedImports])
            cursor = MkSrcPos { line = 1, column = 10 }
            ranges = lookupReference domainUri cursor unioned
        for_ ranges $ \(MkSrcRange (MkSrcPos l c) _ _ u) ->
          putStrLn $ "  rng " <> show l <> ":" <> show c <> " @ " <> show u
        -- Expect: 1 Def in domain, 3 uses in calcs (IS A / GIVETH A / WITH),
        -- 2 uses in user (WITH at top level + WITH inside #EVAL). At least 5
        -- ranges, spanning all three files.
        let touchedUris = Set.fromList [r' | MkSrcRange _ _ _ r' <- ranges]
        length ranges `shouldSatisfy` (>= 5)
        Set.size touchedUris `shouldSatisfy` (>= 3)

  it "Reference mapping unions Foo's range across scenario.l4 (incl. inside #EVAL)" $ do
    -- This is what the LSP's references handler does: build the per-file
    -- mapping for every reverse-dep and union them, then look up at the
    -- cursor. If scenario.l4 contains a Foo reference whose unique matches
    -- domain.l4's defining unique, we expect to find scenario.l4's range.
    let calcsSrc    = "IMPORT domain\n\n`q` MEANS Foo WITH `x` IS 1\n"
        exportsSrc  = "IMPORT calcs\n\n`r` MEANS `q`\n"
        scenarioSrc = "IMPORT exports\n\n`s1` MEANS Foo WITH `x` IS 2\n\n#EVAL Foo WITH `x` IS 3\n"
        vfs = vfsFromList [("domain", domainSrc), ("calcs", calcsSrc), ("exports", exportsSrc)]
    case checkWithImports vfs scenarioSrc of
      Left errs -> fail (show errs)
      Right r -> do
        -- Build the per-file mappings just like GetReferences would
        let scenarioMod = r.tcdModule
            depMods    = map (\ri -> ri.riTypeChecked.program) r.tcdResolvedImports
            unioned    = buildReferenceMapping scenarioMod <> foldMap buildReferenceMapping depMods
        -- Find the defining unique by locating Foo's Def in domain.l4's resolveds
        let defFooUniques = [ getUnique r'
                            | r' <- foldMap toResolved depMods
                            , nameToText (getActual r') == Text.pack "Foo"
                            , case r' of Def {} -> True; _ -> False
                            ]
        putStrLn $ "  domain Defs of Foo: " <> show (length defFooUniques)
        let scenarioUri = case scenarioMod of MkModule _ u _ -> u
            -- For EACH defining unique, collect ranges and split by URI
            perDefining =
              [ (u, ranges, filter (\sr -> sr.moduleUri == scenarioUri) ranges)
              | u <- defFooUniques
              , let ranges = Set.toList (Map.findWithDefault Set.empty u unioned.originalToActual)
              ]
            totalScenarioRanges = sum [ length sr | (_, _, sr) <- perDefining ]
        for_ perDefining $ \(u, ranges, scenarioRanges) -> do
          putStrLn $ "  defining unique " <> show u
          putStrLn $ "    total ranges:        " <> show (length ranges)
          putStrLn $ "    of which scenario:   " <> show (length scenarioRanges)
          for_ scenarioRanges $ \(MkSrcRange (MkSrcPos l c) _ _ _) ->
            putStrLn $ "      scenario range: " <> show l <> ":" <> show c
        putStrLn $ "  total scenario ranges across all defining uniques: " <> show totalScenarioRanges
        -- We expect TWO scenario ranges: the top-level Foo and the #EVAL Foo
        totalScenarioRanges `shouldSatisfy` (>= 2)
