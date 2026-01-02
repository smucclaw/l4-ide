module Hover where

import Base

import qualified Base.Text as Text
import qualified LSP.Core.Shake as Shake
import LSP.L4.Oneshot (oneshotL4ActionAndErrors)
import qualified LSP.L4.Rules as Rules
import qualified LSP.L4.Actions as Actions
import LSP.Core.PositionMapping (zeroMapping)
import Language.LSP.Protocol.Types
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import L4.EvaluateLazy (EvalConfig)

hoverTests :: EvalConfig -> [FilePath] -> FilePath -> Spec
hoverTests evalConfig files root = do
  forM_ files $ \inputFile -> do
    let testCase = makeRelative root inputFile
    let goldenDir = takeDirectory inputFile </> "tests"
    it testCase $ do
      hoverGolden evalConfig goldenDir inputFile

hoverGolden :: EvalConfig -> String -> FilePath -> IO (Golden Text)
hoverGolden evalConfig dir inputFile = do
  (errs, (nfp, mtcRes)) <- oneshotL4ActionAndErrors evalConfig inputFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    tcResult <- Shake.use Rules.TypeCheck uri
    pure (nfp, tcResult)

  let
    output = case mtcRes of
      Nothing -> "TypeCheck returned Nothing\n" <> Text.unlines errs
      Just tcRes ->
        let
          nuri = normalizedFilePathToUri nfp
          hoverPositions =
            [ (Position 4 6, "age")
            , (Position 5 6, "income")
            , (Position 6 6, "hasInsurance")
            , (Position 15 6, "name")
            , (Position 16 6, "salary")
            , (Position 21 32, "name-ref")
            ]
          hoverResults = map (getHoverAt tcRes nuri) hoverPositions
        in
          Text.unlines hoverResults

  pure
    Golden
      { output
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> takeFileName inputFile -<.> "hover.golden"
      , actualFile = Just (dir </> takeFileName inputFile -<.> "hover.actual")
      , failFirstTime = True
      }
 where
  getHoverAt tcRes nuri (pos, label) =
    let mHover = Actions.typeHover pos nuri tcRes zeroMapping
    in  label <> ": " <> formatHover mHover

  formatHover Nothing = "No hover"
  formatHover (Just (Hover (InL (MarkupContent _ content)) _)) =
    Text.replace "\n" " | " content
  formatHover (Just (Hover (InR _) _)) = "MarkedString hover (legacy)"
