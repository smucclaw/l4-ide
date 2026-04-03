module SemanticTokens where

import Base

import qualified Base.Text as Text
import qualified LSP.Core.Shake as Shake
import LSP.L4.Oneshot (oneshotL4ActionAndErrors)
import qualified LSP.L4.Rules as Rules
import LSP.SemanticTokens
import Language.LSP.Protocol.Types
import Optics
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import L4.EvaluateLazy (EvalConfig)

semanticTokenTests :: EvalConfig -> [FilePath] -> FilePath -> Spec
semanticTokenTests evalConfig files root = do
  forM_ files $ \inputFile -> do
    let
      testCase = makeRelative root inputFile
    let
      goldenDir = takeDirectory inputFile </> "tests"
    it testCase $ do
      semanticTokenGolden evalConfig goldenDir inputFile

semanticTokenGolden :: EvalConfig -> String -> FilePath -> IO (Golden Text)
semanticTokenGolden evalConfig dir inputFile = do
  (_errs, moutput) <- oneshotL4ActionAndErrors evalConfig inputFile \nfp -> do
    let
      uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    _ <- Shake.use Rules.TypeCheck uri
    lexerTokens <- Shake.use Rules.LexerSemanticTokens uri
    parserTokens <- Shake.use Rules.ParserSemanticTokens uri
    checkedTokens <- Shake.use Rules.TypeCheckedSemanticTokens uri
    pure (lexerTokens, parserTokens, checkedTokens)
  let
    output_ :: Text -> Maybe [SemanticToken] -> Text
    output_ phase mToks =
      "Phase: " <> phase <> "\n" <>
      case mToks of
        Nothing -> "No Tokens for this phase"
        Just tokens ->
          Text.unlines (fmap prettyToken tokens)
  let
    output =
      Text.unlines
        [ output_ "Lexer" (moutput ^. _1)
        , output_ "Parse" (moutput ^. _2)
        , output_ "Check" (moutput ^. _3)
        ]
  pure
    Golden
      { output
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> takeFileName inputFile -<.> "golden"
      , actualFile = Just (dir </> takeFileName inputFile -<.> "actual")
      , failFirstTime = True
      }
 where
  prettyToken :: SemanticToken -> Text
  prettyToken s =
    Text.textShow (s.start._line + 1)
      <> ":"
      <> Text.textShow (s.start._character + 1)
      <> "-"
      <> Text.textShow (s.start._character + 1 + s.length)
      <> " "
      <> Text.textShow s.category
