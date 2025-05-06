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

semanticTokenTests :: [FilePath] -> FilePath -> Spec
semanticTokenTests files root = do
  forM_ files $ \inputFile -> do
    let
      testCase = makeRelative root inputFile
    let
      goldenDir = takeDirectory inputFile </> "tests"
    it testCase $ do
      semanticTokenGolden goldenDir inputFile

semanticTokenGolden :: String -> FilePath -> IO (Golden Text)
semanticTokenGolden dir inputFile = do
  (_errs, moutput) <- oneshotL4ActionAndErrors inputFile \nfp -> do
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
      , failFirstTime = False
      }
 where
  prettyToken :: SemanticToken -> Text
  prettyToken s =
    Text.show (s.start._line + 1)
      <> ":"
      <> Text.show (s.start._character + 1)
      <> "-"
      <> Text.show (s.start._character + 1 + s.length)
      <> " "
      <> Text.show s.category
