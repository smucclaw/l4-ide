{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ExportValidationSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as Text

import L4.API.VirtualFS (checkWithImports, emptyVFS)
import L4.Import.Resolution (TypeCheckWithDepsResult(..))
import L4.TypeCheck.Types (CheckErrorWithContext(..), CheckError(..))

-- | Run typecheck on a source snippet and return the list of
-- 'ExportFunctionTypeInput' errors that fired (or 'Left' on fatal failure).
exportFnErrors :: Text -> Either [Text] [CheckErrorWithContext]
exportFnErrors source =
  case checkWithImports emptyVFS source of
    Left errs -> Left errs
    Right r -> Right
      [ e
      | e@MkCheckErrorWithContext{kind = ExportFunctionTypeInput _ _} <- r.tcdErrors
      ]

spec :: Spec
spec = describe "validateExportInputs" $ do
  it "rejects @export with a function-typed GIVEN parameter" $ do
    let src = Text.unlines
          [ "@export Apply the predicate"
          , "GIVEN `p` IS A FUNCTION FROM BOOLEAN TO BOOLEAN"
          , "GIVETH A BOOLEAN"
          , "DECIDE `fn_given_test` IF `p` OF TRUE"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> length fns `shouldBe` 1

  it "rejects @export that references a function-typed ASSUME" $ do
    let src = Text.unlines
          [ "ASSUME `pred` IS A FUNCTION FROM BOOLEAN TO BOOLEAN"
          , ""
          , "@export Apply assumed predicate"
          , "GIVETH A BOOLEAN"
          , "DECIDE `fn_assume_test` IF `pred` OF TRUE"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> length fns `shouldBe` 1

  it "accepts @export with only value-typed inputs" $ do
    let src = Text.unlines
          [ "ASSUME `age` IS A NUMBER"
          , ""
          , "@export Check age"
          , "GIVETH A BOOLEAN"
          , "DECIDE `adult` IF `age` >= 18"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> fns `shouldBe` []

  it "ignores function-typed ASSUMEs that aren't referenced by any export" $ do
    let src = Text.unlines
          [ "ASSUME `unused_fn` IS A FUNCTION FROM BOOLEAN TO BOOLEAN"
          , "ASSUME `age` IS A NUMBER"
          , ""
          , "@export Check age"
          , "GIVETH A BOOLEAN"
          , "DECIDE `check_age` IF `age` >= 18"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> fns `shouldBe` []

  it "ignores function-typed GIVENs on non-@export DECIDEs" $ do
    let src = Text.unlines
          [ "GIVEN `p` IS A FUNCTION FROM BOOLEAN TO BOOLEAN"
          , "GIVETH A BOOLEAN"
          , "DECIDE `internal_helper` IF `p` OF TRUE"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> fns `shouldBe` []

  it "rejects MAYBE-wrapped function-typed GIVEN" $ do
    let src = Text.unlines
          [ "IMPORT prelude"
          , ""
          , "@export With maybe callback"
          , "GIVEN `cb` IS A MAYBE OF FUNCTION FROM BOOLEAN TO BOOLEAN"
          , "GIVETH A BOOLEAN"
          , "DECIDE `fn_maybe_test` IF TRUE"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> length fns `shouldBe` 1

  it "rejects function-typed input via type synonym" $ do
    let src = Text.unlines
          [ "DECLARE `Pred` IS A FUNCTION FROM BOOLEAN TO BOOLEAN"
          , ""
          , "@export Via synonym"
          , "GIVEN `p` IS A `Pred`"
          , "GIVETH A BOOLEAN"
          , "DECIDE `fn_syn_test` IF `p` OF TRUE"
          ]
    case exportFnErrors src of
      Left errs -> fail $ "Fatal: " ++ show errs
      Right fns -> length fns `shouldBe` 1
