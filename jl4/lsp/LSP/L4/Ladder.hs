{-# LANGUAGE DataKinds #-}

module LSP.L4.Ladder where

import           Data.Aeson
import           Data.List.NonEmpty (toList)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import qualified Data.Maybe         as Maybe
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           GHC.Generics       (Generic)
import           L4.Annotation

------- Imports for testing -------------
import           L4.Parser
import           L4.Syntax          as S
import           L4.TypeCheck       (rawNameToText)
import           LSP.L4.LadderTypes (ID (..), IRExpr,
                                     VisualizeDecisionLogicIRInfo (..))
import qualified LSP.L4.LadderTypes as L
import           Optics
import           Text.Pretty.Simple
-----------------------------------------

------------------------------------------------------
  -- Entrypoint: Visualise
------------------------------------------------------

-- | Entrypoint: Generate boolean circuits of the 'Program'.
--
-- Simple version where we visualize the first Decide, if it exists.
-- TODO: Might be nicer to have the ret type be an Either-like type
-- so we can tell the user no Decides if that's the case / give better error messages
visualise :: Program Name -> Maybe VisualizeDecisionLogicIRInfo
visualise prog =
  let decides = toListOf (gplate @(Decide Name)) prog
   in case decides of
        [x] -> MkVisualizeDecisionLogicIRInfo <$> translateDecide x
        []  -> Nothing
        _xs -> Nothing

------------------------------------------------------
  -- translateDecide, translateExpr
------------------------------------------------------

-- | Turn a single clause into a boolean circuit.
-- We support only a tiny subset of possible representations, in particular
-- only statements of the form:
--
-- @
--  GIVEN <variable>
--  DECIDE <variable> IF <boolean expression>
-- @
--
-- Moreover, only 'AND', 'OR' and variables are allowed in the '<boolean expression>'.
--
-- These limitations are arbitrary, mostly to make sure we have something to show rather
-- than being complete. So, feel free to lift these limitations at your convenience :)
--
-- Simple implementation: Translate Decide iff <= 1 Given
translateDecide :: Decide Name -> Maybe L.IRExpr
translateDecide (MkDecide _ (MkTypeSig _ givenSig _retSig) (MkAppForm _ (MkName _ fnName) _args) body) =
  case givenSig of
    MkGivenSig _ [MkOptionallyTypedName _ (MkName _ subject) _] ->
      translateExpr (rawNameToText subject) body
    MkGivenSig _ [] ->
      translateExpr (rawNameToText fnName) body
    MkGivenSig _ _xs -> Nothing -- "DECIDEs with more than one GIVEN not currently supported"

-- TODO: Temporary placeholder, to be replaced by getUnique or something
tempId :: ID
tempId = MkID 1

translateExpr :: Text -> Expr Name -> Maybe IRExpr
translateExpr subject e = case e of
  And _ left right ->
    binE L.And left right
  Or _ left right ->
    binE L.Or left right

  Equals {} -> Nothing -- Can't handle 'Is' yet
  Not {} -> Nothing -- Can't handle 'Not' yet
  Proj {} -> Nothing -- Can't handle 'Proj' yet

  -- A 'Var' can apparently be parsed as an App with no arguments ----------------
  Var _ (MkName _ verb) ->
    Just $ leaf subject (rawNameToText verb)
  App _ (MkName _ leafName) [] ->
    Just $ leaf subject (rawNameToText leafName)
  --------------------------------------------------------------------------------
  App _ (MkName _ fnName) args ->
    Just $ leaf subject $ rawNameToText fnName <> Text.unwords (getNames args)
  _ -> Nothing
  where
    getNames args = args ^.. (gplate @Name) % to nameToText
    binE op left right = L.BinExpr tempId op <$> translateExpr subject left <*> translateExpr subject right
-- error $ "[fallthru]\n" <> show x (Keeping comment around because useful for printf-style debugging)

------------------------------------------------------
  -- Leaf makers
------------------------------------------------------

defaultBoolVarValue :: L.BoolValue
defaultBoolVarValue = L.UnknownV

leaf :: Text -> Text -> IRExpr
leaf subject complement = L.BoolVar tempId (subject <> " " <> complement) defaultBoolVarValue

------------------------------------------------------
  -- Name helpers
------------------------------------------------------

nameToText :: Name -> Text
nameToText (MkName _ rawName) = rawNameToText rawName

------------------------------------------------------
  -- Dev utils
------------------------------------------------------

parseAndVisualiseProgram :: Text -> Maybe VisualizeDecisionLogicIRInfo
parseAndVisualiseProgram prog =
  case execParser program "" prog of
    Left errs -> error $ unlines $ fmap (Text.unpack . (.message)) (toList errs)
    Right x   -> visualise x

vizTest :: Text -> IO ()
vizTest = pPrint . parseAndVisualiseProgram
