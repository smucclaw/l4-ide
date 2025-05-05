{-# LANGUAGE ViewPatterns #-}

module LSP.L4.Viz.Ladder where

import Control.DeepSeq
import Control.Monad.Except
import Base
import qualified Base.Text as Text
import Optics.State.Operators ((<%=))

import qualified Language.LSP.Protocol.Types as LSP

import qualified L4.TypeCheck as TC
import L4.Annotation
import L4.Syntax
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    RenderAsLadderInfo (..),
    VersionedDocId (..)
  )
import qualified LSP.L4.Viz.VizExpr as V
import L4.Print (prettyLayout)
import qualified L4.Transform as Transform (simplify)

{- | Temporary hack to disable the translation to IRExpr App
while the frontend doesn't have proper UI for it -}
isDevMode :: Bool
isDevMode = False

------------------------------------------------------
-- Monad
------------------------------------------------------

newtype Viz a = MkViz {getVizE :: VizState -> (Either VizError a, VizState)}
  deriving
    (Functor, Applicative, Monad, MonadState VizState, MonadError VizError)
    via ExceptT VizError (State VizState)

mkVizEnv :: LSP.VersionedTextDocumentIdentifier -> TC.Substitution -> Bool -> VizEnv
mkVizEnv lspVerTxtDocId substitution shouldSimplify =
  let moduleUri = toNormalizedUri lspVerTxtDocId._uri
  in MkVizEnv
    { moduleUri
    , verTxtDocId = MkVersionedDocId lspVerTxtDocId
    , substitution
    , shouldSimplify
    }

data VizEnv = MkVizEnv
  { moduleUri      :: !NormalizedUri
  , verTxtDocId    :: !VersionedDocId
  , substitution   :: !TC.Substitution  -- might be more futureproof to use TypeCheckResult
  , shouldSimplify :: !Bool             -- ^ whether to simplify the expression
  }
  deriving stock (Show, Generic, Eq)

data VizState =
  MkVizState
    { env :: !VizEnv
    , maxId :: !ID
    }
  deriving stock (Show, Generic, Eq)

mkInitialVizState :: VizEnv -> VizState
mkInitialVizState env =
  MkVizState
    { env = env
    , maxId = MkID 0
    }

-- Monad ops

-- | 'Internal' helper: This should only be used by other Viz monad ops
getVizEnv :: Viz VizEnv
getVizEnv = use #env

getVerTxtDocId :: Viz VersionedDocId
getVerTxtDocId = do
  env <- getVizEnv
  pure env.verTxtDocId

getExpandedType :: Type' Resolved -> Viz (Type' Resolved)
getExpandedType ty = do
  env <- getVizEnv
  pure $ TC.applyFinalSubstitution env.substitution env.moduleUri ty

getFresh :: Viz ID
getFresh = do
  #maxId <%= \(MkID n) -> MkID (n + 1)

getShouldSimplify :: Viz Bool
getShouldSimplify = do
  env <- getVizEnv
  pure env.shouldSimplify

------------------------------------------------------
-- VizError
------------------------------------------------------

data VizError
  = InvalidProgramNoDecidesFound
  | InvalidProgramDecidesMustNotHaveMoreThanOneGiven
  | Unimplemented
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- TODO: Incorporate context like the specific erroring rule and the src range too
prettyPrintVizError :: VizError -> Text
prettyPrintVizError = \case
  InvalidProgramNoDecidesFound ->
    "The program isn't the right sort for visualization: there are no DECIDE rules that can be visualized."
  InvalidProgramDecidesMustNotHaveMoreThanOneGiven ->
    "Visualization failed: DECIDE rules must reference no more than one GIVEN variable."
  Unimplemented -> "Unimplemented"

------------------------------------------------------
-- Entrypoint: Visualise
------------------------------------------------------

-- | Entrypoint: Generate boolean circuits of the given 'Decide'.
doVisualize :: Decide Resolved -> VizEnv -> Either VizError RenderAsLadderInfo
doVisualize decide env =
  case  (vizProgram decide).getVizE (mkInitialVizState env) of
    (result, _) -> result

vizProgram :: Decide Resolved -> Viz RenderAsLadderInfo
vizProgram decide = MkRenderAsLadderInfo <$> getVerTxtDocId <*> translateDecide decide

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
translateDecide :: Decide Resolved -> Viz V.FunDecl
translateDecide (MkDecide _ (MkTypeSig _ givenSig _) (MkAppForm _ funResolved _ _) body) =
  do
    shouldSimplify <- getShouldSimplify
    uid            <- getFresh
    vizBody        <- translateExpr shouldSimplify body
    pure $ V.MkFunDecl
      uid
      -- didn't want a backtick'd name in the header
      (mkSimpleVizName funResolved)
      (paramNamesFromGivens givenSig)
      vizBody
      where
        paramNamesFromGivens :: GivenSig Resolved -> [V.Name]
        paramNamesFromGivens (MkGivenSig _ optionallyTypedNames) =
          mkSimpleVizName . getResolved <$> optionallyTypedNames

        -- TODO: I imagine there will be functionality for this kind of thing in a more central place soon;
        -- this can be replaced with that when that happens.
        getResolved :: OptionallyTypedName Resolved -> Resolved
        getResolved (MkOptionallyTypedName _ paramName _) = paramName

        mkSimpleVizName = mkVizNameWith prettyLayout

translateExpr :: Bool -> Expr Resolved -> Viz IRExpr
translateExpr True  =
  translateExpr False . Transform.simplify
translateExpr False = go
  where
    go e =
      case e of
        Not _ negand -> do
          uid <- getFresh
          V.Not uid <$> go negand
        And {} -> do
          uid <- getFresh
          V.And uid <$> traverse go (scanAnd e)
        Or {} -> do
          uid <- getFresh
          V.Or uid <$> traverse go (scanOr e)
        Where _ e' _ds -> go e' -- TODO: lossy

        -- TODO: Should handle BoolLits differently too
        -- 'var'
        App _ resolved [] ->
          leafFromVizName (mkVizNameWith prettyLayout resolved)

        App appAnno fnResolved args -> do
          fnOfAppIsFnFromBooleansToBoolean <- and <$> traverse hasBooleanType (appAnno : map getAnno args)
          -- for now, only translating App of boolean functions to V.App
          if isDevMode && fnOfAppIsFnFromBooleansToBoolean
            then
              V.App
                <$> getFresh
                <*> pure (mkVizNameWith nameToText fnResolved)
                <*> traverse go args
            else
              leaf "" $ Text.unwords $ (nameToText . getOriginal $ fnResolved) : (prettyLayout <$> args)

        _ -> do
          leaf "" (prettyLayout e)

scanAnd :: Expr Resolved -> [Expr Resolved]
scanAnd (And _ e1 e2) =
  scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr Resolved -> [Expr Resolved]
scanOr (Or _ e1 e2) =
  scanOr e1 <> scanOr e2
scanOr e = [e]

------------------------------------------------------
-- Leaf makers
------------------------------------------------------

defaultUBoolVarValue :: V.UBoolValue
defaultUBoolVarValue = V.UnknownV

leafFromVizName :: V.Name -> Viz IRExpr
leafFromVizName vname = do
  uid <- getFresh
  pure $ V.UBoolVar uid vname defaultUBoolVarValue

leaf :: Text -> Text -> Viz IRExpr
leaf subject complement = do
  uid <- getFresh
  tempUniqueTODO <- getFresh
  -- tempUniqueTODO: I'd like to defer properly handling the V.Name for `leaf` and the kinds of cases it's used for.
  -- I'll return to this when we explicitly/properly handle more cases in translateExpr
  -- (I'm currently focusing on state in the frontend in the simpler case of App with no args)
  pure $ V.UBoolVar uid (V.MkName tempUniqueTODO.id $ subject <> " " <> complement) defaultUBoolVarValue

------------------------------------------------------
-- Name helpers
------------------------------------------------------

-- It's not obvious to me that we want to be using
-- getOriginal (as getUniqueName does), instead of getActual,
-- but I'm going with this for now since it's what was used to translate the function name.
mkVizNameWith :: (Name -> Text) -> Resolved -> V.Name
mkVizNameWith printer (getUniqueName -> (uniq, name)) =
  case uniq of
    MkUnique {unique} -> V.MkName unique (printer name)

------------------------------------------------------
-- Helpers for checking if an Expr has Boolean type
------------------------------------------------------

hasBooleanType :: Anno_ t Extension -> Viz Bool
hasBooleanType (Anno {extra = Extension {resolvedInfo = Just (TypeInfo ty _)}}) =
  isBooleanType ty
hasBooleanType _ = pure False

-- | Returns True iff the (expanded) Type Resolved is that of a L4 BOOLEAN
isBooleanType :: Type' Resolved -> Viz Bool
isBooleanType ty = do
  type' <- getExpandedType ty
  pure $ case type' of
    TyApp _ (Ref _ uniq _) [] ->
      uniq == TC.booleanUnique
    _ -> False
