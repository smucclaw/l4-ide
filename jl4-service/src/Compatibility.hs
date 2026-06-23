{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Backwards-compatibility guard for deployment updates.
--
-- When an existing deployment is re-deployed (PUT), the new bundle's
-- exported-function interface must stay compatible with the currently
-- deployed one so existing integrations keep working. This module
-- recursively diffs the two interfaces and reports every
-- backwards-incompatible ("breaking") change.
--
-- Compatibility is directional:
--
--   * Inputs (request parameters): adding a new /optional/ parameter is
--     safe; removing one, renaming it, changing its type/format, making
--     it required, or narrowing its accepted enum values is breaking.
--
--   * Outputs (return value): the mirror image — removing/renaming a
--     field, dropping it from "always present", changing its type, or
--     widening its enum (new values a consumer may not handle) is
--     breaking; adding a new field is safe.
--
-- New functions are safe; removed functions are breaking. This mirrors
-- the client-side check in the deploy sidebar so both surfaces agree.
module Compatibility
  ( FnIface (..)
  , ifaceFromFunction
  , ifaceFromSummary
  , detectBreakingChanges
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import L4.FunctionSchema (Parameter (..), Parameters (..))
import Types (Function (..), FunctionSummary (..))

-- | The compatibility-relevant slice of a deployed/incoming function.
data FnIface = FnIface
  { ifName         :: !Text
  , ifParams       :: !Parameters
  , ifReturnType   :: !Text
  -- ^ Display name (e.g. "BOOLEAN", "DEONTIC"); a change here also
  -- covers deontic ⇄ non-deontic (different request envelope).
  , ifReturnSchema :: !(Maybe Parameter)
  -- ^ Structured return schema. Present from the live in-memory
  -- 'Function'; 'Nothing' for cached metadata 'FunctionSummary'.
  }

ifaceFromFunction :: Function -> FnIface
ifaceFromFunction f =
  FnIface f.name f.parameters f.returnType f.returnSchema

ifaceFromSummary :: FunctionSummary -> FnIface
ifaceFromSummary s =
  FnIface s.fsName s.fsParameters s.fsReturnType s.fsReturnSchema

data Dir = In | Out deriving stock (Eq)

kindOf :: Dir -> Text
kindOf In  = "parameter"
kindOf Out = "return value"

-- | @subject fn dir loc@ — the human-readable location prefix, e.g.
-- @"myFn parameter address.zip"@ or @"myFn return value"@.
subject :: Text -> Dir -> Text -> Text
subject fn dir loc =
  fn <> " " <> kindOf dir <> if Text.null loc then "" else " " <> loc

childLoc :: Text -> Text -> Text
childLoc loc k = if Text.null loc then k else loc <> "." <> k

-- | Diff every old function against the new set. Returns one message
-- per backwards-incompatible change (empty list ⇒ compatible).
detectBreakingChanges
  :: [FnIface]  -- ^ currently deployed
  -> [FnIface]  -- ^ incoming
  -> [Text]
detectBreakingChanges old new =
  let newByName = Map.fromList [(f.ifName, f) | f <- new]
      changed = concat
        [ maybe [] (diffFn o) (Map.lookup o.ifName newByName) | o <- old ]
      removed =
        [ "rule " <> o.ifName <> " removed"
        | o <- old, not (o.ifName `Map.member` newByName) ]
  in changed ++ removed
  -- New functions (present in @new@, absent from @old@) are not breaking.

diffFn :: FnIface -> FnIface -> [Text]
diffFn old new =
  let fn = old.ifName
      returnTypeChanged =
        not (Text.null old.ifReturnType)
          && not (Text.null new.ifReturnType)
          && old.ifReturnType /= new.ifReturnType
      retMsg
        | returnTypeChanged =
            [ fn <> " return type changed from "
                <> old.ifReturnType <> " to " <> new.ifReturnType ]
        | otherwise = []
      paramMsgs =
        diffObject In fn ""
          (old.ifParams.parameterMap) (old.ifParams.required)
          (new.ifParams.parameterMap) (new.ifParams.required)
      -- Return-value diff only when the return type itself is unchanged
      -- and both sides expose a structured schema.
      retSchemaMsgs =
        case (returnTypeChanged, old.ifReturnSchema, new.ifReturnSchema) of
          (False, Just os, Just ns) -> diffNode Out fn "" os ns
          _                         -> []
  in retMsg ++ paramMsgs ++ retSchemaMsgs

-- | Diff a single schema node (old vs new). Recurses into object
-- properties and array items so nested record/list shapes are compared
-- at every depth.
diffNode :: Dir -> Text -> Text -> Parameter -> Parameter -> [Text]
diffNode dir fn loc old new
  -- A type change makes deeper diffing noise; report and stop here.
  | not (Text.null old.parameterType)
      && not (Text.null new.parameterType)
      && old.parameterType /= new.parameterType =
      [ subject fn dir loc <> " type changed from "
          <> old.parameterType <> " to " <> new.parameterType ]
  | otherwise =
      formatMsgs ++ enumMsgs ++ objectMsgs ++ itemsMsgs
 where
  oFmt = fromMaybe "" old.parameterFormat
  nFmt = fromMaybe "" new.parameterFormat
  formatMsgs
    | oFmt /= nFmt && not (Text.null oFmt && Text.null nFmt) =
        [ subject fn dir loc <> " format changed from "
            <> nonEmpty oFmt <> " to " <> nonEmpty nFmt ]
    | otherwise = []
  nonEmpty t = if Text.null t then "none" else t

  oEnum = old.parameterEnum
  nEnum = new.parameterEnum
  enumMsgs
    | not (null oEnum) && not (null nEnum) =
        case dir of
          In ->
            let dropped = filter (`notElem` nEnum) oEnum
            in [ subject fn dir loc <> " no longer accepts "
                   <> Text.intercalate ", " dropped
               | not (null dropped) ]
          Out ->
            let added = filter (`notElem` oEnum) nEnum
            in [ subject fn dir loc <> " may now return new values "
                   <> Text.intercalate ", " added
               | not (null added) ]
    | dir == In && null oEnum && not (null nEnum) =
        [ subject fn dir loc <> " is now restricted to a fixed set of values" ]
    | dir == Out && not (null oEnum) && null nEnum =
        [ subject fn dir loc <> " is no longer limited to a fixed set of values" ]
    | otherwise = []

  objectMsgs =
    case (old.parameterProperties, new.parameterProperties) of
      (Nothing, Nothing) -> []
      (mo, mn) ->
        diffObject dir fn loc
          (fromMaybe Map.empty mo) (fromMaybe [] old.parameterRequired)
          (fromMaybe Map.empty mn) (fromMaybe [] new.parameterRequired)

  itemsMsgs =
    case (old.parameterItems, new.parameterItems) of
      (Just oi, Just ni) ->
        diffNode dir fn (if Text.null loc then "[]" else loc <> "[]") oi ni
      _ -> []

-- | Diff an object's property set (used for both the top-level parameter
-- object and every nested record), honouring required-ness in the
-- compatibility direction.
diffObject
  :: Dir -> Text -> Text
  -> Map.Map Text Parameter -> [Text]  -- ^ old properties, old required
  -> Map.Map Text Parameter -> [Text]  -- ^ new properties, new required
  -> [Text]
diffObject dir fn loc oldProps oldReq newProps newReq =
  concatMap checkOld (Map.toList oldProps) ++ concatMap checkNew (Map.keys newProps)
 where
  checkOld (k, ov) =
    let cl = childLoc loc k
    in case Map.lookup k newProps of
      Nothing ->
        [ subject fn dir cl
            <> (if dir == In then " removed" else " no longer returned") ]
      Just nv ->
        let reqMsg = case dir of
              In | k `elem` newReq && k `notElem` oldReq ->
                   [ subject fn dir cl <> " is now required" ]
              Out | k `elem` oldReq && k `notElem` newReq ->
                   [ subject fn dir cl <> " may now be absent from the result" ]
              _ -> []
        in reqMsg ++ diffNode dir fn cl ov nv

  checkNew k
    -- New optional input / new output field → compatible. Only a new
    -- *required* input breaks existing callers.
    | k `Map.member` oldProps = []
    | dir == In && k `elem` newReq =
        [ fn <> " has a new required parameter " <> childLoc loc k ]
    | otherwise = []
