{-# LANGUAGE OverloadedStrings #-}
-- | Deep Maybe lifting for partial evaluation
--
-- Transforms types by wrapping them in MAYBE at every level, enabling
-- uniform handling of missing/null values in JSON input.
--
-- Example:
--   Original: DECLARE Inputs HAS x IS A BOOLEAN, y IS A NUMBER
--   Lifted:   DECLARE MaybeInputs HAS x IS A MAYBE BOOLEAN, y IS A MAYBE NUMBER
--
-- This allows JSON like {"x": true} (with y missing) to decode successfully,
-- with y becoming NOTHING.
module Backend.MaybeLift
  ( liftTypeToMaybe
  , liftTypeText
  , isPrimitiveType
  ) where

import Base
import qualified Data.Text as Text
import L4.Syntax (Type'(..), Resolved)
import L4.Print (prettyLayout)

-- | Check if a type name is a primitive that should be lifted
-- Note: DATE is not included here because it needs special handling
-- (converted to STRING for JSON, then parsed with TODATE)
isPrimitiveType :: Text -> Bool
isPrimitiveType name = name `elem` ["BOOLEAN", "NUMBER", "STRING"]

-- | Check if a type name is DATE
isDateType :: Text -> Bool
isDateType name = Text.toUpper (Text.strip name) == "DATE"

-- | Check if a type is already a MAYBE type
isAlreadyMaybe :: Text -> Bool
isAlreadyMaybe tyText = "MAYBE " `Text.isPrefixOf` Text.toUpper (Text.strip tyText)

-- | Extract the inner type from a MAYBE type
-- e.g., "MAYBE BOOLEAN" -> "BOOLEAN"
extractMaybeInner :: Text -> Text
extractMaybeInner tyText =
  let stripped = Text.strip tyText
      upper = Text.toUpper stripped
  in if "MAYBE " `Text.isPrefixOf` upper
     then Text.strip $ Text.drop 6 stripped  -- drop "MAYBE "
     else stripped

-- | Lift a type to MAYBE, handling primitives and complex types
--
-- For primitives (BOOLEAN, NUMBER, STRING, DATE):
--   lift BOOLEAN = MAYBE BOOLEAN
--
-- For records (represented as type applications with field types):
--   Each field type is recursively lifted
--
-- For lists:
--   LIST OF a becomes MAYBE (LIST OF (lift a))
--
-- For already-MAYBE types:
--   Don't double-wrap, but DO recurse into the inner type if complex
--   MAYBE BOOLEAN stays as MAYBE BOOLEAN (primitive, already done)
--   MAYBE (LIST OF BOOLEAN) becomes MAYBE (LIST OF (MAYBE BOOLEAN)) (recurse)
liftTypeText :: Text -> Text
liftTypeText tyText
  -- Already wrapped in MAYBE
  | isAlreadyMaybe tyText =
      let inner = extractMaybeInner tyText
          innerUpper = Text.toUpper $ Text.strip inner
      in if isPrimitiveType innerUpper
         then tyText  -- MAYBE primitive - already fully lifted
         else if isDateType inner
              -- MAYBE DATE -> MAYBE STRING (JSON doesn't have date type)
              -- CodeGen handles the string→date conversion with TODATE
              then "MAYBE STRING"
         else if "LIST OF " `Text.isPrefixOf` innerUpper
              -- MAYBE (LIST OF x) - recurse into list element
              then let elemType = Text.strip $ Text.drop 8 inner
                   in "MAYBE (LIST OF (" <> liftTypeText elemType <> "))"
              -- MAYBE complex - keep as is (record fields handled by JSON decoder)
              else tyText
  -- DATE type - convert to STRING for JSON compatibility
  -- The CodeGen module will add TODATE conversion when unwrapping
  | isDateType tyText =
      "MAYBE STRING"
  -- Primitive types - wrap in MAYBE
  | isPrimitiveType (Text.toUpper $ Text.strip tyText) =
      "MAYBE " <> tyText
  -- LIST OF - wrap list and lift element type
  | "LIST OF " `Text.isPrefixOf` Text.toUpper tyText =
      let elemType = Text.strip $ Text.drop 8 tyText
      in "MAYBE (LIST OF (" <> liftTypeText elemType <> "))"
  -- Other types (records, enums, custom types) - just wrap in MAYBE
  -- The JSON decoder will handle field-level nulls
  | otherwise =
      "MAYBE " <> tyText

-- | Lift a resolved type to MAYBE
-- This version works with the AST representation
liftTypeToMaybe :: Type' Resolved -> Text
liftTypeToMaybe ty = liftTypeText (prettyLayout ty)
