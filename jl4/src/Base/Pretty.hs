module Base.Pretty
  ( module X
  , toText
  , parensIf
  )
  where

import Base
import qualified Base.Text as Text

import Prettyprinter as X
import Prettyprinter.Render.Text

toText :: Doc ann -> Text.Text
toText doc =
  renderStrict (layoutPretty defaultLayoutOptions doc)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf False = id
parensIf True  = parens
