module L4.Print where

import L4.Syntax

import Prettyprinter

class LayoutPrinter a where
  printWithLayout :: a -> Doc ann

instance LayoutPrinter Name where
  printWithLayout (MkName _ rawName) = printWithLayout rawName

instance LayoutPrinter RawName where
  printWithLayout = \case
    NormalName t -> text t
    PreDef t -> text t
