{-# OPTIONS_GHC -Wno-orphans #-}
-- | Re-exports canonical VizExpr types from jl4-core and adds LSP-specific utilities.
module LSP.L4.Viz.VizExpr (
  -- * Re-exported from L4.Viz.VizExpr
  module L4.Viz.VizExpr,
  -- * LSP conversion helpers
  fromLspVerDocId,
) where

import L4.Viz.VizExpr
import qualified Language.LSP.Protocol.Types as LSP

-- | Convert an LSP VersionedTextDocumentIdentifier to a VersionedDocId.
fromLspVerDocId :: LSP.VersionedTextDocumentIdentifier -> VersionedDocId
fromLspVerDocId lsp = MkVersionedDocId
  { uri = LSP.getUri lsp._uri
  , version = fromIntegral lsp._version
  }
