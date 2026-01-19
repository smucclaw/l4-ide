{-# OPTIONS_GHC -Wno-orphans #-}
-- | Visualization expression types for WASM-based ladder visualization.
--
-- This module provides types that serialize to JSON compatible with the
-- ladder visualizer frontend. It mirrors the types in LSP.L4.Viz.VizExpr
-- but without depending on LSP types.
--
-- @since 0.1
module L4.Viz.VizExpr
  ( -- * Main types
    RenderAsLadderInfo(..)
  , VersionedDocId(..)
  , FunDecl(..)
  , IRExpr(..)
  , InertContext(..)
  , Name(..)
  , ID(..)
  , UBoolValue(..)
  , Unique
  ) where

import Base
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))

-- | Simplified version of LSP's VersionedTextDocumentIdentifier for WASM.
-- This serializes to the same JSON format expected by the TypeScript frontend.
data VersionedDocId = MkVersionedDocId
  { uri     :: !Text
  , version :: !Int
  }
  deriving stock (Show, Generic, Eq)

instance Aeson.ToJSON VersionedDocId where
  toJSON v = Aeson.object
    [ "uri" .= v.uri
    , "version" .= v.version
    ]

-- | Information for rendering a function as a ladder diagram.
data RenderAsLadderInfo = MkRenderAsLadderInfo
  { verDocId :: !VersionedDocId
  , funDecl  :: !FunDecl
  }
  deriving stock (Show, Generic, Eq)

instance Aeson.ToJSON RenderAsLadderInfo where
  toJSON r = Aeson.object
    [ "verDocId" .= r.verDocId
    , "funDecl"  .= r.funDecl
    ]

-- | Unique identifier for an expression.
type Unique = Int

-- | Name with unique identifier for equality checking.
data Name = MkName
  { unique :: !Unique
  , label  :: !Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Name where
  toJSON n = Aeson.object
    [ "unique" .= n.unique
    , "label"  .= n.label
    ]

-- | Function declaration for visualization.
data FunDecl = MkFunDecl
  { id     :: !ID
  , fnName :: !Name
  , params :: ![Name]
  , body   :: !IRExpr
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON FunDecl where
  toJSON f = Aeson.object
    [ "$type"  .= ("FunDecl" :: Text)
    , "id"     .= f.id
    , "name"   .= f.fnName
    , "params" .= f.params
    , "body"   .= f.body
    ]

-- | InertContext indicates whether an Inert element is inside an AND or OR chain.
-- This determines its evaluation: AND context → True (identity), OR context → False (identity).
data InertContext = InertAnd | InertOr
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON InertContext where
  toJSON InertAnd = Aeson.String "InertAnd"
  toJSON InertOr  = Aeson.String "InertOr"

-- | Intermediate representation for boolean expressions in the visualizer.
data IRExpr
  = And !ID ![IRExpr]
  | Or !ID ![IRExpr]
  | Not !ID !IRExpr
  | UBoolVar !ID !Name !UBoolValue !Bool !Text  -- ^ id name value canInline atomId
  | App !ID !Name ![IRExpr] !Text               -- ^ id fnName args atomId
  | TrueE !ID !Name
  | FalseE !ID !Name
  | InertE !ID !Text !InertContext              -- ^ id text context
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON IRExpr where
  toJSON = \case
    And uid args -> Aeson.object
      [ "$type" .= ("And" :: Text)
      , "id"    .= uid
      , "args"  .= args
      ]
    Or uid args -> Aeson.object
      [ "$type" .= ("Or" :: Text)
      , "id"    .= uid
      , "args"  .= args
      ]
    Not uid negand -> Aeson.object
      [ "$type"  .= ("Not" :: Text)
      , "id"     .= uid
      , "negand" .= negand
      ]
    UBoolVar uid name value canInline atomId -> Aeson.object
      [ "$type"    .= ("UBoolVar" :: Text)
      , "id"       .= uid
      , "name"     .= name
      , "value"    .= value
      , "canInline" .= canInline
      , "atomId"   .= atomId
      ]
    App uid fnName args atomId -> Aeson.object
      [ "$type"  .= ("App" :: Text)
      , "id"     .= uid
      , "fnName" .= fnName
      , "args"   .= args
      , "atomId" .= atomId
      ]
    TrueE uid name -> Aeson.object
      [ "$type" .= ("TrueE" :: Text)
      , "id"    .= uid
      , "name"  .= name
      ]
    FalseE uid name -> Aeson.object
      [ "$type" .= ("FalseE" :: Text)
      , "id"    .= uid
      , "name"  .= name
      ]
    InertE uid txt ctx -> Aeson.object
      [ "$type"   .= ("InertE" :: Text)
      , "id"      .= uid
      , "text"    .= txt
      , "context" .= ctx
      ]

-- | Expression ID for tracking in the visualizer.
newtype ID = MkID { id :: Int }
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

instance Aeson.ToJSON ID where
  toJSON i = Aeson.object
    [ "id" .= i.id
    ]

-- | Three-valued boolean for user inputs (unknown until specified).
data UBoolValue = FalseV | TrueV | UnknownV
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON UBoolValue where
  toJSON FalseV   = Aeson.String "FalseV"
  toJSON TrueV    = Aeson.String "TrueV"
  toJSON UnknownV = Aeson.String "UnknownV"
