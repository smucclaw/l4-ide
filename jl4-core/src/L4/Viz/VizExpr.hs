{-# OPTIONS_GHC -Wno-orphans #-}
-- | Visualization expression types for ladder diagram rendering.
--
-- These are the canonical type definitions used by both the WASM and LSP
-- visualization pipelines.  Serialization uses plain Aeson (no Autodocodec).
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
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.=), (.:))

-- | Versioned document identifier.
-- Serializes identically to LSP's VersionedTextDocumentIdentifier.
data VersionedDocId = MkVersionedDocId
  { uri     :: !Text
  , version :: !Int
  }
  deriving stock (Show, Generic, Eq)

instance Aeson.ToJSON VersionedDocId where
  toJSON v = Aeson.object ["uri" .= v.uri, "version" .= v.version]
instance Aeson.FromJSON VersionedDocId where
  parseJSON = Aeson.withObject "VersionedDocId" $ \o ->
    MkVersionedDocId <$> o .: "uri" <*> o .: "version"

-- | Information for rendering a function as a ladder diagram.
data RenderAsLadderInfo = MkRenderAsLadderInfo
  { verDocId :: VersionedDocId
  , funDecl  :: FunDecl
  }
  deriving stock (Show, Generic, Eq)

instance Aeson.ToJSON RenderAsLadderInfo where
  toJSON r = Aeson.object ["verDocId" .= r.verDocId, "funDecl" .= r.funDecl]
instance Aeson.FromJSON RenderAsLadderInfo where
  parseJSON = Aeson.withObject "RenderAsLadderInfo" $ \o ->
    MkRenderAsLadderInfo <$> o .: "verDocId" <*> o .: "funDecl"

type Unique = Int

-- | Name with unique identifier for equality checking.
data Name = MkName
  { unique :: Unique
  , label  :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Name where
  toJSON n = Aeson.object ["unique" .= n.unique, "label" .= n.label]
instance Aeson.FromJSON Name where
  parseJSON = Aeson.withObject "Name" $ \o ->
    MkName <$> o .: "unique" <*> o .: "label"

-- | Function declaration for visualization.
data FunDecl = MkFunDecl
  { id     :: ID
  , fnName :: Name
  , params :: [Name]
  , body   :: IRExpr
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON FunDecl where
  toJSON f = Aeson.object
    [ "$type" .= ("FunDecl" :: Text), "id" .= f.id
    , "name" .= f.fnName, "params" .= f.params, "body" .= f.body
    ]
instance Aeson.FromJSON FunDecl where
  parseJSON = Aeson.withObject "FunDecl" $ \o ->
    MkFunDecl <$> o .: "id" <*> o .: "name" <*> o .: "params" <*> o .: "body"

-- | InertContext indicates whether an Inert element is inside an AND or OR chain.
data InertContext = InertAnd | InertOr
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON InertContext where
  toJSON InertAnd = Aeson.String "InertAnd"
  toJSON InertOr  = Aeson.String "InertOr"
instance Aeson.FromJSON InertContext where
  parseJSON = Aeson.withText "InertContext" $ \case
    "InertAnd" -> pure InertAnd
    "InertOr"  -> pure InertOr
    t -> fail $ "Unknown InertContext: " <> show t

-- | Intermediate representation for boolean expressions in the visualizer.
data IRExpr
  = And ID [IRExpr]
  | Or ID [IRExpr]
  | Not ID IRExpr
  | UBoolVar ID Name UBoolValue Bool Text  -- ^ id name value canInline atomId
  | App ID Name [IRExpr] Text              -- ^ id fnName args atomId
  | TrueE ID Name
  | FalseE ID Name
  | InertE ID Text InertContext            -- ^ id text context
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON IRExpr where
  toJSON = \case
    And uid args -> Aeson.object ["$type" .= ("And" :: Text), "id" .= uid, "args" .= args]
    Or uid args -> Aeson.object ["$type" .= ("Or" :: Text), "id" .= uid, "args" .= args]
    Not uid negand -> Aeson.object ["$type" .= ("Not" :: Text), "id" .= uid, "negand" .= negand]
    UBoolVar uid name value canInline atomId -> Aeson.object
      ["$type" .= ("UBoolVar" :: Text), "id" .= uid, "name" .= name, "value" .= value, "canInline" .= canInline, "atomId" .= atomId]
    App uid fnName args atomId -> Aeson.object
      ["$type" .= ("App" :: Text), "id" .= uid, "fnName" .= fnName, "args" .= args, "atomId" .= atomId]
    TrueE uid name -> Aeson.object ["$type" .= ("TrueE" :: Text), "id" .= uid, "name" .= name]
    FalseE uid name -> Aeson.object ["$type" .= ("FalseE" :: Text), "id" .= uid, "name" .= name]
    InertE uid txt ctx -> Aeson.object ["$type" .= ("InertE" :: Text), "id" .= uid, "text" .= txt, "context" .= ctx]

instance Aeson.FromJSON IRExpr where
  parseJSON = Aeson.withObject "IRExpr" $ \o -> do
    tag <- o .: "$type" :: Aeson.Parser Text
    case tag of
      "And"      -> And <$> o .: "id" <*> o .: "args"
      "Or"       -> Or <$> o .: "id" <*> o .: "args"
      "Not"      -> Not <$> o .: "id" <*> o .: "negand"
      "UBoolVar" -> UBoolVar <$> o .: "id" <*> o .: "name" <*> o .: "value" <*> o .: "canInline" <*> o .: "atomId"
      "App"      -> App <$> o .: "id" <*> o .: "fnName" <*> o .: "args" <*> o .: "atomId"
      "TrueE"    -> TrueE <$> o .: "id" <*> o .: "name"
      "FalseE"   -> FalseE <$> o .: "id" <*> o .: "name"
      "InertE"   -> InertE <$> o .: "id" <*> o .: "text" <*> o .: "context"
      _ -> fail $ "Unknown IRExpr $type: " <> show tag

-- | Expression ID for tracking in the visualizer.
newtype ID = MkID { id :: Int }
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

instance Aeson.ToJSON ID where
  toJSON i = Aeson.object ["id" .= i.id]
instance Aeson.FromJSON ID where
  parseJSON = Aeson.withObject "ID" $ \o -> MkID <$> o .: "id"

-- | Three-valued boolean for user inputs (unknown until specified).
data UBoolValue = FalseV | TrueV | UnknownV
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON UBoolValue where
  toJSON FalseV   = Aeson.String "FalseV"
  toJSON TrueV    = Aeson.String "TrueV"
  toJSON UnknownV = Aeson.String "UnknownV"
instance Aeson.FromJSON UBoolValue where
  parseJSON = Aeson.withText "UBoolValue" $ \case
    "FalseV"   -> pure FalseV
    "TrueV"    -> pure TrueV
    "UnknownV" -> pure UnknownV
    t -> fail $ "Unknown UBoolValue: " <> show t
