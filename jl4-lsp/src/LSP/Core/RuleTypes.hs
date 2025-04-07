{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FieldSelectors #-}
module LSP.Core.RuleTypes where

import           Control.DeepSeq
import           Data.Aeson.Types                             (Value)
import           Data.Hashable
import           Data.Time.Clock.POSIX

import           Development.IDE.Graph
import           GHC.Generics                                 (Generic)

import           Data.Text.Utf16.Rope.Mixed                   (Rope)
import           LSP.Logger                                   (Pretty (..), viaShow)
import           Language.LSP.Protocol.Types                  (Int32)


type instance RuleResult GetFileExists = Bool

type instance RuleResult AddWatchedFile = Bool


-- The Shake key type for getModificationTime queries
newtype GetModificationTime = GetModificationTime_
    { missingFileDiagnostics :: Bool
      -- ^ If false, missing file diagnostics are not reported
    }
    deriving (Generic)

instance Show GetModificationTime where
    show _ = "GetModificationTime"

instance Eq GetModificationTime where
    -- Since the diagnostics are not part of the answer, the query identity is
    -- independent from the 'missingFileDiagnostics' field
    _ == _ = True

instance Hashable GetModificationTime where
    -- Since the diagnostics are not part of the answer, the query identity is
    -- independent from the 'missingFileDiagnostics' field
    hashWithSalt salt _ = salt

instance NFData   GetModificationTime

pattern GetModificationTime :: GetModificationTime
pattern GetModificationTime = GetModificationTime_ {missingFileDiagnostics=True}

-- | Get the modification time of a file.
type instance RuleResult GetModificationTime = FileVersion

-- | Either the mtime from disk or an LSP version
--   LSP versions always compare as greater than on disk versions
data FileVersion
    = ModificationTime !POSIXTime -- order of constructors is relevant
    | VFSVersion !Int32
    deriving (Show, Generic, Eq, Ord)

instance NFData FileVersion

vfsVersion :: FileVersion -> Maybe Int32
vfsVersion (VFSVersion i)     = Just i
vfsVersion ModificationTime{} = Nothing

-- | Get the contents of a file, either dirty (if the buffer is modified) or Nothing to mean use from disk.
-- FIXME: should be renamed to `GetVirtualFileContents`
type instance RuleResult GetFileContents = (FileVersion, Maybe Rope)
data GetFileContents = GetFileContents
    deriving (Eq, Show, Generic)
instance Hashable GetFileContents
instance NFData   GetFileContents

data GetFileExists = GetFileExists
    deriving (Eq, Show, Generic)

instance NFData   GetFileExists
instance Hashable GetFileExists

data FileOfInterestStatus
  = OnDisk
  | Modified { firstOpen :: !Bool -- ^ was this file just opened
             }
  deriving (Eq, Show, Generic)
instance Hashable FileOfInterestStatus
instance NFData   FileOfInterestStatus

instance Pretty FileOfInterestStatus where
    pretty = viaShow

data IsFileOfInterestResult = NotFOI | IsFOI FileOfInterestStatus
  deriving (Eq, Show, Generic)
instance Hashable IsFileOfInterestResult
instance NFData   IsFileOfInterestResult

type instance RuleResult IsFileOfInterest = IsFileOfInterestResult

data IsFileOfInterest = IsFileOfInterest
    deriving (Eq, Show, Generic)
instance Hashable IsFileOfInterest
instance NFData   IsFileOfInterest

-- See Note [Client configuration in Rules]
-- | Get the client config stored in the ide state
data GetClientSettings = GetClientSettings
    deriving (Eq, Show, Generic)
instance Hashable GetClientSettings
instance NFData   GetClientSettings

type instance RuleResult GetClientSettings = Hashed (Maybe Value)

data AddWatchedFile = AddWatchedFile deriving (Eq, Show, Generic)
instance Hashable AddWatchedFile
instance NFData   AddWatchedFile



{- Note [Client configuration in Rules]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The LSP client configuration is stored by `lsp` for us, and is accesible in
handlers through the LspT monad.

This is all well and good, but what if we want to write a Rule that depends
on the configuration? For example, we might have a plugin that provides
diagnostics - if the configuration changes to turn off that plugin, then
we need to invalidate the Rule producing the diagnostics so that they go
away. More broadly, any time we define a Rule that really depends on the
configuration, such that the dependency needs to be tracked and the Rule
invalidated when the configuration changes, we have a problem.

The solution is that we have to mirror the configuration into the state
that our build system knows about. That means that:
- We have a parallel record of the state in 'IdeConfiguration'
- We install a callback so that when the config changes we can update the
'IdeConfiguration' and mark the rule as dirty.

Then we can define a Rule that gets the configuration, and build Actions
on top of that that behave properly. However, these should really only
be used if you need the dependency tracking - for normal usage in handlers
the config can simply be accessed directly from LspT.

TODO(michaelpj): this is me writing down what I think the logic is, but
it doesn't make much sense to me. In particular, we *can* get the LspT
in an Action. So I don't know why we need to store it twice. We would
still need to invalidate the Rule otherwise we won't know it's changed,
though. See https://github.com/haskell/ghcide/pull/731 for some context.
-}
