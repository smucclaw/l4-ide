{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE DataKinds #-}

-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Options
module LSP.Core.Types.Options
  ( IdeOptions(..)
  , IdeReportProgress(..)
  , IdeDefer(..)
  , IdeTesting(..)
  , IdeOTMemoryProfiling(..)
  , clientSupportsProgress
  , defaultIdeOptions
  , IdeResult
  , OptHaddockParse(..)
  , ProgressReportingStyle(..)
  , PreservedKeys(..)
  , LspSink(..)
  , WithProgressFunc
  , WithIndefiniteProgressFunc
  ) where

import Control.Lens
import Data.Aeson
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Data.Typeable
import Development.IDE.Graph
import GHC.Generics (Generic)
import LSP.Core.Types.Diagnostics
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types (ClientCapabilities)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS
import LSP.Core.RuleTypes

data IdeOptions = IdeOptions
  {
    optExtensions         :: [String]
    -- ^ File extensions to search for code
  , optShakeProfiling     :: Maybe FilePath
    -- ^ Set to 'Just' to create a directory of profiling reports.
  , optTesting            :: IdeTesting
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
  , optReportProgress     :: IdeReportProgress
    -- ^ Whether to report progress during long operations.
  , optProgressStyle      :: ProgressReportingStyle
  , optMaxDirtyAge        :: Int
    -- ^ Age (in # builds) at which we collect dirty keys
  , optLanguageSyntax     :: String
    -- ^ the ```language to use
  , optNewColonConvention :: Bool
    -- ^ whether to use new colon convention
  , optKeywords           :: [T.Text]
    -- ^ keywords used for completions. These are customizable
    -- since DAML has a different set of keywords than Haskell.
  , optCheckProject       :: IO Bool
    -- ^ Whether to typecheck the entire project on load
  , optPreservedKeys       :: CheckParents -> PreservedKeys
  , optCheckParents       :: IO CheckParents
    -- ^ When to typecheck reverse dependencies of a file
  , optShakeOptions       :: ShakeOptions
  , optRunSubset          :: Bool
      -- ^ Experimental feature to re-run only the subset of the Shake graph that has changed
  , optSkipProgress       :: forall a. Typeable a => a -> Bool
      -- ^ Predicate to select which rule keys to exclude from progress reporting.
  }

data CheckParents
    -- Note that ordering of constructors is meaningful and must be monotonically
    -- increasing in the scenarios where parents are checked
    = NeverCheck
    | CheckOnSave
    | AlwaysCheck
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype PreservedKeys = PreservedKeys { keys :: HashSet TypeRep }


data OptHaddockParse = HaddockParse | NoHaddockParse
  deriving (Eq,Ord,Show,Enum)

newtype IdeReportProgress    = IdeReportProgress Bool
newtype IdeDefer             = IdeDefer          Bool
newtype IdeTesting           = IdeTesting        Bool
newtype IdeOTMemoryProfiling = IdeOTMemoryProfiling    Bool

data ProgressReportingStyle
    = Percentage -- ^ Report using the LSP @_percentage@ field
    | Explicit   -- ^ Report using explicit 123/456 text
    | NoProgress -- ^ Do not report any percentage
    deriving Eq

clientSupportsProgress :: LSP.ClientCapabilities -> IdeReportProgress
clientSupportsProgress caps = IdeReportProgress $ Just True ==
    ((\x -> x ^. L.workDoneProgress) =<< LSP._window (caps :: LSP.ClientCapabilities))

defaultIdeOptions :: IdeOptions
defaultIdeOptions = IdeOptions
    {optExtensions = ["hs", "lhs"]
    ,optShakeOptions = shakeOptions
    ,optShakeProfiling = Nothing
    ,optReportProgress = IdeReportProgress False
    ,optProgressStyle = Explicit
    ,optLanguageSyntax = "haskell"
    ,optNewColonConvention = False
    ,optKeywords = []
    ,optTesting = IdeTesting False
    ,optCheckProject = pure True
    ,optCheckParents = pure CheckOnSave
    ,optSkipProgress = defaultSkipProgress
    ,optMaxDirtyAge = 100
    ,optRunSubset = True
    ,optPreservedKeys = \_ -> PreservedKeys HashSet.empty
    }

defaultSkipProgress :: Typeable a => a -> Bool
defaultSkipProgress key
    -- don't do progress for GetFileContents as it's cheap
    | Just GetFileContents <- cast key        = True
    -- don't do progress for GetFileExists, as there are lots of redundant nodes
    -- (normally there is one node per file, but this is not the case for GetFileExists)
    | Just GetFileExists <- cast key          = True
    -- don't do progress for GetModificationTime as there are lot of redundant nodes
    -- (for the interface files)
    | Just GetModificationTime_{} <- cast key = True
    | otherwise                               = False

data LspSink = LspSink
    { sendNotificationToClient :: forall (m :: Method 'ServerToClient 'Notification) . SServerMethod m -> MessageParams m -> IO ()
    , sendRequestToClient ::
      forall (m :: Method 'ServerToClient 'Request). SServerMethod m -> MessageParams m
      -> (Either (TResponseError m) (MessageResult m) -> IO ()) -> IO (LspId m)
    , withClientProgress :: WithProgressFunc
    , withIndefiniteClientProgress :: WithIndefiniteProgressFunc
    , takeVfsSnapshot :: IO VFS
    , currentClientCapabilities :: IO ClientCapabilities
    }

type WithProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> ((LSP.ProgressAmount -> IO ()) -> IO a) -> IO a
type WithIndefiniteProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> IO a -> IO a
