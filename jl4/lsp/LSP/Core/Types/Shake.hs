{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FieldSelectors #-}
module LSP.Core.Types.Shake
  ( Q (..),
    A (..),
    Value (..),
    ValueWithDiagnostics (..),
    Values,
    Key,
    BadDependency (..),
    ShakeValue(..),
    currentValue,
    isBadDependency,
  toShakeValue,encodeShakeValue,decodeShakeValue,toKey,toNoFileKey,fromKey,fromKeyType)
where

import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString.Char8                as BS
import           Data.Dynamic
import           Data.Hashable
import           Data.Typeable                        (cast, type (:~~:) (HRefl))
import           Data.Vector                          (Vector)
import           LSP.Core.PositionMapping
import           LSP.Core.RuleTypes       (FileVersion)
import           Development.IDE.Graph                (Key, RuleResult, newKey,
                                                       pattern Key)
import qualified Development.IDE.Graph                as Graph
import           LSP.Core.Types.Diagnostics
import           GHC.Generics
import qualified StmContainers.Map                    as STM
import           Type.Reflection                      (SomeTypeRep (SomeTypeRep),
                                                       pattern App, pattern Con,
                                                       typeOf, typeRep, eqTypeRep)
import           Language.LSP.Protocol.Types
import qualified Base.Text as Text

data Value v
    = Succeeded (Maybe FileVersion) v
    | Stale (Maybe PositionDelta) (Maybe FileVersion) v
    | Failed Bool -- True if we already tried the persistent rule
    deriving (Functor, Generic, Show)

instance NFData v => NFData (Value v)

-- | Convert a Value to a Maybe. This will only return `Just` for
-- up2date results not for stale values.
currentValue :: Value v -> Maybe v
currentValue (Succeeded _ v) = Just v
currentValue Stale  {}       = Nothing
currentValue Failed {}       = Nothing

data ValueWithDiagnostics
  = ValueWithDiagnostics !(Value Dynamic) !(Vector FileDiagnostic)

-- | The state of the all values and diagnostics
type Values = STM.Map Key ValueWithDiagnostics

-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
newtype BadDependency = BadDependency String deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False

toKey :: Graph.ShakeValue k => k -> NormalizedUri -> Key
toKey = (newKey.) . curry Q

fromKey :: Typeable k => Key -> Maybe (k, NormalizedUri)
fromKey (Key k)
  | Just (Q (k', f)) <- cast k = Just (k', f)
  | otherwise = Nothing

-- | fromKeyType (Q (k,f)) = (typeOf k, f)
fromKeyType :: Key -> Maybe (SomeTypeRep, NormalizedUri)
fromKeyType (Key k) = case typeOf k of
    App con@(Con _) a
        | Just HRefl <- con `eqTypeRep` typeRep @Q
        -> case k of  Q (_, f) -> Just (SomeTypeRep a, f)
    _ -> Nothing

toNoFileKey :: (Show k, Typeable k, Eq k, Hashable k) => k -> Key
toNoFileKey k = newKey $ Q (k, normalizedFilePathToUri emptyNormalizedFilePath)

newtype Q k = Q (k, NormalizedUri)
    deriving newtype (Eq, Hashable, NFData)

instance Show k => Show (Q k) where
    show (Q (k, file)) = show k ++ "; " ++ Text.unpack (fromNormalizedUri file).getUri

-- | Invariant: the 'v' must be in normal form (fully evaluated).
--   Otherwise we keep repeatedly 'rnf'ing values taken from the Shake database
newtype A v = A (Value v)
    deriving Show

instance NFData (A v) where rnf (A v) = v `seq` ()

-- In the Shake database we only store one type of key/result pairs,
-- namely Q (question) / A (answer).
type instance RuleResult (Q k) = A (RuleResult k)


toShakeValue :: (BS.ByteString -> ShakeValue) -> Maybe BS.ByteString -> ShakeValue
toShakeValue = maybe ShakeNoCutoff

data ShakeValue
  = -- | This is what we use when we get Nothing from
    -- a rule.
    ShakeNoCutoff
  | -- | This is used both for `Failed`
    -- as well as `Succeeded`.
    ShakeResult !BS.ByteString
  | ShakeStale !BS.ByteString
  deriving (Generic, Show)

instance NFData ShakeValue

encodeShakeValue :: ShakeValue -> BS.ByteString
encodeShakeValue = \case
  ShakeNoCutoff -> BS.empty
  ShakeResult r -> BS.cons 'r' r
  ShakeStale r  -> BS.cons 's' r

decodeShakeValue :: BS.ByteString -> ShakeValue
decodeShakeValue bs = case BS.uncons bs of
  Nothing -> ShakeNoCutoff
  Just (x, xs)
    | x == 'r' -> ShakeResult xs
    | x == 's' -> ShakeStale xs
    | otherwise -> error $ "Failed to parse shake value " <> show bs
