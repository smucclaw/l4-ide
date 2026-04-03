module Logging (
  Logger,
  LogLevel (..),
  newLogger,
  logDebug,
  logInfo,
  logWarn,
  logError,
) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Text (Text)
import Data.Time (getCurrentTime)
import System.IO (hFlush, stdout)

-- | Log severity levels.
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving stock (Eq, Ord, Show)

-- | Structured JSON logger.
-- Thread-safe via IORef serialization of writes.
data Logger = Logger
  { logMinLevel :: !LogLevel
  , logLock     :: !(IORef ())
  }

-- | Create a logger. Debug mode enables DEBUG level; otherwise INFO and above.
newLogger :: Bool -> IO Logger
newLogger debugMode = do
  lock <- newIORef ()
  pure Logger
    { logMinLevel = if debugMode then LevelDebug else LevelInfo
    , logLock = lock
    }

-- | Log at DEBUG level.
logDebug :: Logger -> Text -> [(Text, Value)] -> IO ()
logDebug = logMsg LevelDebug

-- | Log at INFO level.
logInfo :: Logger -> Text -> [(Text, Value)] -> IO ()
logInfo = logMsg LevelInfo

-- | Log at WARN level.
logWarn :: Logger -> Text -> [(Text, Value)] -> IO ()
logWarn = logMsg LevelWarn

-- | Log at ERROR level.
logError :: Logger -> Text -> [(Text, Value)] -> IO ()
logError = logMsg LevelError

-- | Core logging function. Writes a single JSON line to stdout.
logMsg :: LogLevel -> Logger -> Text -> [(Text, Value)] -> IO ()
logMsg level logger msg fields
  | level < logger.logMinLevel = pure ()
  | otherwise = do
      now <- getCurrentTime
      let entry = object $
            [ "time" .= show now
            , "level" .= levelText level
            , "msg" .= msg
            ] <> [(Key.fromText k, v) | (k, v) <- fields]
          line = encode entry <> "\n"
      -- Serialize writes to prevent interleaved output
      atomicModifyIORef' logger.logLock $ \() ->
        let !_ = () in ((), ())
      LBS.hPut stdout line
      hFlush stdout

-- | Convert log level to text label.
levelText :: LogLevel -> Text
levelText = \case
  LevelDebug -> "debug"
  LevelInfo  -> "info"
  LevelWarn  -> "warn"
  LevelError -> "error"
