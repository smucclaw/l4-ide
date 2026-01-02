module L4.TemporalGit
  ( CommitResolution (..)
  , resolveRepoRoot
  , resolveCommitHash
  , resolveRulesCommitByDate
  )
where

import Base
import qualified Base.Text as Text
import Data.Time (Day, UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Time.Format as TimeFormat
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory)
import System.Process (readProcessWithExitCode)

data CommitResolution = CommitResolution
  { commitHash :: !Text
  , commitTime :: !UTCTime
  }
  deriving stock (Eq, Show)

resolveRepoRoot :: FilePath -> IO (Either Text FilePath)
resolveRepoRoot modulePath = do
  let dir = takeDirectory modulePath
  runGit dir ["rev-parse", "--show-toplevel"] \out ->
    Right (Text.unpack (Text.strip out))

resolveCommitHash :: FilePath -> Text -> IO (Either Text CommitResolution)
resolveCommitHash repoRoot commit = do
  runGit repoRoot ["show", "-s", "--format=%H %cI", Text.unpack commit] \out -> do
    case Text.words (Text.strip out) of
      [fullHash, isoTime] -> do
        let parseOptions =
              [ ISO8601.iso8601ParseM
              , TimeFormat.parseTimeM True TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
              ]
            parsed = asum (map (\f -> f (Text.unpack isoTime)) parseOptions)
        case parsed of
          Just ts -> Right CommitResolution {commitHash = fullHash, commitTime = ts}
          Nothing -> Left ("Could not parse git commit time: " <> isoTime)
      _ ->
        Left ("Unexpected git output while resolving commit " <> Text.pack (show commit))

resolveRulesCommitByDate :: FilePath -> FilePath -> Day -> IO (Either Text CommitResolution)
resolveRulesCommitByDate repoRoot modulePath day = do
  let relPath = makeRelative repoRoot modulePath
      dayEnd = Time.UTCTime day (Time.secondsToDiffTime (24 * 60 * 60 - 1))
      cutoff = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S" dayEnd
      pathScoped =
        ["rev-list", "-1", "--first-parent", "--before", cutoff, "HEAD", "--", relPath]
      repoScoped =
        ["rev-list", "-1", "--first-parent", "--before", cutoff, "HEAD"]
  hashResult <- runGit repoRoot pathScoped (Right . Text.strip)
  chosenHash <- case hashResult of
    Left err -> pure (Left err)
    Right "" ->
      -- Fall back to repo-wide history if the file isn't present in git yet.
      runGit repoRoot repoScoped (Right . Text.strip)
    Right commitHashTxt ->
      pure (Right commitHashTxt)
  case chosenHash of
    Left err -> pure (Left err)
    Right "" -> do
      -- If there is still no match (e.g. dates before repo creation), fall back to HEAD.
      headHash <- runGit repoRoot ["rev-parse", "HEAD"] (Right . Text.strip)
      case headHash of
        Left err -> pure (Left err)
        Right commitHashTxt -> resolveCommitHash repoRoot commitHashTxt
    Right commitHashTxt -> resolveCommitHash repoRoot commitHashTxt

runGit :: FilePath -> [String] -> (Text -> Either Text a) -> IO (Either Text a)
runGit dir args k = do
  (code, out, err) <- readProcessWithExitCode "git" ("-C" : dir : args) ""
  pure case code of
    ExitSuccess ->
      k (Text.pack out)
    ExitFailure _ ->
      Left (Text.unlines ["Git command failed:", Text.pack (unwords ("git" : "-C" : dir : args)), Text.pack err])
