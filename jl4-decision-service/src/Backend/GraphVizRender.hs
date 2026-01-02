module Backend.GraphVizRender
  ( renderPNG
  , renderSVG
  , isGraphVizAvailable
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Maybe (isJust)
import Control.Exception (evaluate)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process
  ( CreateProcess (std_err, std_in, std_out)
  , StdStream (CreatePipe)
  , proc
  , waitForProcess
  , withCreateProcess
  )

renderPNG :: Text.Text -> IO (Either Text.Text BS.ByteString)
renderPNG = runDotBinary ["-Tpng"]

renderSVG :: Text.Text -> IO (Either Text.Text Text.Text)
renderSVG = runDotText ["-Tsvg"]

isGraphVizAvailable :: IO Bool
isGraphVizAvailable = isJust <$> findExecutable "dot"

runDotBinary :: [String] -> Text.Text -> IO (Either Text.Text BS.ByteString)
runDotBinary args dotSource = do
  withCreateProcess processSpec $ \mIn mOut mErr ph -> case (mIn, mOut, mErr) of
    (Just hIn, Just hOut, Just hErr) -> do
      BL.hPut hIn (BL.fromStrict $ Text.encodeUtf8 dotSource)
      hClose hIn
      stdoutLazy <- BL.hGetContents hOut
      stderrLazy <- BL.hGetContents hErr
      -- Force the lazy ByteStrings before the handles are closed
      _ <- evaluate (BL.length stdoutLazy)
      _ <- evaluate (BL.length stderrLazy)
      exitCode <- waitForProcess ph
      let stdoutStrict = BL.toStrict stdoutLazy
          stderrStrict = BL.toStrict stderrLazy
      pure $ case exitCode of
        ExitSuccess -> Right stdoutStrict
        ExitFailure _ -> Left $ decodeUtf8 stderrStrict
    _ -> pure $ Left "Failed to execute GraphViz 'dot' command"
 where
  processSpec =
    (proc "dot" args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

runDotText :: [String] -> Text.Text -> IO (Either Text.Text Text.Text)
runDotText args dotSource = do
  result <- runDotBinary args dotSource
  pure $ case result of
    Left err -> Left err
    Right bytes -> case Text.decodeUtf8' bytes of
      Left ex -> Left (Text.pack (show ex))
      Right txt -> Right txt

decodeUtf8 :: BS.ByteString -> Text.Text
decodeUtf8 = Text.decodeUtf8With Text.lenientDecode
