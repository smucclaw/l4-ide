{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Options.Applicative
import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, makeAbsolute)
import System.FilePath (takeDirectory)
import Text.Printf (printf)

import Development.IDE.Graph.Database (shakeRunDatabase)
import Language.LSP.Protocol.Types (toNormalizedFilePath, normalizedFilePathToUri)

import LSP.Logger (makeDefaultStderrRecorder, cmapWithPrio, cfilter, pretty, Recorder, WithPriority(..), Priority(..))
import L4.TypeCheck.Types (CheckError(..), CheckErrorWithContext(..))
import qualified LSP.Core.Shake as Shake
import qualified LSP.Core.FileStore as Store
import qualified LSP.L4.Rules as Rules
import qualified LSP.L4.Oneshot as Oneshot

import L4.EvaluateLazy (EvalConfig, resolveEvalConfig, EvalDirectiveResult(..), EvalDirectiveValue(..), prettyEvalException)
import qualified L4.EvaluateLazy.GraphViz2 as GraphViz
import L4.DirectiveFilter (filterIdeDirectives)
import qualified L4.Print as Print
import L4.Syntax (Module, Resolved)

-- | Command line options
data Options = Options
  { files   :: [FilePath]
  , verbose :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> many (strArgument (metavar "FILES..." <> help "L4 files to load"))
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

optionsInfo :: ParserInfo Options
optionsInfo = info (helper <*> optionsParser)
  ( fullDesc
  <> progDesc "Interactive REPL for the L4 language"
  <> header "jl4-repl - L4 Read-Eval-Print Loop"
  )

-- | REPL state (uses Oneshot.Log for compatibility with LSP infrastructure)
data ReplState = ReplState
  { ideState   :: Shake.IdeState
  , loadedFile :: Maybe FilePath   -- The main loaded file
  , curDir     :: FilePath
  , evalConfig :: EvalConfig
  , recorder   :: Recorder (WithPriority Oneshot.Log)
  , evalCounter :: IORef Int       -- Counter for unique evaluation filenames
  , imports    :: IORef [Text]     -- Accumulated IMPORT statements
  , traceSink  :: IORef (Maybe TraceSink)
  }

data TraceSink = TraceSink
  { sinkPrefix :: FilePath
  , sinkCounter :: IORef Int
  }

main :: IO ()
main = do
  opts <- execParser optionsInfo
  curDir <- getCurrentDirectory
  evalConfig <- resolveEvalConfig Nothing
  baseRecorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  let recorder = if opts.verbose
                 then baseRecorder
                 else cfilter (\wp -> wp.priority >= Warning) baseRecorder

  putStrLn "L4 REPL v0.1"
  putStrLn "Type :help for help, :quit to exit"
  putStrLn ""

  -- Create IdeState
  state <- Shake.oneshotIdeState (cmapWithPrio Oneshot.ShakeLog recorder) curDir do
    Store.fileStoreRules (cmapWithPrio Oneshot.StoreLog recorder) (const $ pure False)
    Rules.jl4Rules evalConfig curDir (cmapWithPrio Oneshot.RulesLog recorder)

  -- Create counter for unique evaluation filenames
  counter <- newIORef (0 :: Int)
  -- Create mutable list for accumulated imports
  importsRef <- newIORef ([] :: [Text])
  traceSinkRef <- newIORef Nothing

  let replState = ReplState
        { ideState = state
        , loadedFile = Nothing
        , curDir = curDir
        , evalConfig = evalConfig
        , recorder = recorder
        , evalCounter = counter
        , imports = importsRef
        , traceSink = traceSinkRef
        }

  -- If files provided, load them first
  case opts.files of
    [] -> do
      putStrLn "No files loaded. Use :load <file> to load an L4 file."
      runRepl replState
    (f:_) -> do
      absPath <- makeAbsolute f
      putStrLn $ "Loading: " ++ absPath
      let newState = replState { loadedFile = Just absPath }
      -- Trigger initial type check
      loadResult <- loadFile newState absPath
      case loadResult of
        Left err -> do
          putStrLn $ "Error loading file: " ++ Text.unpack err
          runRepl newState
        Right _ -> do
          putStrLn "File loaded successfully."
          runRepl newState

-- | Run the REPL loop
runRepl :: ReplState -> IO ()
runRepl replState = do
  let settings = defaultSettings { historyFile = Just ".jl4_history" }
  runInputT settings (loop replState)
  where
    loop :: ReplState -> InputT IO ()
    loop st = do
      minput <- getInputLine "jl4> "
      case minput of
        Nothing -> outputStrLn "Goodbye!"
        Just input -> do
          (output, newState, shouldQuit) <- liftIO $ processInput st (Text.pack input)
          unless (Text.null output) $ outputStrLn (Text.unpack output)
          if shouldQuit
            then outputStrLn "Goodbye!"
            else loop newState

-- | Load a file and trigger type checking
loadFile :: ReplState -> FilePath -> IO (Either Text ())
loadFile st fp = do
  let nfp = toNormalizedFilePath fp
      uri = normalizedFilePathToUri nfp
  -- Add file to virtual file store
  _ <- shakeRunDatabase st.ideState.shakeDb [Shake.addVirtualFileFromFS nfp]
  -- Trigger type check
  [mtc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck uri]
  case mtc of
    Just tc | tc.success -> pure (Right ())
    Just _tc -> pure (Left "Type checking failed (see diagnostics above)")
    Nothing -> pure (Left "Failed to type check file")

-- | Process a single REPL input
processInput :: ReplState -> Text -> IO (Text, ReplState, Bool)
processInput st input
  | Text.null stripped = pure ("", st, False)
  | ":quit" `Text.isPrefixOf` stripped = pure ("", st, True)
  | ":q" == stripped = pure ("", st, True)
  | ":help" `Text.isPrefixOf` stripped = pure (helpText, st, False)
  | ":h" == stripped = pure (helpText, st, False)
  | ":tracefile" == stripped =
      pure ("Usage: :tracefile <path|off>", st, False)
  | ":tracefile " `Text.isPrefixOf` stripped = do
      let arg = Text.strip $ Text.drop (Text.length (":tracefile " :: Text)) stripped
      msg <- configureTraceSink st arg
      pure (msg, st, False)
  | ":load " `Text.isPrefixOf` stripped = do
      let fp = Text.unpack $ Text.strip $ Text.drop 6 stripped
      absPath <- makeAbsolute fp
      let newState = st { loadedFile = Just absPath }
      result <- loadFile newState absPath
      case result of
        Left err -> pure ("Error: " <> err, newState, False)
        Right _ -> pure ("Loaded: " <> Text.pack absPath, newState, False)
  | ":l " `Text.isPrefixOf` stripped = do
      let fp = Text.unpack $ Text.strip $ Text.drop 3 stripped
      absPath <- makeAbsolute fp
      let newState = st { loadedFile = Just absPath }
      result <- loadFile newState absPath
      case result of
        Left err -> pure ("Error: " <> err, newState, False)
        Right _ -> pure ("Loaded: " <> Text.pack absPath, newState, False)
  | ":reload" `Text.isPrefixOf` stripped || ":r" == stripped = do
      case st.loadedFile of
        Nothing -> pure ("No file loaded", st, False)
        Just fp -> do
          result <- loadFile st fp
          case result of
            Left err -> pure ("Error reloading: " <> err, st, False)
            Right _ -> pure ("Reloaded: " <> Text.pack fp, st, False)
  | ":type " `Text.isPrefixOf` stripped = do
      let expr = Text.strip $ Text.drop 6 stripped
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- getExpressionType st fp expr
          pure (result, st, False)
  | ":t " `Text.isPrefixOf` stripped = do
      let expr = Text.strip $ Text.drop 3 stripped
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- getExpressionType st fp expr
          pure (result, st, False)
  | ":trace " `Text.isPrefixOf` stripped = do
      let expr = Text.strip $ Text.drop 7 stripped
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- evalWithTrace st fp expr
          pure (result, st, False)
  | ":tr " `Text.isPrefixOf` stripped = do
      let expr = Text.strip $ Text.drop 4 stripped
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- evalWithTrace st fp expr
          pure (result, st, False)
  | ":import " `Text.isPrefixOf` stripped = do
      -- :import <lib> is shorthand for IMPORT <lib>
      let lib = Text.strip $ Text.drop 8 stripped
      addImport st ("IMPORT " <> lib)
  | ":i " `Text.isPrefixOf` stripped = do
      -- :i <lib> is shorthand for :import
      let lib = Text.strip $ Text.drop 3 stripped
      addImport st ("IMPORT " <> lib)
  | ":imports" `Text.isPrefixOf` stripped = do
      -- Show current imports
      currentImports <- readIORef st.imports
      if null currentImports
        then pure ("No imports.", st, False)
        else pure (Text.unlines currentImports, st, False)
  | ":" `Text.isPrefixOf` stripped =
      pure ("Unknown command: " <> stripped, st, False)
  | "IMPORT " `Text.isPrefixOf` stripped = do
      -- Handle literal IMPORT statement
      addImport st stripped
  | otherwise = do
      -- Evaluate expression
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- evalExpression st fp stripped
          pure (result, st, False)
  where
    stripped = Text.strip input

-- | Add an import statement to the session
addImport :: ReplState -> Text -> IO (Text, ReplState, Bool)
addImport st importStmt = do
  currentImports <- readIORef st.imports
  if importStmt `elem` currentImports
    then pure ("Already imported: " <> importStmt, st, False)
    else do
      writeIORef st.imports (currentImports ++ [importStmt])
      pure ("Added: " <> importStmt, st, False)

-- | Get the preamble of import statements for virtual files
getImportPreamble :: ReplState -> IO Text
getImportPreamble st = do
  currentImports <- readIORef st.imports
  pure $ if null currentImports
         then ""
         else Text.unlines currentImports <> "\n"

-- | Evaluate an expression in the context of a loaded file
evalExpression :: ReplState -> FilePath -> Text -> IO Text
evalExpression st contextFile exprText = do
  -- Get unique counter for this evaluation
  evalNum <- atomicModifyIORef' st.evalCounter (\n -> (n + 1, n))
  
  -- The expression might be:
  -- 1. A bare expression like "1 + 2" or "foo"
  -- 2. A directive like "#EVAL 1 + 2" or "#CHECK foo"
  let actualExpr = if any (`Text.isPrefixOf` exprText) ["#EVAL", "#CHECK", "#ASSERT", "#EVALTRACE"]
                   then exprText
                   else "#EVAL " <> exprText

  -- Get any accumulated imports
  importPreamble <- getImportPreamble st
  
  -- Get filtered source from typechecked module (AST-level filtering)
  let contextUri = normalizedFilePathToUri (toNormalizedFilePath contextFile)
  [mTc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck contextUri]
  originalContent <- case mTc of
    Just tc -> pure $ Print.prettyLayout (filterIdeDirectives tc.module')
    Nothing -> do
      -- Fallback to raw text if typecheck failed
      mContent <- Shake.getVirtualFileText st.ideState contextUri
      case mContent of
        Just content -> pure content
        Nothing -> Text.IO.readFile contextFile
  let replContent = importPreamble <> originalContent <> "\n\n-- REPL expression " <> Text.pack (show evalNum) <> "\n" <> actualExpr <> "\n"
  
  -- Create a unique virtual file for REPL evaluation
  let replPath = st.curDir <> "/.repl_eval_" <> show evalNum <> ".l4"
      replNfp = toNormalizedFilePath replPath
      replUri = normalizedFilePathToUri replNfp

  -- Add the virtual file
  _ <- shakeRunDatabase st.ideState.shakeDb [Shake.addVirtualFile replNfp replContent]

  -- Type check and evaluate
  [mtc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck replUri]
  case mtc of
    Nothing -> pure "Failed to type check expression"
    Just tc | not tc.success -> do
      -- Extract error messages
      let errors = tc.infos
      pure $ "Type error:\n" <> Text.unlines (map (Text.pack . show) $ take 3 errors)
    Just _tc -> do
      -- Get evaluation results
      [meval] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.EvaluateLazy replUri]
      case meval of
        Nothing -> pure "Evaluation failed"
        Just [] -> 
          if "#CHECK" `Text.isPrefixOf` exprText
          then pure "#CHECK is for type-checking only (reports inferred type in IDE). Use #ASSERT for runtime checks."
          else pure "(no result)"
        Just results -> pure $ formatResults results

-- | Configure trace sink for saving DOT files
configureTraceSink :: ReplState -> Text -> IO Text
configureTraceSink st argInput = do
  let trimmed = Text.strip argInput
  if Text.null trimmed
    then pure "Usage: :tracefile <path|off>"
    else if Text.toLower trimmed == "off"
      then do
        writeIORef st.traceSink Nothing
        pure "Trace output will be printed to stdout."
      else do
        let rawPath = Text.unpack trimmed
        absPath <- makeAbsolute rawPath
        createDirectoryIfMissing True (takeDirectory absPath)
        counterRef <- newIORef 1
        writeIORef st.traceSink (Just TraceSink {sinkPrefix = absPath, sinkCounter = counterRef})
        pure $ "Trace output will be saved to " <> Text.pack absPath <> "-NN.dot"

-- | Format evaluation results for display
formatResults :: [EvalDirectiveResult] -> Text
formatResults results = Text.unlines $ map formatResult results

formatResult :: EvalDirectiveResult -> Text
formatResult (MkEvalDirectiveResult _range res _trace) = case res of
  Assertion True  -> "True (assertion passed)"
  Assertion False -> "False (assertion failed)"
  Reduction (Right nf) -> Print.prettyLayout nf
  Reduction (Left err) -> "Error: " <> Text.unlines (prettyEvalException err)

-- | Evaluate an expression and show its GraphViz trace
evalWithTrace :: ReplState -> FilePath -> Text -> IO Text
evalWithTrace st contextFile exprText = do
  -- Get unique counter
  evalNum <- atomicModifyIORef' st.evalCounter (\n -> (n + 1, n))
  
  -- Build directive (use #EVALTRACE for explicit tracing)
  let actualExpr = if any (`Text.isPrefixOf` exprText) ["#EVAL", "#EVALTRACE", "#CHECK", "#ASSERT"]
                   then exprText
                   else "#EVALTRACE " <> exprText
  
  -- Get imports
  importPreamble <- getImportPreamble st
  
  -- Get filtered source
  let contextUri = normalizedFilePathToUri (toNormalizedFilePath contextFile)
  [mTc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck contextUri]
  originalContent <- case mTc of
    Just tc -> pure $ Print.prettyLayout (filterIdeDirectives tc.module')
    Nothing -> do
      mContent <- Shake.getVirtualFileText st.ideState contextUri
      case mContent of
        Just content -> pure content
        Nothing -> Text.IO.readFile contextFile
  
  let replContent = importPreamble <> originalContent <> "\n\n-- REPL trace " <> Text.pack (show evalNum) <> "\n" <> actualExpr <> "\n"
  
  -- Create virtual file
  let replPath = st.curDir <> "/.repl_trace_" <> show evalNum <> ".l4"
      replNfp = toNormalizedFilePath replPath
      replUri = normalizedFilePathToUri replNfp
  
  _ <- shakeRunDatabase st.ideState.shakeDb [Shake.addVirtualFile replNfp replContent]
  
  -- Type check and evaluate
  [mtc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck replUri]
  case mtc of
    Nothing -> pure "Failed to type check expression"
    Just tc | not tc.success -> do
      let errors = tc.infos
      pure $ "Type error:\n" <> Text.unlines (map (Text.pack . show) $ take 3 errors)
    Just tc -> do
      -- Get evaluation results WITH trace
      [meval] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.EvaluateLazy replUri]
      case meval of
        Nothing -> pure "Evaluation failed"
        Just [] -> pure "(no result)"
        Just results -> formatTraceResults st exprText actualExpr tc.module' results

-- | Format evaluation results showing GraphViz DOT trace or save to files
formatTraceResults :: ReplState -> Text -> Text -> Module Resolved -> [EvalDirectiveResult] -> IO Text
formatTraceResults st exprText actualExpr mModule results = do
  mSink <- readIORef st.traceSink
  case mSink of
    Nothing -> pure $ Text.unlines $ map (formatTraceResult mModule) results
    Just sink -> do
      messages <- mapM (saveTraceResult st exprText actualExpr mModule sink) results
      pure $ Text.unlines messages

formatTraceResult :: Module Resolved -> EvalDirectiveResult -> Text
formatTraceResult mModule (MkEvalDirectiveResult _range _res mtrace) = case mtrace of
  Nothing -> "(no trace available)"
  Just tr -> GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions (Just mModule) tr

saveTraceResult :: ReplState -> Text -> Text -> Module Resolved -> TraceSink -> EvalDirectiveResult -> IO Text
saveTraceResult st exprText actualExpr mModule sink result@(MkEvalDirectiveResult _ _ mtrace) =
  case mtrace of
    Nothing -> pure "(no trace available)"
    Just tr -> do
      idx <- atomicModifyIORef' sink.sinkCounter (\n -> (n + 1, n))
      timestamp <- getCurrentTime
      importsList <- readIORef st.imports
      let filePath = printf "%s-%02d.dot" (sink.sinkPrefix) idx
          dirPath = takeDirectory filePath
      createDirectoryIfMissing True dirPath
      fileHeader <- buildTraceHeader st exprText actualExpr importsList timestamp result
      let dotText = GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions (Just mModule) tr
          fileContent = fileHeader <> dotText <> "\n"
      Text.IO.writeFile filePath fileContent
      pure $ "Saved trace to " <> Text.pack filePath

buildTraceHeader :: ReplState -> Text -> Text -> [Text] -> UTCTime -> EvalDirectiveResult -> IO Text
buildTraceHeader st exprText actualExpr importsList timestamp result = do
  let exprLine = inlineSingleLine exprText
      directiveLine = inlineSingleLine actualExpr
      timestampLine = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" timestamp
      loadedLine = maybe "(none)" Text.pack st.loadedFile
      importsLine =
        if null importsList
          then "(none)"
          else Text.intercalate ", " importsList
      resultLine = inlineSingleLine (summarizeEvalResult result)
      headerLines =
        [ commentLine "Expression" exprLine
        , commentLine "Directive" directiveLine
        , commentLine "Timestamp" timestampLine
        , commentLine "Loaded file" (inlineSingleLine loadedLine)
        , commentLine "Imports" (inlineSingleLine importsLine)
        , commentLine "Result" resultLine
        , ""
        ]
  pure $ Text.unlines headerLines

commentLine :: Text -> Text -> Text
commentLine label content = "// " <> label <> ": " <> content

inlineSingleLine :: Text -> Text
inlineSingleLine txt =
  let pieces = map Text.strip (Text.lines txt)
      nonEmpty = filter (not . Text.null) pieces
  in if null nonEmpty
       then ""
       else Text.intercalate " " nonEmpty

summarizeEvalResult :: EvalDirectiveResult -> Text
summarizeEvalResult (MkEvalDirectiveResult _range res _trace) = case res of
  Assertion True  -> "True (assertion passed)"
  Assertion False -> "False (assertion failed)"
  Reduction (Right nf) -> Print.prettyLayout nf
  Reduction (Left err)  ->
    Text.intercalate "; " ("Error" : prettyEvalException err)

-- | Get the type of an expression without evaluating it
-- Uses #CHECK directive which reports the type as an info message
getExpressionType :: ReplState -> FilePath -> Text -> IO Text
getExpressionType st contextFile exprText = do
  -- Get unique counter for this type query
  evalNum <- atomicModifyIORef' st.evalCounter (\n -> (n + 1, n))
  
  -- Get any accumulated imports
  importPreamble <- getImportPreamble st
  
  -- Get filtered source from typechecked module (AST-level filtering)
  let contextUri = normalizedFilePathToUri (toNormalizedFilePath contextFile)
  [mTc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck contextUri]
  originalContent <- case mTc of
    Just tc -> pure $ Print.prettyLayout (filterIdeDirectives tc.module')
    Nothing -> do
      -- Fallback to raw text if typecheck failed
      mContent <- Shake.getVirtualFileText st.ideState contextUri
      case mContent of
        Just content -> pure content
        Nothing -> Text.IO.readFile contextFile
  -- Build the virtual file content with #CHECK directive
  let replContent = importPreamble <> originalContent <> "\n\n-- REPL type query " <> Text.pack (show evalNum) <> "\n#CHECK " <> exprText <> "\n"
  
  -- Create a unique virtual file for type checking
  let replPath = st.curDir <> "/.repl_type_" <> show evalNum <> ".l4"
      replNfp = toNormalizedFilePath replPath
      replUri = normalizedFilePathToUri replNfp

  -- Add the virtual file
  _ <- shakeRunDatabase st.ideState.shakeDb [Shake.addVirtualFile replNfp replContent]

  -- Type check (no evaluation needed)
  [mtc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck replUri]
  case mtc of
    Nothing -> pure "Failed to type check expression"
    Just tc -> do
      -- Look for CheckInfo in the infos list - #CHECK adds the type as a CheckInfo
      let checkInfoTypes = [ty | MkCheckErrorWithContext (CheckInfo ty) _ <- tc.infos]
      case checkInfoTypes of
        (ty:_) -> pure $ Print.prettyLayout ty
        [] | not tc.success -> do
          -- Type error - report the first few errors
          let errors = tc.infos
          pure $ "Type error:\n" <> Text.unlines (map (Text.pack . show) $ take 3 errors)
        [] -> pure "Could not determine type"

helpText :: Text
helpText = Text.unlines
  [ "Available commands:"
  , "  :help, :h       Show this help"
  , "  :quit, :q       Exit the REPL"
  , "  :load <file>    Load an L4 file"
  , "  :l <file>       Short for :load"
  , "  :reload, :r     Reload the current file"
  , "  :type <expr>    Show the type of an expression"
  , "  :t <expr>       Short for :type"
  , "  :trace <expr>   Show evaluation trace as GraphViz DOT"
  , "  :tr <expr>      Short for :trace"
  , "  :tracefile <prefix>  Save traces to <prefix>-NN.dot"
  , "  :tracefile off       Restore stdout trace output"
  , "  :import <lib>   Import a library (e.g., :import prelude)"
  , "  :i <lib>        Short for :import"
  , "  :imports        Show current imports"
  , ""
  , "Expression syntax:"
  , "  <expr>          Evaluate expression (implicit #EVAL)"
  , "  #EVAL <expr>    Evaluate expression and show result"
  , "  #ASSERT <expr>  Evaluate boolean expression, report pass/fail"
  , "  #EVALTRACE <expr> Evaluate with trace output"
  , ""
  , "Trace visualization:"
  , "  :trace 5 cubed           Show DOT graph"
  , "  :trace 5 cubed | xdot -  View interactively"
  , "  Requires GraphViz tools (dot, xdot) installed separately"
  , ""
  , "Note: #CHECK is a type-checking directive (shows type in IDE) and"
  , "      does not evaluate. Use #ASSERT for runtime boolean checks."
  , ""
  , "Use -v/--verbose for debug output."
  ]
