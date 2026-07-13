-- | @l4 render FILE@ — deterministic L4 → document export.
--
-- Emits the 'L4.Export.Document' IR in one of several formats:
--
--   * @json@ — the document IR (consumed by the TS renderers)
--   * @plan@ — the export plan (imports\/rules tree + reachability)
--   * @text@ — a plain-text rendering of the document
--   * @html@ — a standalone, styled HTML document
--   * @akn@  — Akoma Ntoso XML
--
-- The actual rendering lives in 'L4.Export.Render' (in @jl4-core@) so the CLI
-- and the @jl4-lsp@ server share one implementation; this module only handles
-- the CLI surface (option parsing, loading the file, writing output).
--
-- "Don't render unused definitions and rules" is on by default; pass
-- @--include-unused@ to render unreachable imported material too.
module L4.Cli.Render
  ( RenderOptions(..)
  , RenderFormat(..)
  , renderOptionsParser
  , renderCmd
  ) where

import Base
import qualified Base.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.Export.Document
import L4.Export.Render (RenderConfig(..), renderAkn, renderHtml, renderText)
import L4.Syntax

import L4.Cli.Common

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data RenderFormat = FmtJson | FmtPlan | FmtText | FmtHtml | FmtAkn
  deriving (Eq, Show)

data RenderOptions = RenderOptions
  { renderFile           :: FilePath
  , renderFormat         :: RenderFormat
  , renderOutput         :: Maybe FilePath
  , renderIncludeUnused  :: Bool
  , renderNumberSections :: Bool
  , renderNumberClauses  :: Bool
  , renderToc            :: Bool
  , renderFixedNow       :: FixedNowOpt
  }

renderFormatReader :: ReadM RenderFormat
renderFormatReader = eitherReader \input ->
  case Text.toLower (Text.pack input) of
    "json" -> Right FmtJson
    "plan" -> Right FmtPlan
    "text" -> Right FmtText
    "html" -> Right FmtHtml
    "akn"  -> Right FmtAkn
    "xml"  -> Right FmtAkn
    other  -> Left $ "Invalid format: " <> Text.unpack other <> " (expected json|plan|text|html|akn)"

renderOptionsParser :: Parser RenderOptions
renderOptionsParser = RenderOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file to render")
  <*> option renderFormatReader
        ( long "format"
       <> metavar "FMT"
       <> value FmtHtml
       <> showDefaultWith (const "html")
       <> help "Output format: json|plan|text|html|akn"
        )
  <*> optional
        ( strOption
            ( long "output"
           <> short 'o'
           <> metavar "FILE"
           <> help "Write to FILE instead of stdout"
            )
        )
  <*> switch
        ( long "include-unused"
       <> help "Also render imported definitions/rules not referenced by this document"
        )
  <*> switch
        ( long "number-sections"
       <> help "Number section headings (§ 1, 1.1, …); off by default"
        )
  <*> switch
        ( long "number-clauses"
       <> help "Number clauses (1.); off by default"
        )
  <*> switch
        ( long "toc"
       <> help "Prepend a linked table of contents (HTML only)"
        )
  <*> fixedNowParser

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

renderCmd :: RenderOptions -> IO ()
renderCmd opts = do
  evalConfig <- makeEvalConfig opts.renderFixedNow
  (errs, mTc) <- runOneshot evalConfig opts.renderFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.SuccessfulTypeCheck uri

  case mTc of
    Nothing -> do
      putDiagnostics errs
      exitFailure
    Just tc -> do
      -- Surface any diagnostics on stderr, but still render: a document is
      -- useful even when the file has non-fatal issues.
      putDiagnostics errs
      let cfg = defaultExportConfig
                  { dropUnused = not opts.renderIncludeUnused
                  , mixfixHeadings = mixfixHeadingsFromRegistry tc.mixfixRegistry
                  }
          rcfg = MkRenderConfig
                   { numberSections = opts.renderNumberSections
                   , numberClauses  = opts.renderNumberClauses
                   , toc            = opts.renderToc
                   }
          doc = buildDocument cfg tc.module' (dedupModules (transitiveDeps tc))
      case opts.renderFormat of
        FmtJson -> emitBytes opts (Aeson.encode doc)
        FmtPlan -> emitBytes opts (Aeson.encode (buildPlan cfg tc.module' (dedupModules (transitiveDeps tc))))
        FmtText -> emitText opts (renderText rcfg doc)
        FmtHtml -> emitText opts (renderHtml rcfg doc)
        FmtAkn  -> emitText opts (renderAkn doc)
      exitSuccess

----------------------------------------------------------------------------
-- Output dispatch
----------------------------------------------------------------------------

emitText :: RenderOptions -> Text -> IO ()
emitText opts t = case opts.renderOutput of
  Just f  -> Text.writeFile f t
  Nothing -> Text.putStr t

emitBytes :: RenderOptions -> BSL8.ByteString -> IO ()
emitBytes opts b = case opts.renderOutput of
  Just f  -> BSL8.writeFile f b
  Nothing -> BSL8.putStrLn b

-- | All transitively-imported modules (the resolved dependency forest).
transitiveDeps :: Rules.TypeCheckResult -> [Module Resolved]
transitiveDeps tc = go tc.dependencies
 where
  go = concatMap (\d -> d.module' : go d.dependencies)

dedupModules :: [Module Resolved] -> [Module Resolved]
dedupModules = List.nubBy (\a b -> muri a == muri b)
 where
  muri (MkModule _ u _) = u
