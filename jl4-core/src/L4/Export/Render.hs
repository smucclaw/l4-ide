-- | Presentation of the 'L4.Export.Document' IR: plain text, a styled
-- standalone HTML document, and Akoma Ntoso XML.
--
-- This lives in @jl4-core@ so the @l4 render@ CLI and the @jl4-lsp@ server
-- share one rendering implementation. The numbering knobs ('numberSections',
-- 'numberClauses') are off by default; subclause @(a)(b)@ lettering is always
-- on (it is purely CSS, applied in HTML).
module L4.Export.Render
  ( RenderConfig (..)
  , defaultRenderConfig
  , renderHtml
  , renderText
  , renderAkn
  ) where

import Base

import qualified Base.Text as Text
import Data.Char (toUpper)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import L4.Export.Document

-- | Rendering knobs. Both default to off (no auto-enumeration of section
-- headings or top-level clauses).
data RenderConfig = MkRenderConfig
  { numberSections :: !Bool
  , numberClauses  :: !Bool
  , toc            :: !Bool   -- ^ prepend a linked table of contents (HTML)
  }
  deriving stock (Eq, Show, Generic)

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  MkRenderConfig { numberSections = False, numberClauses = False, toc = False }

-- ----------------------------------------------------------------------------
-- Plain-text rendering
-- ----------------------------------------------------------------------------

renderText :: RenderConfig -> Document -> Text
renderText cfg doc =
  Text.intercalate "\n"
    ( doc.docTitle
    : Text.replicate (Text.length doc.docTitle) "="
    : ""
    : concatMap (sectionText 1) doc.docSections
    )
 where
  sectionText depth sec =
    let childDepth = if Text.null sec.sectionNumber then depth else depth + 1
    in sectionHeadingText depth sec
         <> concatMap groupText sec.sectionGroups
         <> concatMap (sectionText childDepth) sec.sectionSubs

  sectionHeadingText depth sec =
    case sectionLabel cfg.numberSections depth sec of
      Nothing -> []
      Just h  -> ["", h, ""]

  groupText g =
    maybe [] (\l -> [l <> ":", ""]) g.groupLabel
      <> concat (zipWith blockText [1 ..] g.groupBlocks)

  blockText n b =
    let num = Text.pack (show (n :: Int))
        marker = if cfg.numberClauses then num <> ". " else "• "
        lead = marker <> capitalize b.blockHeading <> " "
    in case b.blockRenderedAs of
      RenderedReference forced ->
        [ marker <> capitalize b.blockHeading
            <> "  (" <> fromMaybe "referenced" b.blockCitation <> ")"
            <> (if forced then " [kept: still required]" else "")
        , ""
        ]
      RenderedInline -> case b.blockBody of
        CLeaf "" -> [lead <> b.blockConnector <> ".", ""]
        CLeaf t  -> [lead <> b.blockConnector <> " " <> sb t <> ".", ""]
        body     -> (lead <> b.blockConnector <> ":")
                      : map ("    " <>) (clauseLines body)
                      <> [""]

  clauseLines :: Clause -> [Text]
  clauseLines = \case
    CLeaf t    -> [sb t]
    CFields fs -> map fieldLine fs
    CAll cs    -> "all of the following are true:" : concatMap (bullet . clauseLines) cs
    CAny cs    -> "any of the following is true:"  : concatMap (bullet . clauseLines) cs
    CIf chain els -> ifLines chain els
    CCases s brs  -> ("depending on " <> sb s <> ":") : concatMap caseLines brs
    CDeontic p m a due prov h l -> deonLines p m a due prov h l
    CTable cols rows -> tableLines cols rows
    CWhere inner defs -> clauseLines inner <> ("where:" : concatMap whereDefLines defs)

  whereDefLines (h, c, cl) =
    let lead = capitalize (sb h) <> " " <> c
    in bullet $ case cl of
      CLeaf "" -> [lead <> "."]
      CLeaf t  -> [lead <> " " <> sb t <> "."]
      body     -> (lead <> ":") : map ("  " <>) (clauseLines body)

  -- A plain-text table with padded columns; complex cells collapse to one line.
  tableLines cols rows =
    let cellText c = Text.unwords (Text.words (Text.intercalate " " (clauseLines c)))
        textRows = [ map cellText row | row <- rows ]
        widths = [ maximum (Text.length h : [ Text.length (r !! i) | r <- textRows, i < length r ])
                 | (i, h) <- zip [0 ..] cols ]
        pad w t = t <> Text.replicate (w - Text.length t) " "
        renderRow cells = "| " <> Text.intercalate " | " (zipWith pad widths cells) <> " |"
        sep = "|" <> Text.intercalate "|" (map (\w -> Text.replicate (w + 2) "-") widths) <> "|"
    in renderRow cols : sep : map renderRow textRows

  fieldLine (k, v) = "- " <> sb k <> (if Text.null v then "" else " — " <> sb v)

  bullet :: [Text] -> [Text]
  bullet (l : ls) = ("- " <> l) : map ("  " <>) ls
  bullet []       = []

  branchLine kw (c, t) = case clauseLines t of
    [one] -> ["- " <> kw <> sb c <> ": " <> one]
    more  -> ("- " <> kw <> sb c <> ":") : map ("  " <>) more

  elseLine e = case clauseLines e of
    [one] -> ["- otherwise: " <> one]
    more  -> "- otherwise:" : map ("  " <>) more

  ifLines chain els =
    concat (zipWith (\i br -> branchLine (if (i :: Int) == 0 then "if " else "otherwise, if ") br) [0 ..] chain)
      <> maybe [] elseLine els

  caseLines (pat, cl) =
    let kw = if pat == "otherwise" then "otherwise" else "if it is " <> sb pat
    in case clauseLines cl of
      [one] -> ["- " <> kw <> ": " <> one]
      more  -> ("- " <> kw <> ":") : map ("  " <>) more

  deonLines p m a due prov h l =
    let head' = sb p <> " " <> m <> " " <> sb a
                  <> maybe "" (\d -> " within " <> sb d) due
                  <> maybe "" (\x -> ", provided that " <> sb x) prov
        conseq = catMaybes [ fmap (("on compliance, ",)) h, fmap (("on breach, ",)) l ]
    in if null conseq
         then [head' <> "."]
         else (head' <> ":") : concatMap (\(lab, c) -> bullet (prefixFirst lab (clauseLines c))) conseq

  prefixFirst lab (x : xs) = (lab <> x) : xs
  prefixFirst _   []       = []

  sb = stripBackticks

-- ----------------------------------------------------------------------------
-- HTML rendering
-- ----------------------------------------------------------------------------

renderHtml :: RenderConfig -> Document -> Text
renderHtml cfg doc =
  Text.intercalate "\n"
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "<meta charset=\"utf-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "<title>" <> esc doc.docTitle <> "</title>"
    , "<style>" <> css <> "</style>"
    , "</head>"
    , "<body>"
    , "<article class=\"l4-doc" <> (if cfg.numberClauses then " num-clauses" else "") <> "\">"
    , "<h1>" <> esc doc.docTitle <> "</h1>"
    , if cfg.toc then tocHtml cfg doc.docSections else ""
    , Text.concat (map (sectionHtml 1) doc.docSections)
    , "</article>"
    , "</body>"
    , "</html>"
    ]
 where
  linkMap = collectLinks doc

  sectionHtml depth sec =
    let childDepth = if Text.null sec.sectionNumber then depth else depth + 1
    in headingHtml depth sec
         <> Text.concat (map groupHtml sec.sectionGroups)
         <> Text.concat (map (sectionHtml childDepth) sec.sectionSubs)

  headingHtml depth sec =
    case sectionLabel cfg.numberSections depth sec of
      Nothing -> ""
      Just h  ->
        let tag = "h" <> Text.pack (show (min 4 (depth + 1)))
        in "<" <> tag <> " id=\"" <> sectionAnchor sec <> "\" class=\"section\">"
             <> esc h <> "</" <> tag <> ">"

  groupHtml g =
    maybe "" (\l -> "<p class=\"group-label\">" <> esc l <> "</p>") g.groupLabel
      <> "<ol class=\"clauses\">"
      <> Text.concat (map blockHtml g.groupBlocks)
      <> "</ol>"

  blockHtml b =
    let kindClass = case b.blockKind of
          RuleUnit       -> "rule"
          TypeUnit       -> "type"
          AssumptionUnit -> "assumption"
        anchor = " id=\"def_" <> anchorOf b <> "\""
    in case b.blockRenderedAs of
      RenderedInline ->
        "<li class=\"clause " <> kindClass <> "\"" <> anchor <> ">"
          <> termHtml b <> " " <> bodyHtml b.blockConnector b.blockBody
          <> "</li>"
      RenderedReference forced ->
        "<li class=\"clause reference " <> kindClass <> "\"" <> anchor <> ">"
          <> "<span class=\"term\">" <> esc (capitalize b.blockHeading) <> "</span> — "
          <> "<em class=\"citation\">" <> esc (fromMaybe "referenced" b.blockCitation) <> "</em>"
          <> (if forced then " <span class=\"forced\">(retained — still required)</span>" else "")
          <> "</li>"

  termHtml b =
    let inner = esc (capitalize b.blockHeading)
        quoted = if b.blockKind == TypeUnit then "“" <> inner <> "”" else inner
    in "<span class=\"term\">" <> quoted <> "</span>"

  bodyHtml conn = \case
    CLeaf "" -> esc conn <> "."
    CLeaf t  -> esc conn <> " " <> prose t <> "."
    CFields fs -> esc conn <> ":" <> fieldsHtml fs
    CAll cs  -> introAll conn <> treeHtml cs
    CAny cs  -> introAny conn <> treeHtml cs
    CIf chain els -> esc conn <> " as follows:" <> ifHtml chain els
    CCases s brs  -> esc conn <> ", depending on " <> prose s <> ":" <> casesHtml brs
    CDeontic p m a due prov h l -> esc conn <> " " <> deonticHtml p m a due prov h l
    CTable cols rows -> esc conn <> ":" <> tableHtml cols rows
    CWhere inner defs -> bodyHtml conn inner <> whereHtml defs

  introAll conn = if conn == "holds if"
    then "holds if all of the following are true:"
    else esc conn <> " all of the following are true:"
  introAny conn = if conn == "holds if"
    then "holds if any of the following is true:"
    else esc conn <> " any of the following is true:"

  clauseHtml = \case
    CLeaf t  -> prose t
    CFields fs -> fieldsHtml fs
    CAll cs  -> "all of the following are true:" <> treeHtml cs
    CAny cs  -> "any of the following is true:" <> treeHtml cs
    CIf chain els -> ifHtml chain els
    CCases s brs  -> "depending on " <> prose s <> ":" <> casesHtml brs
    CDeontic p m a due prov h l -> deonticHtml p m a due prov h l
    CTable cols rows -> tableHtml cols rows
    CWhere inner defs -> clauseHtml inner <> whereHtml defs

  whereHtml defs =
    " <span class=\"where-label\">where:</span>"
      <> "<ol class=\"where-defs\">"
      <> Text.concat (map whereDefLi defs)
      <> "</ol>"
   where
    whereDefLi (h, c, cl) =
      "<li><span class=\"term\">" <> esc (capitalize h) <> "</span> "
        <> bodyHtml c cl <> "</li>"

  tableHtml cols rows =
    "<table class=\"l4-table\"><thead><tr>"
      <> Text.concat (map (\c -> "<th>" <> esc c <> "</th>") cols)
      <> "</tr></thead><tbody>"
      <> Text.concat (map rowHtml rows)
      <> "</tbody></table>"
   where
    rowHtml cells =
      "<tr>" <> Text.concat (map (\c -> "<td>" <> clauseHtml c <> "</td>") cells) <> "</tr>"

  treeHtml cs =
    "<ol class=\"tree\">" <> Text.concat (map (\c -> "<li>" <> clauseHtml c <> "</li>") cs) <> "</ol>"

  ifHtml chain els =
    "<ol class=\"tree\">"
      <> Text.concat (zipWith branchLi [0 ..] chain)
      <> maybe "" (\e -> "<li>" <> kw "Otherwise" <> ": " <> clauseHtml e <> "</li>") els
      <> "</ol>"
   where
    branchLi i (cond, then_) =
      "<li>" <> kw (if (i :: Int) == 0 then "If" else "Otherwise, if")
        <> " <span class=\"cond\">" <> lk cond <> "</span>: " <> clauseHtml then_ <> "</li>"

  casesHtml brs =
    "<ol class=\"tree\">" <> Text.concat (map caseLi brs) <> "</ol>"
   where
    caseLi (pat, cl)
      | pat == "otherwise" = "<li>" <> kw "Otherwise" <> ": " <> clauseHtml cl <> "</li>"
      | otherwise = "<li>" <> kw "If it is" <> " <span class=\"cond\">" <> lk pat <> "</span>: " <> clauseHtml cl <> "</li>"

  deonticHtml party modal act due prov hence lest =
    let base = prose party <> " <strong>" <> esc modal <> "</strong> " <> prose act
                 <> maybe "" (\d -> " within " <> prose d) due
                 <> maybe "" (\p -> ", provided that " <> prose p) prov
        conseq = catMaybes [ fmap (("On compliance, ",)) hence, fmap (("On breach, ",)) lest ]
    in case conseq of
      []          -> base <> "."
      [(lab, c)]  -> base <> ". " <> lab <> clauseHtml c <> "."
      cs          -> base <> ":<ol class=\"tree\">"
                       <> Text.concat (map (\(lab, c) -> "<li>" <> lab <> clauseHtml c <> "</li>") cs)
                       <> "</ol>"

  fieldsHtml fs =
    "<ol class=\"tree\">" <> Text.concat (map fieldLi fs) <> "</ol>"
   where
    fieldLi (k, v) =
      "<li><span class=\"field\">" <> lk k <> "</span>"
        <> (if Text.null v then "" else " — " <> prose v) <> "</li>"

  kw w = "<span class=\"kw\">" <> w <> "</span>"
  lk = proseHtml linkMap
  prose t = "<span class=\"prose\">" <> lk t <> "</span>"

-- ----------------------------------------------------------------------------
-- Akoma Ntoso (AKN/XML) rendering
-- ----------------------------------------------------------------------------

renderAkn :: Document -> Text
renderAkn doc =
  Text.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<akomaNtoso xmlns=\"http://docs.oasis-open.org/legaldocml/ns/akn/3.0\">\n"
    , "<act name=\"act\">\n"
    , aknMeta
    , "<preface><longTitle><p>" <> xesc doc.docTitle <> "</p></longTitle></preface>\n"
    , "<body>\n"
    , Text.concat (map sectionAkn doc.docSections)
    , "</body>\n</act>\n</akomaNtoso>\n"
    ]
 where
  aknMeta =
    "<meta><identification source=\"#l4\">"
      <> frbr "FRBRWork" "/akn/doc/main" "/akn/doc"
      <> frbr "FRBRExpression" "/akn/doc/main/eng@" "/akn/doc/eng@"
      <> frbr "FRBRManifestation" "/akn/doc/main/eng@.xml" "/akn/doc/eng@.xml"
      <> "</identification></meta>\n"
  frbr el this uri =
    "<" <> el <> "><FRBRthis value=\"" <> this <> "\"/><FRBRuri value=\"" <> uri
      <> "\"/><FRBRdate date=\"1970-01-01\" name=\"generation\"/><FRBRauthor href=\"#l4\"/></" <> el <> ">"

  sectionAkn sec
    | Text.null sec.sectionNumber, sec.sectionHeading == Nothing =
        Text.concat (map groupAkn sec.sectionGroups)
          <> Text.concat (map sectionAkn sec.sectionSubs)
    | otherwise =
        "<section eId=\"sec_" <> idSafe sec.sectionNumber <> "\">"
          <> (if Text.null sec.sectionNumber then "" else "<num>" <> xesc sec.sectionNumber <> "</num>")
          <> maybe "" (\h -> "<heading>" <> xesc h <> "</heading>") sec.sectionHeading
          <> Text.concat (map groupAkn sec.sectionGroups)
          <> Text.concat (map sectionAkn sec.sectionSubs)
          <> "</section>\n"

  groupAkn g =
    maybe "" (\l -> "<hcontainer name=\"crossHeading\"><heading>" <> xesc l <> "</heading></hcontainer>") g.groupLabel
      <> Text.concat (map blockAkn g.groupBlocks)

  blockAkn b =
    "<paragraph eId=\"para_" <> anchorOf b <> "\">"
      <> "<heading>" <> xesc (capitalize b.blockHeading) <> "</heading>"
      <> "<content>" <> bodyAkn b <> "</content>"
      <> "</paragraph>\n"

  bodyAkn b = case b.blockRenderedAs of
    RenderedReference _ -> p (xesc (fromMaybe "referenced" b.blockCitation))
    RenderedInline -> case b.blockBody of
      CLeaf "" -> p (xt b.blockConnector <> ".")
      CLeaf t  -> p (xt b.blockConnector <> " " <> xt t <> ".")
      CFields fs -> p (xt b.blockConnector <> ":") <> fieldsAkn fs
      CAll cs  -> p (xt b.blockConnector <> " all of the following are true:") <> listAkn cs
      CAny cs  -> p (xt b.blockConnector <> " any of the following is true:") <> listAkn cs
      CIf chain els -> p (xt b.blockConnector <> " as follows:") <> ifAkn chain els
      CCases s brs -> p (xt b.blockConnector <> ", depending on " <> xt s <> ":") <> casesAkn brs
      CDeontic pa m a due prov h l -> deonticAkn pa m a due prov h l
      CTable cols rows -> p (xt b.blockConnector <> ":") <> tableAkn cols rows
      CWhere inner defs -> bodyAkn b { blockBody = inner } <> whereAkn defs

  clauseAkn = \case
    CLeaf t  -> p (xt t)
    CFields fs -> fieldsAkn fs
    CAll cs  -> p "all of the following are true:" <> listAkn cs
    CAny cs  -> p "any of the following is true:" <> listAkn cs
    CIf chain els -> ifAkn chain els
    CCases s brs  -> p ("depending on " <> xt s <> ":") <> casesAkn brs
    CDeontic pa m a due prov h l -> deonticAkn pa m a due prov h l
    CTable cols rows -> tableAkn cols rows
    CWhere inner defs -> clauseAkn inner <> whereAkn defs

  whereAkn defs =
    p "where:" <> blockList (map whereItem defs)
   where
    whereItem (h, c, cl) =
      item (p (xt (capitalize h) <> " " <> xt c <> ":") <> clauseAkn cl)

  tableAkn cols rows =
    "<table>"
      <> "<tr>" <> Text.concat (map (\c -> "<th>" <> p (xesc c) <> "</th>") cols) <> "</tr>"
      <> Text.concat
           (map (\row -> "<tr>" <> Text.concat (map (\c -> "<td>" <> clauseAkn c <> "</td>") row) <> "</tr>") rows)
      <> "</table>"

  listAkn cs = blockList (map (item . clauseAkn) cs)

  ifAkn chain els =
    blockList $
      zipWith (\i (c, t) -> item (p ((if (i :: Int) == 0 then "if " else "otherwise, if ") <> xt c <> ":") <> clauseAkn t)) [0 ..] chain
      <> maybe [] (\e -> [item (p "otherwise:" <> clauseAkn e)]) els

  casesAkn brs =
    blockList
      [ item (p (kw <> ":") <> clauseAkn cl)
      | (pat, cl) <- brs
      , let kw = if pat == "otherwise" then "otherwise" else "if it is " <> xt pat
      ]

  deonticAkn party modal act due prov hence lest =
    p (xt party <> " " <> xt modal <> " " <> xt act
        <> maybe "" (\d -> " within " <> xt d) due
        <> maybe "" (\x -> ", provided that " <> xt x) prov
        <> (if null conseq then "." else ":"))
      <> (if null conseq then "" else blockList (map (\(lab, c) -> item (p lab <> clauseAkn c)) conseq))
   where
    conseq = catMaybes [ fmap (("On compliance, ",)) hence, fmap (("On breach, ",)) lest ]

  fieldsAkn fs =
    blockList [ item (p (xt k <> if Text.null v then "" else " — " <> xt v)) | (k, v) <- fs ]

  blockList items = "<blockList>" <> Text.concat items <> "</blockList>"
  item content = "<item>" <> content <> "</item>"
  p content = "<p>" <> content <> "</p>"

-- ----------------------------------------------------------------------------
-- Shared helpers
-- ----------------------------------------------------------------------------

sectionLabel :: Bool -> Int -> DocSection -> Maybe Text
sectionLabel numbered depth sec =
  let num = sec.sectionNumber
      marker
        | not numbered   = ""
        | Text.null num  = ""
        | depth <= 1     = "§ " <> num <> "  "
        | otherwise      = num <> "  "
      body = Text.stripEnd (marker <> fromMaybe "" sec.sectionHeading)
  in if Text.null body then Nothing else Just body

capitalize :: Text -> Text
capitalize t = case Text.uncons t of
  Just (c, rest) -> Text.cons (toUpper c) rest
  Nothing        -> t

stripBackticks :: Text -> Text
stripBackticks = Text.filter (/= '`')

xt :: Text -> Text
xt = xesc . stripBackticks

xesc :: Text -> Text
xesc =
    Text.replace "<" "&lt;"
  . Text.replace ">" "&gt;"
  . Text.replace "&" "&amp;"

idSafe :: Text -> Text
idSafe = Text.map (\c -> if c `elem` (" :/.#" :: String) then '_' else c)

-- | A short, stable, path-free anchor for a block: @<sort>_<n>_<module>@.
anchorOf :: Block -> Text
anchorOf b =
  let parts   = Text.splitOn ":" b.blockId
      sortIdx = Text.intercalate "_" (take 2 parts)
      uriPart = Text.intercalate ":" (drop 2 parts)
      lastSeg = if Text.null uriPart then "" else List.last (Text.splitOn "/" uriPart)
      base    = fromMaybe lastSeg (Text.stripSuffix ".l4" lastSeg)
  in idSafe (sortIdx <> (if Text.null base then "" else "_" <> base))

-- | Variable spans (the linearizer's @`backticks`@) become links to the
-- defining clause when the name is a defined term — with a @title@ tooltip
-- showing the definition — otherwise plain italic.
proseHtml :: Map.Map Text (Text, Text) -> Text -> Text
proseHtml linkMap t =
  case Text.splitOn "`" t of
    []         -> ""
    [x]        -> esc x
    (x : rest) -> esc x <> wrap rest
 where
  wrap [] = ""
  wrap [x] = esc x
  wrap (varSeg : afterSeg : more) = renderVar varSeg <> esc afterSeg <> wrap more
  renderVar name = case Map.lookup name linkMap of
    Just (anchor, tip) ->
      "<a class=\"ref\" href=\"#def_" <> anchor <> "\" title=\"" <> esc tip <> "\">"
        <> esc name <> "</a>"
    Nothing -> "<span class=\"var\">" <> esc name <> "</span>"

-- | Each defined term -> (anchor of its clause, a short definition for the
-- hover tooltip).
collectLinks :: Document -> Map.Map Text (Text, Text)
collectLinks doc = Map.fromList (concatMap sec doc.docSections)
 where
  sec s = concatMap grp s.sectionGroups <> concatMap sec s.sectionSubs
  grp g = map (\b -> (b.blockHeading, (anchorOf b, blockTip b))) g.groupBlocks

blockTip :: Block -> Text
blockTip b =
  Text.strip (Text.take 200 (stripBackticks (b.blockConnector <> " " <> clauseSummary b.blockBody)))

-- | A one-line gist of a clause for tooltips.
clauseSummary :: Clause -> Text
clauseSummary = \case
  CLeaf t        -> t
  CFields fs     -> "a record with " <> Text.intercalate ", " (map fst fs)
  CAll _         -> "all of several conditions"
  CAny _         -> "any of several conditions"
  CIf _ _        -> "determined by conditions"
  CCases s _     -> "depending on " <> s
  CDeontic p m a _ _ _ _ -> p <> " " <> m <> " " <> a
  CTable cols _  -> "a table of " <> Text.intercalate ", " cols
  CWhere body _  -> clauseSummary body

-- | A stable anchor for a section heading (from its number, else its heading).
sectionAnchor :: DocSection -> Text
sectionAnchor sec =
  "sec_" <> if Text.null sec.sectionNumber
              then idSafe (fromMaybe "" sec.sectionHeading)
              else idSafe sec.sectionNumber

-- | A linked, nested table of contents from the section hierarchy.
tocHtml :: RenderConfig -> [DocSection] -> Text
tocHtml cfg secs =
  let body = level 1 secs
  in if Text.null body
       then ""
       else "<nav class=\"toc\"><div class=\"toc-title\">Contents</div>" <> body <> "</nav>"
 where
  level depth ss =
    let entries = Text.concat (map (entry depth) ss)
    in if Text.null entries then "" else "<ul>" <> entries <> "</ul>"
  entry depth s =
    let childDepth = if Text.null s.sectionNumber then depth else depth + 1
        subs = level childDepth s.sectionSubs
    in case sectionLabel cfg.numberSections depth s of
      Nothing -> subs   -- the unnumbered document-body root: splice children up
      Just h  ->
        "<li><a href=\"#" <> sectionAnchor s <> "\">" <> esc h <> "</a>" <> subs <> "</li>"

esc :: Text -> Text
esc =
    Text.replace "<" "&lt;"
  . Text.replace ">" "&gt;"
  . Text.replace "\"" "&quot;"
  . Text.replace "&" "&amp;"

-- | Styling that mimics a printed legal contract.
css :: Text
css = Text.intercalate "\n"
  [ "  :root { --ink:#000; --muted:#3a3a3a; }"
  , "  * { box-sizing:border-box; }"
  , "  body { margin:0; background:#fff; color:var(--ink);"
  , "         font:16px/1.62 'Times New Roman', Times, serif; }"
  , "  .l4-doc { width:100%; max-width:1024px; margin:0 auto; padding:56px 64px;"
  , "            background:#fff; }"
  , "  h1 { text-align:center; font-size:1.65rem; font-weight:700;"
  , "       text-transform:uppercase; letter-spacing:.02em; margin:0 0 2.2rem; }"
  , "  .section { font-weight:700; margin:2.1rem 0 .9rem; line-height:1.3; }"
  , "  h2.section { font-size:1.3rem; }"
  , "  h3.section { font-size:1.12rem; }"
  , "  h4.section { font-size:1.02rem; }"
  , "  .group-label { font-weight:700; font-style:italic; margin:1.25rem 0 .55rem; }"
  , "  .term { font-weight:700; }"
  , "  .prose .var { font-style:italic; }"
  , "  li.reference { color:var(--muted); }"
  , "  .citation { font-style:italic; }"
  , "  .forced { font-size:.85em; color:#8a2b2b; font-style:normal; }"
  -- Table of contents.
  , "  nav.toc { margin:0 0 2rem; padding:1rem 1.25rem; background:#fafafa;"
  , "            border:1px solid #e6e6e6; }"
  , "  nav.toc .toc-title { font-weight:700; text-transform:uppercase;"
  , "       letter-spacing:.08em; font-size:.8rem; color:var(--muted); margin-bottom:.5rem; }"
  , "  nav.toc ul { margin:0; padding-left:1.2rem; list-style:none; }"
  , "  nav.toc > ul { padding-left:0; }"
  , "  nav.toc li { margin:.15rem 0; }"
  , "  nav.toc a { color:#1a3a6b; text-decoration:none; }"
  , "  nav.toc a:hover { text-decoration:underline; }"
  , "  ol.clauses { margin:0 0 1rem; padding:0; list-style:none; counter-reset:clause; }"
  , "  ol.clauses > li.clause { position:relative; margin:0 0 1rem; }"
  , "  ol.tree { margin:.35rem 0 .35rem 1.6rem; padding:0; list-style:none; counter-reset:t; }"
  , "  ol.tree > li { position:relative; margin:.28rem 0; padding-left:1.9rem; }"
  , "  ol.tree > li::before {"
  , "       counter-increment:t; content:'(' counter(t, lower-alpha) ')';"
  , "       position:absolute; left:0; top:0; width:1.5rem; color:var(--muted); }"
  , "  ol.tree ol.tree > li::before { content:'(' counter(t, lower-roman) ')'; }"
  , "  ol.tree ol.tree ol.tree > li::before { content:'(' counter(t, decimal) ')'; }"
  , "  ol.tree ol.tree ol.tree ol.tree > li::before { content:'(' counter(t, upper-alpha) ')'; }"
  , "  .kw { font-weight:700; }"
  , "  .cond { font-style:italic; color:#333; }"
  , "  a.ref { color:#1a3a6b; text-decoration:none; border-bottom:1px dotted #9bb; }"
  , "  a.ref:hover { border-bottom-style:solid; }"
  , "  .field { font-weight:600; }"
  -- Local WHERE / LET definitions attached to a rule.
  , "  .where-label { font-style:italic; color:var(--muted); margin-left:.15rem; }"
  , "  ol.where-defs { margin:.25rem 0 .35rem 1.6rem; padding-left:0; list-style:none; }"
  , "  ol.where-defs > li { margin:.2rem 0; }"
  -- Tables for list-of-record definitions.
  , "  table.l4-table { border-collapse:collapse; margin:.6rem 0; width:100%;"
  , "                   font-size:.95em; }"
  , "  table.l4-table th, table.l4-table td {"
  , "       border:1px solid #cfcfcf; padding:.3rem .55rem; text-align:left;"
  , "       vertical-align:top; }"
  , "  table.l4-table th { background:#f0f0f0; font-weight:700; }"
  , "  table.l4-table tbody tr:nth-child(even) { background:#fafafa; }"
  , "  table.l4-table table.l4-table { margin:.2rem 0; }"
  , "  .num-clauses ol.clauses > li.clause { padding-left:2.4rem; }"
  , "  .num-clauses ol.clauses > li.clause::before {"
  , "       counter-increment:clause; content:counter(clause) '.';"
  , "       position:absolute; left:0; top:0; width:1.9rem; text-align:right;"
  , "       font-variant-numeric:tabular-nums; }"
  , "  @page { size:A4; margin:25mm 22mm; }"
  , "  @media print {"
  , "    body { background:#fff; }"
  , "    .l4-doc { width:auto; margin:0; padding:0; border:0; box-shadow:none; }"
  , "    ol.clauses > li.clause { break-inside:avoid; }"
  , "    h1, .section { break-after:avoid; }"
  , "  }"
  ]
