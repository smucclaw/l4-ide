{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ExplorerPage (
  renderExplorerPageBS,
) where

import Types (DeploymentId (..), DeploymentState (..), DeploymentMetadata (..), FunctionSummary (..), FileEntry (..))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding

-- | Render the deployment explorer HTML page from the in-memory registry snapshot.
-- Shows all deployments including pending, compiling, and failed ones.
renderExplorerPageBS :: Map.Map DeploymentId DeploymentState -> ByteString
renderExplorerPageBS registry = LBS.fromStrict $ Text.Encoding.encodeUtf8 $ Text.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>L4 Deployments</title>"
  , "  <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
  , "  <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
  , "  <link href=\"https://fonts.googleapis.com/css2?family=Merriweather:wght@700&display=swap\" rel=\"stylesheet\">"
  , "  <style>"
  , "    * { box-sizing: border-box; margin: 0; padding: 0; }"
  , "    body { font-family: system-ui, -apple-system, sans-serif; max-width: 960px; margin: 0 auto; padding: 1rem; color: #1a1a1a; }"
  , "    h1, h2 { font-family: 'Merriweather', system-ui, -apple-system, sans-serif; }"
  , "    h1 { font-size: 1.5rem; margin-bottom: 0.5rem; }"
  , "    .page-header { display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 0.25rem 1rem; margin-bottom: 1rem; }"
  , "    .page-header h1 { margin-bottom: 0; }"
  , "    .header-status { font-size: 0.8rem; color: #888; text-align: right; line-height: 1.5em; margin-left: auto; }"
  , "    .header-status .status-dot { vertical-align: middle; margin-right: 0.35rem; }"
  , "    .header-status #host-info { color: #666; }"
  , "    .status-dot { display: inline-block; width: 0.5em; height: 0.5em; min-width: 0.5em; min-height: 0.5em; border-radius: 50%; flex-shrink: 0; }"
  , "    .status-dot.ok { background: #4caf50; }"
  , "    .status-dot.warn { background: #ff9800; }"
  , "    h2 { font-size: 1.125rem; margin: 1.5rem 0 0.75rem; }"
  , "    h3 { font-size: 1rem; cursor: pointer; user-select: none; display: flex; align-items: center; gap: 0.4rem; }"
  , "    h3:hover { color: #1565c0; }"
  , "    h3::before { content: ''; display: inline-block; width: 0.6em; height: 0.6em; background: currentColor; clip-path: polygon(0 0, 100% 50%, 0 100%); transition: transform 0.15s ease; transform: rotate(90deg); flex-shrink: 0; }"
  , "    .deploy-section.collapsed h3::before { transform: rotate(0deg); }"
  , "    input[type=search] { width: 100%; padding: 0.5rem 0.75rem; border: 1px solid #ddd; border-radius: 0.375rem; font-size: 0.875rem; margin-bottom: 1rem; }"
  , "    table { width: 100%; border-collapse: collapse; font-size: 0.875rem; margin-bottom: 1rem; }"
  , "    th { text-align: left; padding: 0.5rem 0.75rem; border-bottom: 2px solid #ddd; font-weight: 600; }"
  , "    td { padding: 0.5rem 0.75rem; border-bottom: 1px solid #eee; }"
  , "    tr:hover td { background: #f5f5f5; }"
  , "    .tag { display: inline-block; padding: 0.125rem 0.5rem; border-radius: 999px; font-size: 0.75rem; font-weight: 500; }"
  , "    .tag-bool { background: #e8f5e9; color: #2e7d32; }"
  , "    .tag-num { background: #e3f2fd; color: #1565c0; }"
  , "    .tag-str { background: #fce4ec; color: #c62828; }"
  , "    .tag-deontic { background: #f3e5f5; color: #6a1b9a; }"
  , "    .tag-other { background: #f5f5f5; color: #616161; }"
  , "    .tag-status { font-size: 0.7rem; }"
  , "    .tag-ready { background: #e8f5e9; color: #2e7d32; }"
  , "    .tag-pending { background: #fff3e0; color: #e65100; }"
  , "    .tag-compiling { background: #e3f2fd; color: #1565c0; }"
  , "    .tag-failed { background: #fce4ec; color: #c62828; }"
  , "    .deploy-panel { border: 1px solid #eee; border-radius: 0.5rem; margin-bottom: 1rem; }"
  , "    .deploy-section { padding: 0.75rem 1rem; }"
  , "    .deploy-section + .deploy-section { border-top: 1px solid #eee; }"
  , "    .deploy-section.collapsed table { display: none; }"
  , "    .deploy-section.collapsed .deploy-error { display: none; }"
  , "    .deploy-header { display: flex; justify-content: space-between; align-items: center; }"
  , "    .deploy-meta { font-size: 0.8rem; color: #888; font-weight: 400; }"
  , "    .deploy-error { font-size: 0.8rem; color: #c62828; background: #fce4ec; padding: 0.5rem 0.75rem; border-radius: 0.375rem; margin-top: 0.5rem; white-space: pre-wrap; font-family: monospace; }"
  , "    .info-panel { background: #f8f9fa; border-radius: 0.5rem; }"
  , "    .info { padding: 1rem; }"
  , "    .info + .info { border-top: 1px solid #eaeaea; }"
  , "    .info h3 { margin-top: 0; }"
  , "    .info.collapsed h3::before { transform: rotate(0deg); }"
  , "    .info.collapsed .info-body { display: none; }"
  , "    .info pre { background: #1a1a1a; color: #e0e0e0; padding: 0.75rem; border-radius: 0.375rem; overflow-x: auto; font-size: 0.8rem; margin: 0.5rem 0; }"
  , "    .info .note { font-size: 0.8rem; color: #666; margin-top: 0.5rem; line-height: 1.6; }"
  , "    .info code { background: #e8e8e8; padding: 0.1rem 0.3rem; border-radius: 0.2rem; font-size: 0.8rem; }"
  , "    .info dt { font-weight: 600; margin-top: 0.5rem; font-size: 0.85rem; }"
  , "    .info dd { margin-left: 1rem; font-size: 0.8rem; color: #555; }"
  , "    .btn-compile { display: inline-block; padding: 0.2rem 0.6rem; border: 1px solid #ccc; border-radius: 0.375rem; font-size: 0.75rem; color: #000; background: transparent; cursor: pointer; font-weight: 500; margin-left: auto; }"
  , "    .btn-compile:hover { background: #fff3e0; }"
  , "    .btn-compile:disabled { opacity: 0.5; cursor: not-allowed; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <div class=\"page-header\">"
  , "    <h1>L4 Deployments</h1>"
  , renderStatusBar registry
  , "  </div>"
  , ""
  , renderDeploymentsSection registry
  , ""
  , "  <h2 style=\"margin-left: 0.2em;\">What to do next</h2>"
  , "  <div class=\"info-panel\">"
  -- WebMCP embed
  , "  <div class=\"info collapsed\">"
  , "    <h3>Embed into your website with WebMCP <span id=\"webmcp-pill\" class=\"tag tag-ready\" style=\"display:none;margin-left:auto;font-size:0.7rem;font-family:system-ui,-apple-system,sans-serif;text-align:center;\">WebMCP available in this browser</span></h3>"
  , "    <p class=\"note info-desc\">Add a script tag to any web page so browser AI agents can discover, evaluate, and browse your L4 rules as structured tools.</p>"
  , "    <div class=\"info-body\">"
  , "    <pre id=\"embed-snippet\"></pre>"
  , "    <div class=\"note\">"
  , "      <dl>"
  , "        <dt><code>data-scope</code></dt>"
  , "        <dd>Filter which deployments to register tools for.<br>"
  , "            <code>deployment-id</code> one deployment<br>"
  , "            <code>id1,id2</code> multiple deployments<br>"
  , "            Default: all deployments.</dd>"
  , "        <dt><code>data-tools</code></dt>"
  , "        <dd>Tool registration mode:<br>"
  , "            <code>auto</code> (default) discovery always + direct if &le; 20 functions<br>"
  , "            <code>discovery</code> only search/schema/evaluate meta-tools<br>"
  , "            <code>direct</code> only per-function tools<br>"
  , "            <code>all</code> both, always</dd>"
  , "        <dt><code>data-api-key</code></dt>"
  , "        <dd>Add your API key for <a href=\"https://legalese.cloud\">Legalese Cloud</a> deployments. Needs <strong>l4:rules</strong> for discovery, <strong>l4:evaluate</strong> to execute, <strong>l4:read</strong> for file browsing tools.</dd>"
  , "      </dl>"
  , "    </div>"
  , "    </div>"
  , "  </div>"
  -- MCP
  , "  <div class=\"info collapsed\">"
  , "    <h3>Connect via MCP (Model Context Protocol)</h3>"
  , "    <p class=\"note info-desc\">Use the MCP server endpoint with any MCP-compatible client (Claude Desktop, Cursor, VS Code, etc.).</p>"
  , "    <div class=\"info-body\">"
  , "    <pre id=\"mcp-url\"></pre>"
  , "    <div class=\"note\">"
  , "      <dl>"
  , "        <dt>Org-wide (all deployments)</dt>"
  , "        <dd><code>POST /.mcp</code> &mdash; registers rule evaluation and file browsing tools for all deployments.</dd>"
  , "        <dt>Scoped to one deployment</dt>"
  , "        <dd><code>POST /{deployment-id}/.mcp</code> or <code>POST /deployments/{deployment-id}/.mcp</code></dd>"
  , "        <dt>Authentication</dt>"
  , "        <dd>For <a href=\"https://legalese.cloud\">Legalese Cloud</a> deployments, send your API key as <code>Authorization: Bearer sk_...</code>.<br><strong>l4:rules</strong> to list tools. <strong>l4:evaluate</strong> to call rule evaluation tools. <strong>l4:read</strong> to call file browsing tools.</dd>"
  , "        <dt>Discovery</dt>"
  , "        <dd><code>GET /.well-known/mcp</code> returns the MCP discovery document.</dd>"
  , "      </dl>"
  , "      <p style=\"margin-top: 0.75rem;\"><strong>Example config</strong> (for <code>.claude.json</code>):</p>"
  , "      <pre id=\"mcp-config\"></pre>"
  , "    </div>"
  , "    </div>"
  , "  </div>"
  -- REST API
  , "  <div class=\"info collapsed\">"
  , "    <h3>Using the REST API</h3>"
  , "    <p class=\"note info-desc\">Evaluate deployed L4 rules via HTTP. Browse source files, single and batch evaluation endpoints, OpenAPI metadata with scope filtering.</p>"
  , "    <div class=\"info-body\">"
  , "    <div class=\"note\">"
  , "      <dl>"
  , "        <dt>List functions</dt>"
  , "        <dd><code>GET /{deployment-id}/functions</code><br>"
  , "            <code>GET /deployments/{deployment-id}/functions</code></dd>"
  , "        <dt>Evaluate a function</dt>"
  , "        <dd><code>POST /{deployment-id}/{function-name}/evaluation</code><br>"
  , "            <code>POST /deployments/{deployment-id}/functions/{function-name}/evaluation</code><br>"
  , "            Body: <code>{\"arguments\": {\"field-name\": value, ...}}</code><br>"
  , "            Function names with spaces can use hyphens or URL-encoding (<code>check-person</code>, <code>check%20person</code>).</dd>"
  , "        <dt>Batch evaluation</dt>"
  , "        <dd><code>POST /{deployment-id}/{function-name}/evaluation/batch</code><br>"
  , "            <code>POST /deployments/{deployment-id}/functions/{function-name}/evaluation/batch</code></dd>"
  , "        <dt>Browse source files</dt>"
  , "        <dd><code>GET /deployments/{deployment-id}/files</code> &mdash; list files with content, search by identifier or text.<br>"
  , "            <code>GET /deployments/{deployment-id}/files?identifier={name}</code> &mdash; find definitions and references.<br>"
  , "            <code>GET /deployments/{deployment-id}/files?search={text}</code> &mdash; grep source files.<br>"
  , "            <code>GET /deployments/{deployment-id}/files/{path}.l4</code> &mdash; raw file content.<br>"
  , "            <code>GET /deployments/{deployment-id}/files/{path}.l4?lines=10:20</code> &mdash; line range.</dd>"
  , "        <dt>List deployments with function details</dt>"
  , "        <dd><code>GET /deployments</code> &mdash; all deployments with function names.<br>"
  , "            <code>GET /deployments?functions=full</code> &mdash; include full parameter schemas.<br>"
  , "            <code>GET /deployments?scope={deployment-id}</code> &mdash; filter by deployment.<br>"
  , "            Combine: <code>GET /deployments?functions=full&amp;scope={deployment-id}</code></dd>"
  , "        <dt>OpenAPI 3.0 spec</dt>"
  , "        <dd><code>GET /openapi.json</code> &mdash; org-wide OpenAPI 3.0 spec describing all endpoints.<br>"
  , "            <code>GET /deployments/{deployment-id}/openapi.json</code> &mdash; per-deployment spec.</dd>"
  , "        <dt>Authentication</dt>"
  , "        <dd>For <a href=\"https://legalese.cloud\">Legalese Cloud</a>, send <code>Authorization: Bearer sk_...</code>. Needs <strong>l4:rules</strong> to list, <strong>l4:evaluate</strong> to execute, <strong>l4:read</strong> to browse files.</dd>"
  , "      </dl>"
  , "    </div>"
  , "    </div>"
  , "  </div>"
  -- Deploy
  , "  <div class=\"info collapsed\">"
  , "    <h3>Deploying L4 rules</h3>"
  , "    <p class=\"note info-desc\">Upload zip archives (bundles) of <code>.l4</code> files to create or update deployments.</p>"
  , "    <div class=\"info-body\">"
  , "    <div class=\"note\">"
  , "      <dl>"
  , "        <dt>Create or replace a deployment</dt>"
  , "        <dd><code>POST /deployments</code> &mdash; multipart form with <code>id</code> field and <code>sources</code> zip file.<br>"
  , "            <code>PUT /{deployment-id}</code> &mdash; replace an existing deployment.</dd>"
  , "        <dt>Check deployment status</dt>"
  , "        <dd><code>GET /{deployment-id}</code><br>"
  , "            <code>GET /deployments/{deployment-id}</code><br>"
  , "            Returns status (<code>ready</code>, <code>pending</code>, or <code>failed</code>) and function count.</dd>"
  , "        <dt>Delete a deployment</dt>"
  , "        <dd><code>DELETE /{deployment-id}</code><br>"
  , "            <code>DELETE /deployments/{deployment-id}</code></dd>"
  , "        <dt>Authentication</dt>"
  , "        <dd>For <a href=\"https://legalese.cloud\">Legalese Cloud</a>, send <code>Authorization: Bearer sk_...</code>. Requires <strong>l4:deploy</strong> permission.</dd>"
  , "      </dl>"
  , "      <p style=\"margin-top: 0.75rem;\"><strong>Example</strong> (deploy via curl):</p>"
  , "      <pre id=\"deploy-example\"></pre>"
  , "    </div>"
  , "    </div>"
  , "  </div>"
  , "  </div>"
  , ""
  , "  <script>"
  , "    // Collapsible sections (deployments + info boxes)"
  , "    document.querySelectorAll('.deploy-section h3, .info h3').forEach(function(h3) {"
  , "      h3.addEventListener('click', function() { h3.closest('.deploy-section, .info').classList.toggle('collapsed'); });"
  , "    });"
  , ""
  , "    var BASE_URL = window.location.origin;"
  , "    var hostInfo = document.getElementById('host-info');"
  , "    if (hostInfo) hostInfo.textContent = BASE_URL;"
  , "    document.getElementById('embed-snippet').textContent = '<script src=\"' + BASE_URL + '/.webmcp/embed.js\" data-scope=\"*\" data-tools=\"auto\" data-api-key=\"sk_...\"></' + 'script>';"
  , "    document.getElementById('mcp-url').textContent = BASE_URL + '/.mcp';"
  , "    document.getElementById('mcp-config').textContent = JSON.stringify({ mcpServers: { 'l4-rules': { type: 'http', url: BASE_URL + '/.mcp', headers: { Authorization: 'Bearer sk_...' } } } }, null, 2);"
  , "    document.getElementById('deploy-example').textContent = 'curl -X POST ' + BASE_URL + '/deployments \\\\\\n  -F id=my-rules \\\\\\n  -F sources=@bundle.zip';"
  , ""
  , "    // Search filter across server-rendered deployments"
  , "    document.getElementById('fn-search').addEventListener('input', function(ev) {"
  , "      var q = (ev.target.value || '').toLowerCase();"
  , "      document.querySelectorAll('.deploy-section').forEach(function(sec) {"
  , "        var rows = sec.querySelectorAll('tbody tr');"
  , "        var anyVisible = false;"
  , "        rows.forEach(function(row) {"
  , "          var text = row.textContent.toLowerCase();"
  , "          var show = !q || text.indexOf(q) >= 0;"
  , "          row.style.display = show ? '' : 'none';"
  , "          if (show) anyVisible = true;"
  , "        });"
  , "        // For deployments without function tables (pending/failed), match on deployment id"
  , "        if (rows.length === 0) {"
  , "          anyVisible = !q || sec.textContent.toLowerCase().indexOf(q) >= 0;"
  , "        }"
  , "        sec.style.display = (!q || anyVisible) ? '' : 'none';"
  , "        if (q && anyVisible) sec.classList.remove('collapsed');"
  , "      });"
  , "    });"
  , ""
  , "    function escH(s){return String(s).replace(/&/g,'\\x26amp;').replace(/</g,'\\x26lt;').replace(/>/g,'\\x26gt;').replace(/\\\"/g,'\\x26quot;');}"
  , "    function compileDeployment(btn,deployId){"
  , "      btn.disabled=true;btn.textContent='Compiling\\u2026';"
  , "      var sec=btn.closest('.deploy-section');"
  , "      var tag=sec.querySelector('.tag-pending');"
  , "      if(tag){tag.className='tag tag-status tag-compiling';tag.textContent='compiling';}"
  , "      var meta=sec.querySelector('.deploy-meta');"
  , "      if(meta) meta.textContent='compiling\\u2026';"
  , "      fetch('/'+encodeURIComponent(deployId),{headers:{'Accept':'application/json'}})"
  , "        .then(function(r){return r.json().then(function(d){return{ok:r.ok,data:d};})})"
  , "        .then(function(res){"
  , "          var d=res.data; btn.remove();"
  , "          if(d.status==='ready'&&d.metadata){"
  , "            var t=sec.querySelector('.tag-status');"
  , "            if(t){t.className='tag tag-status tag-ready';t.textContent='ready';}"
  , "            var m=sec.querySelector('.deploy-meta');"
  , "            var fns=d.metadata.functions||[];"
  , "            if(m) m.textContent=fns.length+' functions';"
  , "            var h='';"
  , "            if(fns.length){"
  , "              h+='<table><thead><tr><th>Name</th><th>Return</th><th>Description</th></tr></thead><tbody>';"
  , "              fns.forEach(function(fn){"
  , "                var rt=(fn.returnType||'').toUpperCase();"
  , "                var cls=rt==='BOOLEAN'?'tag-bool':rt==='NUMBER'?'tag-num':rt==='STRING'?'tag-str':rt==='DEONTIC'?'tag-deontic':'tag-other';"
  , "                h+='<tr><td>'+escH(fn.name)+'</td><td><span class=\\\"tag '+cls+'\\\">'+escH(rt)+'</span></td><td>'+escH(fn.description||'')+'</td></tr>';"
  , "              });"
  , "              h+='</tbody></table>';"
  , "            }"
  , "            var files=d.metadata.files||[];"
  , "            if(files.length){"
  , "              h+='<table><thead><tr><th>File</th><th>Exports</th></tr></thead><tbody>';"
  , "              files.forEach(function(f){"
  , "                h+='<tr><td><a href=\\\"'+escH(f.path)+'\\\">'+escH(f.path)+'</a></td><td>'+escH((f.exports||[]).join(', '))+'</td></tr>';"
  , "              });"
  , "              h+='</tbody></table>';"
  , "            }"
  , "            sec.querySelector('.deploy-header').insertAdjacentHTML('afterend',h);"
  , "            sec.classList.remove('collapsed');"
  , "            var dot=document.querySelector('.header-status .status-dot');if(dot){dot.className='status-dot ok';}"
  , "          }else if(d.status==='failed'){"
  , "            var t2=sec.querySelector('.tag-status');"
  , "            if(t2){t2.className='tag tag-status tag-failed';t2.textContent='failed';}"
  , "            var m2=sec.querySelector('.deploy-meta');"
  , "            if(m2) m2.textContent='compilation error';"
  , "            sec.querySelector('.deploy-header').insertAdjacentHTML('afterend','<div class=\\\"deploy-error\\\">'+escH(d.error||'Compilation failed')+'</div>');"
  , "            sec.classList.remove('collapsed');"
  , "          }"
  , "        })"
  , "        .catch(function(){btn.textContent='Error';var m3=sec.querySelector('.deploy-meta');if(m3) m3.textContent='request failed';});"
  , "    }"
  , ""
  , "    // WebMCP: show pill when embed.js reports successful tool registration"
  , "    window.addEventListener('webmcp-ready', function(e) {"
  , "      if (e.detail.webmcpRegistered) {"
  , "        var pill = document.getElementById('webmcp-pill');"
  , "        if (pill) pill.style.display = '';"
  , "      }"
  , "    });"
  , "  </script>"
  , "  <script src=\"/.webmcp/embed.js\"></script>"
  , "</body>"
  , "</html>"
  ]

-- | Render the status bar summarizing deployment counts.
renderStatusBar :: Map.Map DeploymentId DeploymentState -> Text
renderStatusBar registry
  | Map.null registry = ""
  | otherwise =
      "    <div class=\"header-status\"><div id=\"host-info\"></div><span class=\"status-dot " <> dotClass <> "\"></span>"
        <> showT totalDeps <> " deployments</div>"
  where
    entries = Map.toAscList registry
    totalDeps = length entries
    hasReady = any isReady (map snd entries)
    dotClass
      | hasReady = "ok"
      | otherwise = "warn"
    isReady (DeploymentReady _ _) = True
    isReady _ = False

-- | Render the deployments section with search and all deployment cards.
renderDeploymentsSection :: Map.Map DeploymentId DeploymentState -> Text
renderDeploymentsSection registry
  | Map.null registry = Text.unlines
      [ "  <div style=\"padding: 1rem 1rem;\">"
      , "    <p style=\"color: #666; font-size: 0.95rem; margin-bottom: 0.25rem;\">No deployments found.</p>"
      , "    <p style=\"color: #999; font-size: 0.8rem;\">Upload an L4 bundle via <code style=\"background:#e8e8e8;padding:0.1rem 0.3rem;border-radius:0.2rem;font-size:0.8rem;\">POST /deployments</code> to get started.</p>"
      , "  </div>"
      ]
  | otherwise = Text.unlines
      [ "  <div id=\"deployments-section\">"
      , "    <input type=\"search\" id=\"fn-search\" placeholder=\"Search across all functions...\" />"
      , "    <div id=\"deploy-list\" class=\"deploy-panel\">"
      , Text.unlines (map (uncurry renderDeployment) (Map.toAscList registry))
      , "    </div>"
      , "  </div>"
      ]

-- | Render a single deployment card.
renderDeployment :: DeploymentId -> DeploymentState -> Text
renderDeployment (DeploymentId did) state = Text.unlines $
  [ "      <div class=\"deploy-section collapsed\" data-deployment-id=\"" <> esc did <> "\">"
  , "        <div class=\"deploy-header\"><h3>"
      <> esc did <> " "
      <> statusTag <> " "
      <> "<span class=\"deploy-meta\">" <> metaText <> "</span>"
      <> "</h3>"
      <> compileButton
      <> "</div>"
  ] ++ bodyLines ++
  [ "      </div>"
  ]
  where
    (statusLabel, statusClass) = case state of
      DeploymentPending _  -> ("pending",   "tag-pending")
      DeploymentCompiling  -> ("compiling", "tag-compiling")
      DeploymentReady _ _  -> ("ready",     "tag-ready")
      DeploymentFailed _   -> ("failed",    "tag-failed")

    statusTag = "<span class=\"tag tag-status " <> statusClass <> "\">" <> statusLabel <> "</span>"

    compileButton = case state of
      DeploymentPending _ -> "<button class=\"btn-compile\" onclick=\"compileDeployment(this, '" <> escAttr did <> "'); event.stopPropagation();\">Compile now</button>"
      _ -> ""

    metaText = case state of
      DeploymentReady _ meta -> showT (length (meta.metaFunctions)) <> " functions"
      DeploymentPending (Just meta) -> showT (length (meta.metaFunctions)) <> " functions"
      DeploymentPending Nothing -> "not yet compiled"
      DeploymentFailed _     -> "compilation error"
      DeploymentCompiling    -> "compiling..."

    bodyLines = case state of
      DeploymentReady _ meta -> renderMetaTables meta
      DeploymentPending (Just meta) -> renderMetaTables meta
      DeploymentFailed err ->
        [ "        <div class=\"deploy-error\">" <> esc err <> "</div>" ]
      _ -> []

-- | Render functions and files tables from deployment metadata.
renderMetaTables :: DeploymentMetadata -> [Text]
renderMetaTables meta =
  [ "        <table><thead><tr><th>Name</th><th>Return</th><th>Description</th></tr></thead><tbody>"
  , Text.unlines (map renderFunctionRow (meta.metaFunctions))
  , "        </tbody></table>"
  ] ++ if null (meta.metaFiles) then [] else
  [ "        <table><thead><tr><th>File</th><th>Exports</th></tr></thead><tbody>"
  , Text.unlines (map renderFileRow (meta.metaFiles))
  , "        </tbody></table>"
  ]

-- | Render a single function row in the deployment table.
renderFunctionRow :: FunctionSummary -> Text
renderFunctionRow fs =
  "          <tr><td>" <> esc fs.fsName
  <> "</td><td><span class=\"tag " <> tagClass rt <> "\">" <> esc rt <> "</span></td><td>"
  <> esc fs.fsDescription <> "</td></tr>"
  where
    rt = Text.toUpper fs.fsReturnType

-- | Render a single file row in the files table.
renderFileRow :: FileEntry -> Text
renderFileRow fe =
  "          <tr><td><a href=\"" <> esc fe.fePath <> "\">" <> esc fe.fePath <> "</a></td><td>"
  <> esc (Text.intercalate ", " fe.feExports)
  <> "</td></tr>"

-- | Map return type to CSS class.
tagClass :: Text -> Text
tagClass rt
  | rt == "BOOLEAN" = "tag-bool"
  | rt == "NUMBER"  = "tag-num"
  | rt == "STRING"  = "tag-str"
  | rt == "DEONTIC" = "tag-deontic"
  | otherwise       = "tag-other"

-- | HTML-escape text.
esc :: Text -> Text
esc = Text.replace "&" "&amp;"
    . Text.replace "<" "&lt;"
    . Text.replace ">" "&gt;"
    . Text.replace "\"" "&quot;"

-- | Escape text for use inside a JS string literal within an HTML attribute.
escAttr :: Text -> Text
escAttr = Text.replace "\\" "\\\\"
        . Text.replace "'" "\\'"
        . Text.replace "&" "&amp;"
        . Text.replace "<" "&lt;"
        . Text.replace ">" "&gt;"
        . Text.replace "\"" "&quot;"

-- | Show an Int as Text.
showT :: Int -> Text
showT = Text.pack . show
