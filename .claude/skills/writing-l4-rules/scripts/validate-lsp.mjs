#!/usr/bin/env node
// L4 validation via jl4-lsp stdio.
//
// Usage:  validate-lsp.mjs <file.l4> [file2.l4 ...]
//
// Spawns jl4-lsp (which must be on PATH or installed alongside the VSCode
// extension), drives a minimal LSP session, and prints all diagnostics —
// including the #EVAL/#ASSERT results that the language server reports as
// Information-severity diagnostics. Exit code: 0 if no errors, 1 otherwise.
//
// Prefer this to jl4-cli when your environment ships the VSCode extension
// but not the separate jl4-cli binary.

import { spawn } from "node:child_process";
import { readFileSync, statSync } from "node:fs";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

const files = process.argv.slice(2);
if (files.length === 0) {
  console.error("Usage: validate-lsp.mjs <file.l4> [file2.l4 ...]");
  process.exit(2);
}

for (const f of files) {
  try {
    statSync(f);
  } catch (e) {
    console.error(`file not found: ${f}`);
    process.exit(2);
  }
}

const lsp = spawn("jl4-lsp", [], { stdio: ["pipe", "pipe", "inherit"] });

let buffer = Buffer.alloc(0);
const pending = new Map(); // id -> {resolve, reject}
const diagnostics = new Map(); // uri -> diagnostics[]

lsp.stdout.on("data", (chunk) => {
  buffer = Buffer.concat([buffer, chunk]);
  while (true) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd === -1) break;
    const header = buffer.slice(0, headerEnd).toString("ascii");
    const m = header.match(/Content-Length:\s*(\d+)/i);
    if (!m) {
      console.error("LSP framing error — no Content-Length");
      process.exit(3);
    }
    const len = parseInt(m[1], 10);
    const bodyStart = headerEnd + 4;
    if (buffer.length < bodyStart + len) break;
    const body = buffer.slice(bodyStart, bodyStart + len).toString("utf8");
    buffer = buffer.slice(bodyStart + len);
    let msg;
    try {
      msg = JSON.parse(body);
    } catch (e) {
      console.error("LSP JSON parse error:", e.message);
      continue;
    }
    handleMessage(msg);
  }
});

lsp.on("close", (code) => {
  if (code !== 0 && code !== null) {
    console.error(`jl4-lsp exited with code ${code}`);
    process.exit(code);
  }
});

let nextId = 1;
function send(method, params, isRequest = false) {
  const msg = { jsonrpc: "2.0", method, params };
  if (isRequest) {
    msg.id = nextId++;
    return new Promise((resolve, reject) => {
      pending.set(msg.id, { resolve, reject });
      writeMessage(msg);
    });
  }
  writeMessage(msg);
}

function writeMessage(msg) {
  const body = JSON.stringify(msg);
  const header = `Content-Length: ${Buffer.byteLength(body, "utf8")}\r\n\r\n`;
  lsp.stdin.write(header + body, "utf8");
}

function handleMessage(msg) {
  if (msg.id !== undefined && pending.has(msg.id)) {
    const { resolve, reject } = pending.get(msg.id);
    pending.delete(msg.id);
    if (msg.error) reject(msg.error);
    else resolve(msg.result);
    return;
  }
  if (msg.method === "textDocument/publishDiagnostics") {
    const { uri, diagnostics: ds } = msg.params;
    diagnostics.set(uri, ds);
  }
  // Ignore window/logMessage, $/progress, registrations, etc.
}

function severityName(n) {
  return { 1: "Error", 2: "Warning", 3: "Info", 4: "Hint" }[n] || String(n);
}

async function main() {
  const rootPath = resolve(".");
  const rootUri = pathToFileURL(rootPath).toString();

  await send(
    "initialize",
    {
      processId: process.pid,
      rootUri,
      workspaceFolders: [{ uri: rootUri, name: "root" }],
      capabilities: {
        textDocument: {
          publishDiagnostics: { versionSupport: false },
          synchronization: {
            didSave: true,
            willSave: false,
            willSaveWaitUntil: false,
            dynamicRegistration: false,
          },
        },
        workspace: {
          workspaceFolders: true,
          didChangeConfiguration: { dynamicRegistration: false },
        },
      },
    },
    true,
  );
  send("initialized", {});

  for (const f of files) {
    const abs = resolve(f);
    const uri = pathToFileURL(abs).toString();
    const text = readFileSync(abs, "utf8");
    send("textDocument/didOpen", {
      textDocument: { uri, languageId: "l4", version: 1, text },
    });
  }

  // Wait for diagnostics for each requested file. jl4-lsp publishes them
  // after compilation. We poll with a short timeout.
  const wantedUris = files.map((f) => pathToFileURL(resolve(f)).toString());
  const deadline = Date.now() + 30000;
  while (Date.now() < deadline) {
    if (wantedUris.every((u) => diagnostics.has(u))) break;
    await new Promise((r) => setTimeout(r, 100));
  }

  let errorCount = 0;
  for (const uri of wantedUris) {
    const ds = diagnostics.get(uri) || [];
    console.log(`\n── ${uri} ──`);
    if (ds.length === 0) {
      console.log("  (no diagnostics — checking succeeded)");
      continue;
    }
    for (const d of ds) {
      const { start } = d.range;
      const sev = severityName(d.severity);
      console.log(
        `  ${sev} ${start.line + 1}:${start.character + 1}  ${d.message}`,
      );
      if (d.severity === 1) errorCount++;
    }
  }

  try {
    await send("shutdown", null, true);
  } catch {}
  send("exit", null);
  lsp.stdin.end();

  setTimeout(() => {
    process.exit(errorCount > 0 ? 1 : 0);
  }, 200);
}

main().catch((e) => {
  console.error("validate-lsp failed:", e);
  process.exit(3);
});
