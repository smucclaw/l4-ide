#!/usr/bin/env node

/**
 * L4 Cloud Validation Script
 *
 * Validates L4 code using the cloud LSP service at jl4.legalese.com
 * This allows validation without requiring local jl4-cli installation.
 *
 * Usage:
 *   node validate-cloud.mjs <file.l4>
 *   node validate-cloud.mjs --url wss://custom-host.com/lsp <file.l4>
 */

import WebSocket from "ws";
import { readFileSync } from "fs";
import { resolve, basename } from "path";

// Configuration
const DEFAULT_LSP_URL = "wss://jl4.legalese.com/lsp";
const TIMEOUT_MS = 30000;

// Parse command line arguments
const args = process.argv.slice(2);
let lspUrl = DEFAULT_LSP_URL;
let filePath = null;
let debug = false;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--url" && i + 1 < args.length) {
    lspUrl = args[++i];
  } else if (args[i] === "--debug") {
    debug = true;
  } else if (!filePath) {
    filePath = args[i];
  }
}

if (!filePath) {
  console.error("Usage: node validate-cloud.mjs [--url <wss://...>] <file.l4>");
  process.exit(1);
}

// Read the L4 file
const absolutePath = resolve(filePath);
const fileName = basename(absolutePath);
let fileContent;

try {
  fileContent = readFileSync(absolutePath, "utf-8");
} catch (error) {
  console.error(`Error reading file: ${error.message}`);
  process.exit(1);
}

console.log(`Validating: ${fileName}`);
console.log(`Connecting to: ${lspUrl}\n`);

// LSP Message utilities
let messageId = 0;

function createMessage(method, params) {
  return {
    jsonrpc: "2.0",
    id: ++messageId,
    method,
    params,
  };
}

function createNotification(method, params) {
  return {
    jsonrpc: "2.0",
    method,
    params,
  };
}

function sendMessage(ws, message) {
  if (debug) {
    console.log(
      "\n>>> Sending:",
      message.method || `response to ${message.id}`,
    );
    console.log(JSON.stringify(message, null, 2));
  }

  // Send JSON-RPC directly as WebSocket frame (no Content-Length header)
  ws.send(JSON.stringify(message));
}

// Main validation logic
const ws = new WebSocket(lspUrl);
const diagnostics = [];
let initialized = false;
let timeout = null;

// Set overall timeout
timeout = setTimeout(() => {
  console.error("\n❌ Timeout waiting for LSP response");
  ws.close();
  process.exit(1);
}, TIMEOUT_MS);

ws.on("open", () => {
  console.log("✓ Connected to LSP server");

  // Send initialize request
  const initializeParams = {
    processId: process.pid,
    clientInfo: {
      name: "l4-skill-validator",
      version: "1.0.0",
    },
    rootUri: null, // Don't send local path to remote server
    workspaceFolders: null,
    capabilities: {
      textDocument: {
        synchronization: {
          didOpen: true,
          didChange: true,
          didSave: true,
          didClose: true,
        },
        publishDiagnostics: {
          relatedInformation: true,
          versionSupport: true,
          codeDescriptionSupport: true,
          dataSupport: true,
        },
      },
    },
  };

  sendMessage(ws, createMessage("initialize", initializeParams));
});

ws.on("message", (data) => {
  if (debug) {
    console.log("\n<<< Received raw data:", data.toString().substring(0, 200));
  }

  const messages = parseMessages(data.toString());

  if (debug && messages.length > 0) {
    console.log(`\n<<< Parsed ${messages.length} message(s)`);
  }

  messages.forEach((message) => {
    if (debug) {
      console.log(JSON.stringify(message, null, 2));
    }
    // Handle initialize response
    if (message.id === 1 && message.result) {
      console.log("✓ LSP server initialized");
      initialized = true;

      // Send initialized notification
      sendMessage(ws, createNotification("initialized", {}));

      // Open the document
      const documentUri = `file://${absolutePath}`;
      const didOpenParams = {
        textDocument: {
          uri: documentUri,
          languageId: "jl4",
          version: 1,
          text: fileContent,
        },
      };

      sendMessage(
        ws,
        createNotification("textDocument/didOpen", didOpenParams),
      );
      console.log("✓ Document opened\n");

      // Give the server a moment to process and send diagnostics
      setTimeout(() => {
        displayResults();
        clearTimeout(timeout);
        ws.close();
      }, 2000);
    }

    // Handle diagnostics
    if (message.method === "textDocument/publishDiagnostics") {
      const diags = message.params.diagnostics || [];
      diagnostics.push(...diags);
    }

    // Handle errors
    if (message.error) {
      console.error("LSP Error:", message.error);
    }
  });
});

ws.on("error", (error) => {
  console.error(`\n❌ WebSocket error: ${error.message}`);
  clearTimeout(timeout);
  process.exit(1);
});

ws.on("close", () => {
  console.log("\n✓ Connection closed");
});

// Parse JSON-RPC messages from WebSocket frames
// vscode-ws-jsonrpc sends JSON directly, not with Content-Length headers
function parseMessages(data) {
  try {
    const message = JSON.parse(data.toString());
    return [message];
  } catch (e) {
    console.error("Failed to parse message:", e.message);
    return [];
  }
}

// Display validation results
function displayResults() {
  console.log("=".repeat(60));

  if (diagnostics.length === 0) {
    console.log("✅ Checking succeeded.");
    console.log("\nNo errors or warnings found.");
    process.exit(0);
  }

  // Group diagnostics by severity
  const errors = diagnostics.filter((d) => d.severity === 1);
  const warnings = diagnostics.filter((d) => d.severity === 2);
  const info = diagnostics.filter((d) => d.severity === 3);

  console.log(`Found ${diagnostics.length} diagnostic(s):`);
  console.log(`  Errors: ${errors.length}`);
  console.log(`  Warnings: ${warnings.length}`);
  console.log(`  Info: ${info.length}`);
  console.log();

  // Display each diagnostic
  diagnostics.forEach((diag, index) => {
    const severityLabel =
      {
        1: "❌ Error",
        2: "⚠️  Warning",
        3: "ℹ️  Info",
        4: "💡 Hint",
      }[diag.severity] || "Message";

    const start = diag.range.start;
    const location = `${fileName}:${start.line + 1}:${start.character + 1}`;

    console.log(`${severityLabel} @ ${location}`);
    console.log(`  ${diag.message}`);
    if (diag.source) {
      console.log(`  Source: ${diag.source}`);
    }
    console.log();
  });

  console.log("=".repeat(60));

  // Exit with error code if there are errors
  process.exit(errors.length > 0 ? 1 : 0);
}
