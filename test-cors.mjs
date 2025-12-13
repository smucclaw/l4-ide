#!/usr/bin/env node
// Test CORS on websessions endpoint

const testUrl = "http://localhost:8002/";

console.log("Testing CORS on websessions endpoint...\n");

try {
  // Test POST request
  const response = await fetch(testUrl, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Origin: "http://localhost:5173",
    },
    body: JSON.stringify({ test: "data", source: "test" }),
  });

  const corsHeaders = {
    "access-control-allow-origin": response.headers.get(
      "access-control-allow-origin",
    ),
    "access-control-allow-methods": response.headers.get(
      "access-control-allow-methods",
    ),
    "access-control-allow-headers": response.headers.get(
      "access-control-allow-headers",
    ),
  };

  console.log("Response status:", response.status, response.statusText);
  console.log("CORS headers:", corsHeaders);

  if (!corsHeaders["access-control-allow-origin"]) {
    console.log(
      "\n❌ CORS NOT CONFIGURED - Frontend will be blocked by browser",
    );
  } else {
    console.log("\n✅ CORS configured correctly");
  }
} catch (error) {
  console.error("Error:", error.message);
}
