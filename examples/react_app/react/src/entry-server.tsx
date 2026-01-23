/**
 * Server Entry Point (Level 3: Full SSR)
 *
 * This file runs in Node.js, receiving JSON-RPC requests from Kirin
 * and returning rendered HTML.
 *
 * Protocol: JSON-RPC 2.0 over stdio
 * Request:  {"jsonrpc":"2.0","id":1,"method":"render","params":{"url":"/","props":{}}}
 * Response: {"jsonrpc":"2.0","id":1,"result":{"html":"..."}}
 */

import { renderToString } from 'react-dom/server';
import { createInterface } from 'readline';
import App from './App';

interface RenderRequest {
  jsonrpc: string;
  id: number;
  method: string;
  params: {
    url: string;
    props?: Record<string, unknown>;
  };
}

interface RenderResponse {
  jsonrpc: string;
  id: number;
  result?: { html: string };
  error?: { code: number; message: string };
}

// Request counter for graceful restart
let requestCount = 0;
const MAX_REQUESTS = 5000;

// Read JSON-RPC requests from stdin
const rl = createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
});

rl.on('line', (line: string) => {
  requestCount++;

  try {
    const request: RenderRequest = JSON.parse(line);

    if (request.method === 'render') {
      const { url, props = {} } = request.params;

      // Render React component to string
      const html = renderToString(<App {...props} />);

      const response: RenderResponse = {
        jsonrpc: '2.0',
        id: request.id,
        result: { html },
      };

      console.log(JSON.stringify(response));
    } else if (request.method === 'health') {
      // Health check
      const response: RenderResponse = {
        jsonrpc: '2.0',
        id: request.id,
        result: {
          html: JSON.stringify({
            status: 'ok',
            requests: requestCount,
            memory: process.memoryUsage().heapUsed,
          }),
        },
      };
      console.log(JSON.stringify(response));
    } else {
      // Unknown method
      const response: RenderResponse = {
        jsonrpc: '2.0',
        id: request.id,
        error: { code: -32601, message: `Method not found: ${request.method}` },
      };
      console.log(JSON.stringify(response));
    }
  } catch (error) {
    // Parse error
    const response: RenderResponse = {
      jsonrpc: '2.0',
      id: 0,
      error: {
        code: -32700,
        message: error instanceof Error ? error.message : 'Parse error',
      },
    };
    console.log(JSON.stringify(response));
  }

  // Graceful restart after MAX_REQUESTS
  if (requestCount >= MAX_REQUESTS) {
    process.exit(0);
  }
});

// GC hint for long-running process
if (global.gc) {
  setInterval(() => global.gc!(), 60000);
}

// Log startup
console.error(`[Kirin SSR Worker] Started (PID: ${process.pid})`);
