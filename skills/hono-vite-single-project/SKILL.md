---
name: hono-vite-single-project
description: Instructions on configuring Hono with Vite in a single project (SPA frontend + API backend dev server proxying) using @hono/vite-dev-server and static asset serving in production.
---

# Hono + Vite Single Project Integration Guide

This skill documents how to design, configure, and bundle a single unified codebase containing a Hono API server (Backend) and a Vite SPA (Frontend, such as Vue or React).

---

## 1. Folder Structure

A clean layout isolates backend code from frontend client files while retaining a single `package.json` for dependencies:

```
├── package.json
├── vite.config.ts            # Root config orchestrating both dev servers
├── index.html                # Frontend entrypoint html
├── server/                   # Backend folder
│   ├── server.ts             # Hono app instantiation & routes
│   ├── prod.ts               # Node production server entrypoint
│   └── tsconfig.json         # TS config for node backend environment
└── web/                      # Frontend folder
    ├── main.ts               # Vue/React client entrypoint
    ├── App.vue               # SPA root view
    ├── api.ts                # Type-safe Hono Client instantiation
    └── tsconfig.json         # TS config for browser environment
```

---

## 2. Dev Server Configuration (`vite.config.ts`)

Use `@hono/vite-dev-server` to run your Hono backend within Vite's development loop. Ensure you exclude client-side assets and routing patterns from the Hono handler pipeline so that Vite serves them instead:

```typescript
import { defineConfig } from "vite-plus";
import vue from "@vitejs/plugin-vue";
import devServer, { defaultOptions } from "@hono/vite-dev-server";

export default defineConfig({
  plugins: [
    vue(),
    devServer({
      entry: "server/server.ts", // Path to Hono app entry point
      exclude: [
        /^\/$/,                  // Let Vite serve index.html
        /^\/index\.html$/,
        /.*\.vue$/,              // Exclude client-side files
        "/web/**",               // Exclude frontend folder
        ...defaultOptions.exclude
      ],
    }),
  ],
  server: {
    port: 3000,
    strictPort: true,
  },
});
```

---

## 3. Production Environment & Asset Serving

In production, Hono needs to serve static frontend assets compiled by Vite.

### Production Entrypoint (`server/prod.ts`)
Create a production server utilizing `@hono/node-server/serve-static` to serve pre-built files from `dist/`:

```typescript
import { serve } from "@hono/node-server";
import { serveStatic } from "@hono/node-server/serve-static";
import app from "./server";

// 1. Serve static files from 'dist' directory (frontend production build output)
app.use("/*", serveStatic({ root: "./dist" }));

// 2. Fallback to index.html for SPA client-side routing
app.get("*", serveStatic({ path: "./dist/index.html" }));

const port = Number(process.env.PORT) || 3000;
console.log(`Production server running on http://localhost:${port}`);

serve({
  fetch: app.fetch,
  port,
});
```

### Production Build Script (`package.json`)
The build process first type-checks both frontend and backend directories, builds client-side assets with Vite, and bundles the server code with `vp pack`:

```json
{
  "scripts": {
    "dev": "vp dev",
    "build": "tsc -p web/tsconfig.json && tsc -p server/tsconfig.json && vp build && vp pack server/prod.ts --out-dir dist-server --clean",
    "start": "node dist-server/prod.mjs"
  }
}
```

---

## 4. Type-Safe Hono RPC Client

One of Hono's main strengths is type-safe RPC. By exporting the routing type from the server, the client can infer full route parameters, query shapes, payloads, and response JSON formats.

### Server Export (`server/server.ts`)
```typescript
import { Hono } from "hono";
const app = new Hono();

const routes = app
  .get("/api/hello", (c) => c.json({ message: "Hello!" }));

// Export the type definition of the complete routes
export type AppType = typeof routes;
export default app;
```

### Client Import (`web/api.ts`)
```typescript
import { hc } from "hono/client";
import type { AppType } from "../server/server";

// Instantiate the type-safe client pointing to base URL
export const api = hc<AppType>("/");
```

### Usage in SPA (`web/App.vue`)
```typescript
import { api } from "./api";

async function checkStatus() {
  const res = await api.api.hello.$get();
  if (res.ok) {
    const data = await res.json();
    console.log(data.message); // Fully typed, no 'any' types
  }
}
```
