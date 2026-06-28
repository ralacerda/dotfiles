---
name: vite-plus-toolchain
description: A complete guide on working with Vite+ (vp), the unified toolchain for the web, including single project layouts, workspaces/monorepos, CLI commands, task runners, and environment doctoring.
---

# Vite+ Toolchain (vp) CLI Guide

`Vite+` is a unified toolchain built on top of modern frontend and runtime utilities, including Vite, Rolldown, Vitest, tsdown, Oxlint, Oxfmt, and Vite Task. It provides a single CLI `vp` to manage both backend and frontend development tasks.

---

## 1. What is Vite+ (`vp`)?

Vite+ wraps your workspace toolchains. Instead of managing separate tools for linting, formatting, dev serving, type-checking, and packing, `vp` acts as the single orchestrator.

Key design points:
- **Fast Linting/Formatting**: Integrates `Oxlint` and `Oxfmt` (rust-based, extremely fast).
- **Unified Build/Dev Server**: Runs Vite dev servers for backend Hono + frontend Vue/React.
- **Backend Packing**: Bundles Node.js backend files using `vp pack` with Rolldown/tsdown.

---

## 2. CLI Command Cheat Sheet

| Command | Description | Example |
| :--- | :--- | :--- |
| `vp dev` | Starts the unified client/server dev server. | `vp dev` |
| `vp check` | Runs linters, formatters, typecheckers. | `vp check` |
| `vp check --fix` | Formats and fixes lints on staged or project files. | `vp check --fix` |
| `vp build` | Compiles client frontend assets to production. | `vp build` |
| `vp pack <entry>` | Packs server code into a single production bundle. | `vp pack server/prod.ts --out-dir dist-server --clean` |
| `vp env doctor` | Diagnoses runtime Node.js/package manager health. | `vp env doctor` |
| `vp config` | Configures client git hooks and project settings. | `vp config --hooks-dir .vite-hooks` |

---

## 3. Project Configuration Models

### Single Project Layout
In a single project combining frontend and backend (e.g. Hono API + Vue SPA), `vp` is typically configured at the root.
- The `tsconfig.json` files are separate for frontend (e.g., `web/tsconfig.json`) and backend (`server/tsconfig.json`).
- `vite.config.ts` configures the backend Hono dev server via a plugin.

### Workspace/Monorepo Configuration
In a monorepo setup:
- `pnpm-workspace.yaml` handles packages isolation.
- Root `package.json` delegates commands.
- `vp` can be run in workspace-aware modes or scoped using standard package-manager filters (e.g., `pnpm --filter server run build`).

### Pre-commit Hooks Integration
In `vite.config.ts`, you can define a `staged` section that executes tasks dynamically when Git commits are made:
```typescript
export default defineConfig({
  staged: {
    "web/**/*": "vp check --fix",
    "server/**/*": "vp check --fix",
  },
  lint: {
    ignorePatterns: ["repos/**"], // Exclude third-party/vendored code
    rules: {
      "require-yield": "off", // Crucial when using Generator functions in Effect
    },
  },
  fmt: {
    ignorePatterns: ["repos/**"],
  }
});
```

---

## 4. Diagnosing Environment Quirks

If runtime behaviors look wrong or package managers conflict:
1. Run `vp env doctor` to print environment facts.
2. Check `.vite-plus` folder inside user's home folder for global caches.
