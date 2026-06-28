---
name: git-subtree-reference
description: Guide on embedding third-party reference repositories as read-only reference material using Git subtrees, including IDE configuration, Vite+ oxlint/oxfmt exclusion, and agent instructions.
---

# Git Subtree Reference Repositories (Vendoring Guide)

Making external software source code directly accessible within your repository dramatically improves the quality of coding agents. Instead of guessing API usages or reading sparse markdown docs, the agent can traverse actual implementations, unit tests, and types.

This guide details how to embed reference codebases under `repos/` as a Git subtree and configure tools to exclude them from formatting, linting, and user autocomplete.

---

## 1. Embedding a Subtree

Using `git subtree` nests one repository inside another as a subfolder. Unlike submodules, subtrees do not require special init/update steps on checkout and track the files directly.

### Adding a reference repository
Run the following command at the project root:

```bash
git subtree add \
  --prefix=repos/<name> \
  <repo_git_url> \
  <branch> \
  --squash
```

> [!IMPORTANT]
> Always use the `--squash` flag. Without it, you will import the entire commit history of the external repository (often thousands of commits) into your project's Git log.

*Example:*
```bash
git subtree add \
  --prefix=repos/effect-smol \
  https://github.com/Effect-TS/effect-smol.git \
  main \
  --squash
```

### Pulling updates
When the upstream repository updates, pull changes with:

```bash
git subtree pull \
  --prefix=repos/<name> \
  <repo_git_url> \
  <branch> \
  --squash
```

---

## 2. Formatting & Linting Configuration (Vite+)

We do not want formatting (`oxfmt`) or linting (`oxlint`) checks to run on or fail due to vendored files. Configure your `vite.config.ts` to ignore the `repos/` path:

```typescript
import { defineConfig } from "vite-plus";

export default defineConfig({
  lint: {
    ignorePatterns: ["repos/**"], // Prevent linting reference files
  },
  fmt: {
    ignorePatterns: ["repos/**"], // Prevent formatting reference files
  }
});
```

---

## 3. Editor Configuration (VSCode)

To prevent your IDE from suggesting auto-imports or polluting search results with code from reference subtrees, configure `.vscode/settings.json` at the root of the project:

```json
{
  "typescript.preferences.autoImportFileExcludePatterns": [
    "repos/**"
  ],
  "javascript.preferences.autoImportFileExcludePatterns": [
    "repos/**"
  ],
  "files.exclude": {
    "repos/**": true
  },
  "files.watcherExclude": {
    "repos/**": true
  },
  "search.exclude": {
    "repos/**": true
  }
}
```

---

## 4. Agent Integration Guide (`AGENTS.md`)

Document the vendored repositories explicitly for your coding agent. Add a section to `AGENTS.md` (or your instructions document) guiding the agent on how to use them:

```markdown
## Reference Repositories

This project vendors external codebases under `repos/` for reference:
- `@repos/effect-smol`: Source of truth for Effect v4 API, modules, tests, and code patterns.

### Usage Instructions
1. Use files under `repos/` as read-only reference material.
2. Under no circumstances should you edit files inside `repos/` unless explicitly requested.
3. Do not import from paths under `repos/`. Application code must continue using normal package manager dependencies (e.g. `import { Effect } from "effect"`).
4. Rely on these reference codebases for idiomatic patterns rather than making guesses or executing web searches.
```
