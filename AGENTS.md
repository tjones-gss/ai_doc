# Repository Guidelines

## Project Structure & Module Organization

- `docs/` holds all published content; each Markdown file needs front matter for sidebar placement and `last_updated`.
- `src/` contains React/TSX customizations (theme overrides, components) while `static/` serves unprocessed assets such as logos or downloads.
- Automation resides under `scripts/` (link/frontmatter/structure validators); tests live in `tests/`, and build artifacts land in `build/`.
- Configuration touchpoints: `docusaurus.config.js` (nav, metadata), `sidebars.js`, `playwright.config.js`, and `tsconfig.json`.

## Build, Test, and Development Commands

- Install/update deps with `npm install`.
- Local authoring: `npm start` launches Docusaurus with hot reload at `localhost:3000`.
- Production prep: `npm run build` emits static assets, and `npm run serve` verifies the build locally.
- Quality gates: `npm run test:all` chains build, link, Markdown, frontmatter, and e2e checks; run subsets such as `npm run test:links` or `npm run test:e2e` during iteration.
- Lint/format via `npm run lint`, `npm run lint:fix`, and `npm run format:check`; run `npm run validate` before committing.

## Coding Style & Naming Conventions

- Follow Prettier defaults (2-space indent, single quotes in JS/TS, trailing commas) and keep Markdown wrapped at ~100 characters.
- React/TS files should use PascalCase component names and camelCase hooks/utilities; group related JSX, CSS, and assets within feature folders under `src/`.
- Markdown filenames stay kebab-case (e.g., `prompt-library.md`) and include descriptive headings that mirror sidebar labels.
- ESLint + @typescript-eslint enforce typing rules; do not disable lint rules without justification in the PR.

## Testing Guidelines

- Playwright specs under `tests/` follow the `*.spec.ts` pattern; prefer descriptive titles such as `workflow.spec.ts`.
- When editing docs, run `npm run test:markdown` and `npm run test:frontmatter`; for structural changes, add `npm run test:structure`.
- UI or navigation edits require at least `npm run test:e2e`; attach failing screenshots from `playwright-report/` if issues persist.
- Treat link and image validation (`npm run test:links`, `npm run validate:images`) as part of your definition of done.

## Commit & Pull Request Guidelines

- Use imperative, present-tense commit subjects (~72 chars) similar to `Fix MDX build errors` or `Add modernization framework`; group related changes per commit.
- Reference issue IDs in the body when applicable and describe rationale plus testing evidence.
- PRs should include: summary of changes, verification commands run, screenshots for visual updates, and mention of follow-up tasks.
- Do not merge while `npm run test:all` is failing; request review once CI is green and pages were smoke-tested via `npm run serve`.
