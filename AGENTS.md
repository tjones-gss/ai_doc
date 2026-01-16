# Repository Guidelines

## Project Structure & Module Organization

- `docs/` holds all published Markdown; keep kebab-case filenames and refresh `last_updated`.
- `src/pages` plus `src/css/custom.css` drive bespoke layouts and theming, while assets live in `static/` (served) and `images/` (source); centralize shared tokens in `custom.css` for easy updates.
- `scripts/` hosts the Node validators, and `tests/e2e` together with `.github/workflows/deploy.yml` guard releases by running Playwright checks and deploying the `build/` artifact to GitHub Pages.

## Build, Test, and Development Commands

- `npm install` (or `npm ci` in CI) with Node 22 readies dependencies; run `npm run prepare` once to install Husky hooks.
- `npm start` serves Docusaurus at `http://localhost:3000` with hot reload so documentation changes are visible immediately.
- `npm run build` writes the production bundle to `build/`; follow with `npm run serve` for smoke tests or `npm run deploy` to mirror the Pages workflow.
- `npm run test:all` chains build, link, markdown, frontmatter, and Playwright checks; use `npm run validate` for quicker loops and ensure `npm run lint`, `format:check`, and `type-check` pass before every PR.

## Coding Style & Naming Conventions

- Prettier enforces two-space indentation, single quotes, and ~100 character line width; fix formatting via `npm run format`.
- ESLint (React + @typescript-eslint) expects functional components, top-level hooks, and typed props; lint-staged blocks commits until violations are fixed.
- Markdown should stay at H3 or flatter with descriptive IDs and kebab-case assets like `ai-menu-dark.png`; extend tokens in `src/css/custom.css` via CSS variables instead of inline hex.

## Testing Guidelines

- `npm run test:build` compiles into `build-test`, catching MDX, import, or TypeScript drift before CI runs.
- `npm run test:links`, `npm run test:markdown`, and `npm run test:frontmatter` rely on the validators in `scripts/`; add structural rules there rather than scattering custom scripts.
- Playwright suites sit under `tests/e2e` with `*.spec.ts` naming; use `npm run test:e2e:headed` when debugging navigation regressions and attach relevant screenshots to pull requests.

## Commit & Pull Request Guidelines

- Git history favors short, imperative subjects such as `Fix mobile hamburger menu z-index issue`; keep subjects under 72 characters and add `Refs #123` in the body when applicable.
- Branches follow descriptive slugs like `claude/enhance-docs-ui-design-011CV5tMRttQBeFWAkU5nGPZ`; reuse that structure for clarity and automated triage.
- Pull requests should describe user impact, list the validation commands run (`npm run test:all`, `npm run lint`), include before/after screenshots for `docs/` or `src/pages`, and confirm `last_updated`, `static/img`, and config changes stay aligned.
