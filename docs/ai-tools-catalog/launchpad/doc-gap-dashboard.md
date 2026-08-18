---
title: 'doc-gap-dashboard'
description: 'Displays the end user (public) documentation gaps between published HelpJuice documentation and user features. The application will analyze a configured set of repos/branches weekly. Provides a dashboard of history and trending gaps. Also provides drilldown to see gaps and recommendations for remediation.'
sidebar_position: 139
last_updated: 2026-08-18
tags: [ai-tools, gss-internal, launchpad]
---

# doc-gap-dashboard

> **TL;DR** — Displays the end user (public) documentation gaps between published HelpJuice documentation and user features. The application will analyze a configured set of repos/branches weekly. Provides a dashboard of history and trending gaps. Also provides drilldown to see gaps and recommendations for remediation.

## Overview

`doc-gap-dashboard` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

> One line: what does this app do? _Replace this with a real, use-case-first > sentence and keep it accurate as the app changes (see AGENTS.md)._

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/doc-gap-dashboard](https://launchpad.globalshopsolutions.dev/apps/doc-gap-dashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/doc-gap-dashboard](https://github.com/GlobalShopSolutions-InternalTools/doc-gap-dashboard)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- This app is deployed **declaratively** from git — no Launch buttons, no timers,
- nothing to toggle. Two tiers:
- **Preview** (`staging` branch → `{app}-{hash}.globalshopsolutions.dev`): push a
- `staging` branch that carries `.deploy/app.yaml` and a preview deploys
- automatically; each push rolls it onto the new `:dev` image.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** HTML
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-08-17
- **Repo topics:** `launchpad`, `owner-jonschwartz-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [jonschwartz@gssmail.com](mailto:jonschwartz@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/doc-gap-dashboard](https://github.com/GlobalShopSolutions-InternalTools/doc-gap-dashboard)
- **App page:** [doc-gap-dashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/doc-gap-dashboard)
- **Last reviewed:** 2026-08-18
