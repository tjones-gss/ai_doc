---
title: 'release-hub'
description: 'Coordinates the release pipeline: QA testing, management approval, and IT submission to the Microsoft Store.'
sidebar_position: 132
last_updated: 2026-08-05
tags: [ai-tools, gss-internal, launchpad]
---

# release-hub

> **TL;DR** — Coordinates the release pipeline: QA testing, management approval, and IT submission to the Microsoft Store.

## Overview

`release-hub` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

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

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/release-hub](https://launchpad.globalshopsolutions.dev/apps/release-hub)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/release-hub](https://github.com/GlobalShopSolutions-InternalTools/release-hub)
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
- **Repo last pushed:** 2026-08-04
- **Repo topics:** `launchpad`, `owner-zwu-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [zwu@gssmail.com](mailto:zwu@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/release-hub](https://github.com/GlobalShopSolutions-InternalTools/release-hub)
- **App page:** [release-hub on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/release-hub)
- **Last reviewed:** 2026-08-05
