---
title: 'gss-mafia'
description: 'a helper tool to aid in having productive mob sessions'
sidebar_position: 126
last_updated: 2026-07-24
tags: [ai-tools, gss-internal, launchpad]
---

# gss-mafia

> **TL;DR** — a helper tool to aid in having productive mob sessions

## Overview

`gss-mafia` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

> Helps teams plan mob programming sessions — collect suggestions, pick topics, assign groups, and review how past mobs went.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/gss-mafia](https://launchpad.globalshopsolutions.dev/apps/gss-mafia)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-mafia](https://github.com/GlobalShopSolutions-InternalTools/gss-mafia)
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

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-07-23
- **Repo topics:** `launchpad`, `owner-sharris-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [sharris@gssmail.com](mailto:sharris@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-mafia](https://github.com/GlobalShopSolutions-InternalTools/gss-mafia)
- **App page:** [gss-mafia on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/gss-mafia)
- **Last reviewed:** 2026-07-24
