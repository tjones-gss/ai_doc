---
title: 'Assembly'
description: 'Assembly is a document sharing and review app, share proposals, review and collaborate to improve.'
sidebar_position: 135
last_updated: 2026-08-13
tags: [ai-tools, gss-internal, launchpad]
---

# Assembly

> **TL;DR** — Assembly is a document sharing and review app, share proposals, review and collaborate to improve.

## Overview

`assembly` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

> One line: Assembly — publish proposals, share the link in Teams, and > discuss or suggest changes directly on the rendered page. Every document > is versioned and fetchable as markdown.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/assembly](https://launchpad.globalshopsolutions.dev/apps/assembly)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/assembly](https://github.com/GlobalShopSolutions-InternalTools/assembly)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- This app is deployed **declaratively** from git — no Launch buttons, no
- timers, and no `staging` preview branch. Do iterative work on a feature
- branch cut from `main`; verify locally (`npm run build && npm test`); then
- merge into `main` to deploy — `main` is always-on and serves
- `assembly.globalshopsolutions.dev` directly. See [`AGENTS.md`](./AGENTS.md)
- All infra variables are optional — the server starts cleanly with none set,
- logging one warning per disabled subsystem, and `/healthz` reports
- `db: "unconfigured"`. In deployed environments these arrive via Launchpad
- Internal Secrets (ADR-0071); locally, set what you need in a `.env` or your
- shell.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-08-12
- **Repo topics:** `launchpad`, `owner-dgordon-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [dgordon@gssmail.com](mailto:dgordon@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/assembly](https://github.com/GlobalShopSolutions-InternalTools/assembly)
- **App page:** [assembly on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/assembly)
- **Last reviewed:** 2026-08-13
