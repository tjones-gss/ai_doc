---
title: 'Newsletter'
description: 'Internal app created via Launchpad by avaldez@gssmail.com'
sidebar_position: 91
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Newsletter

> **TL;DR** — Internal app created via Launchpad by avaldez@gssmail.com

## Overview

`newsletter` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Internal newsletter app with WYSIWYG editing, slide deck uploads, and a featured showcase.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/newsletter](https://launchpad.globalshopsolutions.dev/apps/newsletter)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/newsletter](https://github.com/GlobalShopSolutions-InternalTools/newsletter)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

```bash
- # Terminal 1: Start Express server (uses SQLite locally)
- cd server
- npm install
- npm run dev
- Push to `main` triggers automatic build and deploy to `newsletter.globalshopsolutions.dev`.
- PostgreSQL runs as a StatefulSet with persistent storage. Create the `POSTGRES_PASSWORD` secret
- via [secrets.globalshopsolutions.dev](https://secrets.globalshopsolutions.dev) before first deploy.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-05-22
- **Repo topics:** `launchpad`, `owner-avaldez-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [avaldez@gssmail.com](mailto:avaldez@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/newsletter](https://github.com/GlobalShopSolutions-InternalTools/newsletter)
- **App page:** [newsletter on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/newsletter)
- **Last reviewed:** 2026-07-02
