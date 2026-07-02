---
title: 'Carte Blanche'
description: 'A scratch pad app for quick diagrams and free-drawing that doesn''t look or feel terrible.'
sidebar_position: 64
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Carte Blanche

> **TL;DR** — A scratch pad app for quick diagrams and free-drawing that doesn't look or feel terrible.

## Overview

`carte-blanche` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

A scratch pad app for quick diagrams and free-drawing that doesn't look or feel terrible.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/carte-blanche](https://launchpad.globalshopsolutions.dev/apps/carte-blanche)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/carte-blanche](https://github.com/GlobalShopSolutions-InternalTools/carte-blanche)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

```bash
- docker build -t carte-blanche .
- docker run -p 8080:8080 carte-blanche
- # → open http://localhost:8080
```

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-05-21
- **Repo topics:** `launchpad`, `owner-sstarlin-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [sstarlin@gssmail.com](mailto:sstarlin@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/carte-blanche](https://github.com/GlobalShopSolutions-InternalTools/carte-blanche)
- **App page:** [carte-blanche on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/carte-blanche)
- **Last reviewed:** 2026-07-02
