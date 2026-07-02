---
title: 'TC Navigation Catalog'
description: 'Internal app created via Launchpad by zsandford@gssmail.com'
sidebar_position: 109
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# TC Navigation Catalog

> **TL;DR** — Internal app created via Launchpad by zsandford@gssmail.com

## Overview

`tc-navigation-catalog` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

REST service on LaunchPad that catalogs test ↔ program ↔ menu path mappings from TAD CacheData, plus mined TA script steps per menu path via testarchitect-mcp.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/tc-navigation-catalog](https://launchpad.globalshopsolutions.dev/apps/tc-navigation-catalog)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tc-navigation-catalog](https://github.com/GlobalShopSolutions-InternalTools/tc-navigation-catalog)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

```bash
- pip install -e ".[dev]"
- set TAD_DATABASE_URL=http://localhost:7001
- set TA_MCP_URL=http://localhost:8080/mcp
- python -m catalog
- LaunchPad internal app — port `8080`. `.deploy/dependencies.yaml` adds a **2Gi PVC** at `/data` for persistent SQLite.
- Deployment uses **`strategy: Recreate`** (not RollingUpdate) because the PVC is `ReadWriteOnce` — only one pod can mount the volume at a time.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** Python
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-07-01
- **Repo topics:** `launchpad`, `owner-zsandford-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [zsandford@gssmail.com](mailto:zsandford@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tc-navigation-catalog](https://github.com/GlobalShopSolutions-InternalTools/tc-navigation-catalog)
- **App page:** [tc-navigation-catalog on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/tc-navigation-catalog)
- **Last reviewed:** 2026-07-02
