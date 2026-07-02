---
title: 'SourceControlWatch'
description: 'Tracks file-level GitHub and SVN commits for GSS; local WinForms app plus a Launchpad API for queries, ingest, and mcp-intelligence.'
sidebar_position: 14
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, mcp-server]
---

# SourceControlWatch

> **TL;DR** — Tracks file-level GitHub and SVN commits for GSS; local WinForms app plus a Launchpad API for queries, ingest, and mcp-intelligence.

## Overview

`sourcecontrolwatch` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

| Remote | URL | |--------|-----| | GitHub (code) | GlobalShopSolutions-Maintenance/SourceControlWatch | | Launchpad (deploy target) | GlobalShopSolutions-InternalTools/sourcecontrolwatch | | Service URL (after Internal Tools deploy) | https://sourcecontrolwatch.globalshopsolutions.dev/_/dashboard/ | | REST API (automation) | docs/API-INTEGRATION.md |

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad / MCP endpoint:** [https://launchpad.globalshopsolutions.dev/apps/sourcecontrolwatch](https://launchpad.globalshopsolutions.dev/apps/sourcecontrolwatch)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/sourcecontrolwatch](https://github.com/GlobalShopSolutions-InternalTools/sourcecontrolwatch)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- Optional **repo allowlist** in Settings: one `owner/repo` per line. Leave empty to poll all org repositories.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** C#
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-25
- **Repo topics:** `launchpad`, `owner-bcorbin-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [bcorbin@gssmail.com](mailto:bcorbin@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/sourcecontrolwatch](https://github.com/GlobalShopSolutions-InternalTools/sourcecontrolwatch)
- **App page:** [sourcecontrolwatch on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/sourcecontrolwatch)
- **Last reviewed:** 2026-07-02
