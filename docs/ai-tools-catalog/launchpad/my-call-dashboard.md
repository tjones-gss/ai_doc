---
title: 'Call Dashboard'
description: 'Real-time Call Dashboard for tracking Service Web calls through triage, development, and QA. Includes a local web dashboard, weekly reports, and Cursor IDE automation.'
sidebar_position: 89
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Call Dashboard

> **TL;DR** — Real-time Call Dashboard for tracking Service Web calls through triage, development, and QA. Includes a local web dashboard, weekly reports, and Cursor IDE automation.

## Overview

`my-call-dashboard` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

A real-time visibility dashboard for tracking Service Web calls through the triage, development, and QA pipeline. Provides at-a-glance status of all active working slots, historical records of completed work, and integrates with Cursor IDE agent automation for hands-free manifest updates.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/my-call-dashboard](https://launchpad.globalshopsolutions.dev/apps/my-call-dashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/my-call-dashboard](https://github.com/GlobalShopSolutions-InternalTools/my-call-dashboard)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- The `setup.ps1` script creates:
- Numbered working directories with your chosen prefix (e.g., `Triage 01` through `Triage 99`)
- `triage-manifest.json` (pre-seeded with available slots)
- `triage-history.json` (empty, ready for recycled entries)
- `call-dashboard-config.json` (your personal config)
- The server reads `call-dashboard-config.json` from the `server/` directory or one level up. Example:
```json
- {
- "userName": "Shawn Miller",
- "workingDirectoryRoot": "C:\\Users\\smiller\\Cursor",

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** PowerShell
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-24
- **Repo topics:** `launchpad`, `owner-smiller-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [smiller@gssmail.com](mailto:smiller@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/my-call-dashboard](https://github.com/GlobalShopSolutions-InternalTools/my-call-dashboard)
- **App page:** [my-call-dashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/my-call-dashboard)
- **Last reviewed:** 2026-07-02
