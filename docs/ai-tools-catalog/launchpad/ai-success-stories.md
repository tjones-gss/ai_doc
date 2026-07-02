---
title: 'AI Success Stories'
description: 'AI Success Stories is an internal tool for tracking and celebrating how our department uses AI in their day-to-day work. Team members submit monthly stories describing a problem they solved with AI, what tool they used, and the impact it had.'
sidebar_position: 58
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# AI Success Stories

> **TL;DR** — AI Success Stories is an internal tool for tracking and celebrating how our department uses AI in their day-to-day work. Team members submit monthly stories describing a problem they solved with AI, what tool they used, and the impact it had.

## Overview

`ai-success-stories` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

AI Success Stories is an internal launchpad app for monthly AI-win submissions, admin highlights, and compliance visibility across teams.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/ai-success-stories](https://launchpad.globalshopsolutions.dev/apps/ai-success-stories)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/ai-success-stories](https://github.com/GlobalShopSolutions-InternalTools/ai-success-stories)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

1. Install Node.js 20+ and .NET 9 SDK.
2. Set environment variables:
- `POSTGRES_CONNECTION_STRING`
- `AZURE_OPENAI_KEY`
- `AZURE_OPENAI_ENDPOINT`
- `.deploy/app.yaml` defines app runtime settings
- `.deploy/dependencies.yaml` defines PostgreSQL dependency wiring
- `Dockerfile` builds client and server into one runtime image

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** C#
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-04-29
- **Repo topics:** `launchpad`, `owner-smedina-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [smedina@gssmail.com](mailto:smedina@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/ai-success-stories](https://github.com/GlobalShopSolutions-InternalTools/ai-success-stories)
- **App page:** [ai-success-stories on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/ai-success-stories)
- **Last reviewed:** 2026-07-02
