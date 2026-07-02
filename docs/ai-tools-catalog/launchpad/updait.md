---
title: 'Updait'
description: 'A system for distilling project updates into a single, audience-focused, textual update'
sidebar_position: 114
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Updait

> **TL;DR** — A system for distilling project updates into a single, audience-focused, textual update

## Overview

`updait` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Distills GitHub, Monday.com, and (soon) other docs/project signals into a single, audience-tailored project update.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/updait](https://launchpad.globalshopsolutions.dev/apps/updait)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/updait](https://github.com/GlobalShopSolutions-InternalTools/updait)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- `docker build -t updait .` → multi-stage Node image that builds both workspaces and
- serves the web bundle from `UPDAIT_STATIC_DIR=/app/web/dist` on port 8080. Bound
- to ECR + ArgoCD via the Launchpad reusable workflow already wired in `.github/workflows/`.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** JavaScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-05-07
- **Repo topics:** `launchpad`, `owner-sstarlin-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [sstarlin@gssmail.com](mailto:sstarlin@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/updait](https://github.com/GlobalShopSolutions-InternalTools/updait)
- **App page:** [updait on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/updait)
- **Last reviewed:** 2026-07-02
