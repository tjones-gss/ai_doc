---
title: 'Patch Delta Report'
description: 'Interactive web app that generates Patch Delta Reports showing known bugs between two patch build numbers, helping customers understand what issues affect their selected patch.'
sidebar_position: 130
last_updated: 2026-07-29
tags: [ai-tools, gss-internal, launchpad]
---

# Patch Delta Report

> **TL;DR** — Interactive web app that generates Patch Delta Reports showing known bugs between two patch build numbers, helping customers understand what issues affect their selected patch.

## Overview

`patch-delta-report` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Interactive web app that generates a Patch Delta Report showing known bugs between two patch build numbers.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/patch-delta-report](https://launchpad.globalshopsolutions.dev/apps/patch-delta-report)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/patch-delta-report](https://github.com/GlobalShopSolutions-InternalTools/patch-delta-report)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

```bash
- pip install -r requirements.txt
- # Set environment variables
- export ISSUE_SERVER=GSS2K19SW
- export ISSUE_DATABASE=Issue_Maintenance
- Deployed via Launchpad. Push to `main` triggers the CI pipeline which builds the Docker image and deploys automatically.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** Python
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-07-28
- **Repo topics:** `launchpad`, `owner-rravindranath-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [rravindranath@gssmail.com](mailto:rravindranath@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/patch-delta-report](https://github.com/GlobalShopSolutions-InternalTools/patch-delta-report)
- **App page:** [patch-delta-report on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/patch-delta-report)
- **Last reviewed:** 2026-07-29
