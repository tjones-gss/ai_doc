---
title: 'GSS Document API - Admin Dashboard'
description: 'Account management / Admin tool.'
sidebar_position: 77
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# GSS Document API - Admin Dashboard

> **TL;DR** — Account management / Admin tool.

## Overview

`gss-document-api-account-management` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Web UI for Genii Extract GSS Document API account management. It covers clients, API keys, audit log, and per-client detail (companies, usage, billing, documents).

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/gss-document-api-account-management](https://launchpad.globalshopsolutions.dev/apps/gss-document-api-account-management)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-document-api-account-management](https://github.com/GlobalShopSolutions-InternalTools/gss-document-api-account-management)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

1. Create a virtual environment and install dependencies:
```bash
- python -m venv .venv
- .venv\Scripts\activate
- pip install -r requirements.txt
- This repository is used for development.
- Deployment uses our internal **Launchpad** self-service platform: https://docs.globalshopsolutions.dev/
- There is a seperate repository for deploying to launchpad: https://github.com/GlobalShopSolutions-InternalTools/gss-document-api-account-management
- A push to **`main`** on the InternalTools repo triggers github CI to build and deploy the app: [`.github/workflows/ci.yaml`](.github/workflows/ci.yaml)
- The app itself is definied in: [`.deploy/app.yaml`](.deploy/app.yaml)
- Variables are loaded from the environment and from `.env` in the project root. See [`.env.example`](.env.example).
- **`DOCUMENT_API_BASE_URL` /  `GSS_DOCUMENT_API_BASE_URL`** (required). GSS Document API URL to read from.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** JavaScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-30
- **Repo topics:** `launchpad`, `owner-rseddon-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [rseddon@gssmail.com](mailto:rseddon@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-document-api-account-management](https://github.com/GlobalShopSolutions-InternalTools/gss-document-api-account-management)
- **App page:** [gss-document-api-account-management on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/gss-document-api-account-management)
- **Last reviewed:** 2026-07-02
