---
title: 'support-data-extraction-vault'
description: 'Internal app created via Launchpad by jjoy@gssmail.com'
sidebar_position: 104
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# support-data-extraction-vault

> **TL;DR** — Internal app created via Launchpad by jjoy@gssmail.com

## Overview

`support-data-extraction-vault` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

The production upload vault for the Support Data Extraction tool (brief rule #2: no Graph secret on the customer box). The tool (running inside GSSMenu on a customer box) collects + zips support data and POSTs the zip here; this listener holds the Microsoft Graph credential server-side and performs the upload to the Customer Care Uploads SharePoint library, then returns the SharePoint reference.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/support-data-extraction-vault](https://launchpad.globalshopsolutions.dev/apps/support-data-extraction-vault)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/support-data-extraction-vault](https://github.com/GlobalShopSolutions-InternalTools/support-data-extraction-vault)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- Push to `main` → CI builds the image to ECR → ArgoCD deploys to the `internal` namespace at
- `https://support-data-extraction-vault.globalshopsolutions.dev`.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** C#
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-30
- **Repo topics:** `launchpad`, `owner-jjoy-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [jjoy@gssmail.com](mailto:jjoy@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/support-data-extraction-vault](https://github.com/GlobalShopSolutions-InternalTools/support-data-extraction-vault)
- **App page:** [support-data-extraction-vault on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/support-data-extraction-vault)
- **Last reviewed:** 2026-07-02
