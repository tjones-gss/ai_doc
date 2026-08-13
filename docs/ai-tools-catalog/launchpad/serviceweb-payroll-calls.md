---
title: 'serviceweb-payroll-calls'
description: 'Show me the payroll calls'
sidebar_position: 137
last_updated: 2026-08-13
tags: [ai-tools, gss-internal, launchpad]
---

# serviceweb-payroll-calls

> **TL;DR** — Show me the payroll calls

## Overview

`serviceweb-payroll-calls` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Lets Payroll QA see open Service Web calls in the Testing-OE/Acct/Systems queue without digging through Service Web filters.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/serviceweb-payroll-calls](https://launchpad.globalshopsolutions.dev/apps/serviceweb-payroll-calls)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/serviceweb-payroll-calls](https://github.com/GlobalShopSolutions-InternalTools/serviceweb-payroll-calls)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- Push to `main` triggers automatic deploy via `.deploy/app.yaml` + Docker.
- After deploy, open https://serviceweb-payroll-calls.globalshopsolutions.dev
- Recommended flow: iterate on `staging`, verify on the preview hostname, then merge `staging` → `main` to promote.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** Python
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-08-12
- **Repo topics:** `launchpad`, `owner-mpasemann-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [mpasemann@gssmail.com](mailto:mpasemann@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/serviceweb-payroll-calls](https://github.com/GlobalShopSolutions-InternalTools/serviceweb-payroll-calls)
- **App page:** [serviceweb-payroll-calls on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/serviceweb-payroll-calls)
- **Last reviewed:** 2026-08-13
