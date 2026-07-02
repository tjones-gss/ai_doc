---
title: 'TAD Triage & Remediation-Draft Assistant'
description: 'Internal app created via Launchpad by kcao@gssmail.com'
sidebar_position: 107
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# TAD Triage & Remediation-Draft Assistant

> **TL;DR** — Internal app created via Launchpad by kcao@gssmail.com

## Overview

`tad-too-tuff` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Listens for TAD DebugFileProcessed SignalR events and, for each completed failure, gathers evidence (video frames at the error mark + GraphQL history + infralog runtime logs + TA execution log), triages it with an OpenAI subagent, and routes the outcome through a deterministic, guardrailed state machine: high-confidence non-terminal test bugs → draft a QA/TA notification; product bugs → open a *draft* GitHub PR for human review; uncertain/inconclusive → escalated to humans. The agent never merges. Every processed failure produces a FailureReport fanned out to pluggable report sinks (markdown file + console + dashboard always on; webhook + GraphQL write-back gated).

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/tad-too-tuff](https://launchpad.globalshopsolutions.dev/apps/tad-too-tuff)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tad-too-tuff](https://github.com/GlobalShopSolutions-InternalTools/tad-too-tuff)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

```bash
- npm install
- cp .env.example .env   # set OPENAI_API_KEY and DATABASE_* vars; keep DRY_RUN=true to start
- npm run dev
```

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-17
- **Repo topics:** `launchpad`, `owner-kcao-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [kcao@gssmail.com](mailto:kcao@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tad-too-tuff](https://github.com/GlobalShopSolutions-InternalTools/tad-too-tuff)
- **App page:** [tad-too-tuff on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/tad-too-tuff)
- **Last reviewed:** 2026-07-02
