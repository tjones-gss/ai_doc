---
title: 'Traveler'
description: 'Developer workflow planning tool similar to Monday. Provides easy serviceweb call importing and agile cadence support.'
sidebar_position: 113
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Traveler

> **TL;DR** — Developer workflow planning tool similar to Monday. Provides easy serviceweb call importing and agile cadence support.

## Overview

`traveler` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Internal dev workflow app for the GSS dev team. Mirrors tickets from ServiceWeb (IHOP_Net — a GSS-internal ticket system, *not* ServiceNow) into local Postgres and renders them as a Linear/Monday-style board layered with sprint ceremonies on top.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/traveler](https://launchpad.globalshopsolutions.dev/apps/traveler)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/traveler](https://github.com/GlobalShopSolutions-InternalTools/traveler)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- Production builds + deploys are driven by the GSS launchpad reusable workflow, which runs automatically on push to `main`:
- **[`.github/workflows/ci.yaml`](.github/workflows/ci.yaml)** — thin wrapper that delegates to `GlobalShopSolutions-Limited/.github/.github/workflows/internal-app-build.yml@main`. Don't modify without coordinating with platform.
- **[`.github/workflows/pr.yml`](.github/workflows/pr.yml)** — our own lint/typecheck/branch-name checks on every PR. Additive, doesn't interfere with the launchpad flow.
- **[`Dockerfile`](Dockerfile)** — multi-stage Next.js standalone build. Builds `app/` and serves on port 8080.
- **[`entrypoint.sh`](entrypoint.sh)** — runs `prisma migrate deploy` on container start, then execs `node server.js`. Fail-fast: a migration error aborts the container before it ever accepts traffic. If we scale to multiple replicas this needs to move to a pre-deploy step to avoid races.
- The container reads these from env at startup. Launchpad should inject them from its secret store (the ones marked **secret**) and from app config (everything else):
- `DATABASE_URL` is the primary secret the app uses; `PGHOST`/`PGUSER`/`PGPASSWORD`/`PGPORT`/`PGSSLMODE` are the individual libpq vars (the entrypoint constructs `DATABASE_URL` from them if only those are set). `POSTGRES_USER/PASSWORD/DB` and `SW_SQL_SA_PASSWORD` are dev-only (docker-compose) and not needed in prod.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-07-01
- **Repo topics:** `launchpad`, `owner-dgordon-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [dgordon@gssmail.com](mailto:dgordon@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/traveler](https://github.com/GlobalShopSolutions-InternalTools/traveler)
- **App page:** [traveler on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/traveler)
- **Last reviewed:** 2026-07-02
