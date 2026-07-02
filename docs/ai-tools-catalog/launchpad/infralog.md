---
title: 'infralog'
description: 'Scrape logs and store them in a ClickHouse database for AI consumption'
sidebar_position: 83
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# infralog

> **TL;DR** — Scrape logs and store them in a ClickHouse database for AI consumption

## Overview

`infralog` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Centralized log collection for the GSS test fleet. A daily Windows scheduled task on each TGFTA test VM uploads GSS application logs (CoreLog, GSSEO, TRAC, menu, file errors, multistation) to a launchpad-hosted ingest endpoint, which parses and stores them in ClickHouse for analysis.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/infralog](https://launchpad.globalshopsolutions.dev/apps/infralog)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/infralog](https://github.com/GlobalShopSolutions-InternalTools/infralog)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- Push to `main`; the platform's standard CI builds the Docker image, ArgoCD discovers `.deploy/app.yaml`, and the app appears at `https://infralog.globalshopsolutions.dev` in ~3 minutes.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** C#
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-04
- **Repo topics:** `launchpad`, `owner-aakram-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [aakram@gssmail.com](mailto:aakram@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/infralog](https://github.com/GlobalShopSolutions-InternalTools/infralog)
- **App page:** [infralog on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/infralog)
- **Last reviewed:** 2026-07-02
