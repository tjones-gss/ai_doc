---
title: 'genii-quote'
description: 'An intelligent quoting assistant that helps Global Shop Solutions quickly estimate and prepare quotes for customer custom requests by analyzing request details, identifying required work, suggesting pricing inputs, and producing a consistent, review-ready quote summary for the team.'
sidebar_position: 73
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# genii-quote

> **TL;DR** — An intelligent quoting assistant that helps Global Shop Solutions quickly estimate and prepare quotes for customer custom requests by analyzing request details, identifying required work, suggesting pricing inputs, and producing a consistent, review-ready quote summary for the team.

## Overview

`genii-quote` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Genii Estimate Assistant helps Global Shop Solutions programmers turn internal tickets into reviewable dev and QA hour estimates. Cursor plus Genii MCP is the primary workflow; the Next.js app is a light review and export surface.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/genii-quote](https://launchpad.globalshopsolutions.dev/apps/genii-quote)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/genii-quote](https://github.com/GlobalShopSolutions-InternalTools/genii-quote)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

1. Copy `.env.example` to `.env.local` and set `DATABASE_URL` if needed.
2. Install dependencies with `npm install`.
3. Apply the SQLite schema with `npm run db:migrate`.
4. Start the app with `npm run dev` and open `http://localhost:8080`.
- The web UI uses Next.js Server Components and Server Actions. There is no browser-facing REST API for estimate CRUD or export.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-09
- **Repo topics:** `launchpad`, `owner-mhowerton-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [mhowerton@gssmail.com](mailto:mhowerton@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/genii-quote](https://github.com/GlobalShopSolutions-InternalTools/genii-quote)
- **App page:** [genii-quote on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/genii-quote)
- **Last reviewed:** 2026-07-02
