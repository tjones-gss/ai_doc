---
title: 'Rocketlane MCP Server v2.7.1'
description: 'MCP for connecting with Rocketlane'
sidebar_position: 13
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, mcp-server]
---

# Rocketlane MCP Server v2.7.1

> **TL;DR** — MCP for connecting with Rocketlane

## Overview

`rocketlane-mcp` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Model Context Protocol server for managing customer implementation projects in Rocketlane, built for Global Shop Solutions.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad / MCP endpoint:** [https://launchpad.globalshopsolutions.dev/apps/rocketlane-mcp](https://launchpad.globalshopsolutions.dev/apps/rocketlane-mcp)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/rocketlane-mcp](https://github.com/GlobalShopSolutions-InternalTools/rocketlane-mcp)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

```bash
- npm install
- npm run build        # TypeScript compile
- npm run typecheck    # Type check without emit
- npm test             # Unit tests (vitest)
- Deployed via Launchpad. Push to `main` on `GlobalShopSolutions-InternalTools/rocketlane-mcp` triggers CI build + deploy.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-07-01
- **Repo topics:** `launchpad`, `owner-adsouza-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [adsouza@gssmail.com](mailto:adsouza@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/rocketlane-mcp](https://github.com/GlobalShopSolutions-InternalTools/rocketlane-mcp)
- **App page:** [rocketlane-mcp on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/rocketlane-mcp)
- **Last reviewed:** 2026-07-02
