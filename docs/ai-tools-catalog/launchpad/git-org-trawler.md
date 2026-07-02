---
title: 'GitHub Org Trawler'
description: 'An application that is able to search through all available repos for information'
sidebar_position: 74
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# GitHub Org Trawler

> **TL;DR** — An application that is able to search through all available repos for information

## Overview

`git-org-trawler` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Web application that discovers repositories across the GitHub Enterprise (and optional SVN) and trawls their contents using pluggable "trawlers" — small plugins that each define what to search for and how to search for it.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/git-org-trawler](https://launchpad.globalshopsolutions.dev/apps/git-org-trawler)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/git-org-trawler](https://github.com/GlobalShopSolutions-InternalTools/git-org-trawler)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- From the repo root (scripts proxy into `server/` and `client/`):
```bash
- # install once per workspace
- cd server && npm install && cd ../client && npm install && cd ..
- # run the API and UI (separate terminals)
- This repo is configured for the internal self-service platform:
- > Note: in dev the token-encryption key is **ephemeral** — stored PATs become unreadable after a server restart. Set a real `TRAWLER_TOKEN_ENCRYPTION_KEY` if you want them to survive.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-29
- **Repo topics:** `launchpad`, `owner-sharris-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [sharris@gssmail.com](mailto:sharris@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/git-org-trawler](https://github.com/GlobalShopSolutions-InternalTools/git-org-trawler)
- **App page:** [git-org-trawler on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/git-org-trawler)
- **Last reviewed:** 2026-07-02
