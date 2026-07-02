---
title: 'RepoHound'
description: 'A tool to fuzzy search all orgs and repos'
sidebar_position: 99
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# RepoHound

> **TL;DR** — A tool to fuzzy search all orgs and repos

## Overview

`repohound` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Fuzzy-searchable index of every repository you can reach across the Global Shop Solutions GitHub Enterprise (and, optionally, the internal SVN tree). Deployed on Launchpad at repohound.globalshopsolutions.dev.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/repohound](https://launchpad.globalshopsolutions.dev/apps/repohound)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/repohound](https://github.com/GlobalShopSolutions-InternalTools/repohound)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

No setup details were available in the README. Start with the LaunchPad page or repository, then ask the owner for access and environment details.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** CSS
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-05-07
- **Repo topics:** `launchpad`, `owner-sstarlin-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [sstarlin@gssmail.com](mailto:sstarlin@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/repohound](https://github.com/GlobalShopSolutions-InternalTools/repohound)
- **App page:** [repohound on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/repohound)
- **Last reviewed:** 2026-07-02
