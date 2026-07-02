---
title: 'Issue Maintenance — Menu Ownership Dashboard'
description: 'Internal app created via Launchpad by cmasden@gssmail.com'
sidebar_position: 85
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Issue Maintenance — Menu Ownership Dashboard

> **TL;DR** — Internal app created via Launchpad by cmasden@gssmail.com

## Overview

`issue-maintenance-menu-ownership` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Dashboard that visualizes which developers own which menu items in Global Shop Solutions, based on issue history from the Issue Maintenance API.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/issue-maintenance-menu-ownership](https://launchpad.globalshopsolutions.dev/apps/issue-maintenance-menu-ownership)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/issue-maintenance-menu-ownership](https://github.com/GlobalShopSolutions-InternalTools/issue-maintenance-menu-ownership)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- The React app calls the Issue Maintenance API directly using browser-native Windows Authentication.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** TypeScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-24
- **Repo topics:** `launchpad`, `owner-cmasden-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [cmasden@gssmail.com](mailto:cmasden@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/issue-maintenance-menu-ownership](https://github.com/GlobalShopSolutions-InternalTools/issue-maintenance-menu-ownership)
- **App page:** [issue-maintenance-menu-ownership on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/issue-maintenance-menu-ownership)
- **Last reviewed:** 2026-07-02
