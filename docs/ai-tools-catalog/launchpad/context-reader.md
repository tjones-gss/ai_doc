---
title: 'context-reader'
description: 'read context to see claims'
sidebar_position: 67
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# context-reader

> **TL;DR** — read context to see claims

## Overview

`context-reader` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

A tiny webapp that publishes the auth context the client sends in — Authorization header, cookies, JWT payloads (header + claims, signature not verified), query params, and the full request headers — as JSON or a formatted HTML page.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/context-reader](https://launchpad.globalshopsolutions.dev/apps/context-reader)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/context-reader](https://github.com/GlobalShopSolutions-InternalTools/context-reader)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

No setup details were available in the README. Start with the LaunchPad page or repository, then ask the owner for access and environment details.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** JavaScript
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-05-11
- **Repo topics:** `launchpad`, `owner-dgordon-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [dgordon@gssmail.com](mailto:dgordon@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/context-reader](https://github.com/GlobalShopSolutions-InternalTools/context-reader)
- **App page:** [context-reader on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/context-reader)
- **Last reviewed:** 2026-07-02
