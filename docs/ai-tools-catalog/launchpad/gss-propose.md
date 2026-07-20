---
title: 'gss-propose'
description: 'A webapp for publishing proposals you would like to directly link to chats in teams, proposals will persist and allow in-line comments and rework tracking much like a google doc.'
sidebar_position: 124
last_updated: 2026-07-20
tags: [ai-tools, gss-internal, launchpad]
---

# gss-propose

> **TL;DR** — A webapp for publishing proposals you would like to directly link to chats in teams, proposals will persist and allow in-line comments and rework tracking much like a google doc.

## Overview

`gss-propose` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

> One line: what does this app do? _Replace this with a real, use-case-first > sentence and keep it accurate as the app changes (see AGENTS.md)._

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/gss-propose](https://launchpad.globalshopsolutions.dev/apps/gss-propose)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-propose](https://github.com/GlobalShopSolutions-InternalTools/gss-propose)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- This app is deployed **declaratively** from git — no Launch buttons, no timers,
- nothing to toggle. Two tiers:
- **Preview** (`staging` branch → `{app}-{hash}.globalshopsolutions.dev`): push a
- `staging` branch that carries `.deploy/app.yaml` and a preview deploys
- automatically; each push rolls it onto the new `:dev` image.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** HTML
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-07-17
- **Repo topics:** `launchpad`, `owner-dgordon-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [dgordon@gssmail.com](mailto:dgordon@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-propose](https://github.com/GlobalShopSolutions-InternalTools/gss-propose)
- **App page:** [gss-propose on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/gss-propose)
- **Last reviewed:** 2026-07-20
