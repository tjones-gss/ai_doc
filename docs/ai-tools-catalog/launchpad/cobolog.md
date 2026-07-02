---
title: 'cobolog - Quick User Manual'
description: 'CLI that adds removable debug logging (flow, watchpoints, PERFORM snapshots) to Fujitsu COBOL sources for bug triage. Used from Cursor with YAML configs; not a cluster service—developers run cobolog install locally, compile, and read COMPROC logs in the clinic.'
sidebar_position: 66
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# cobolog - Quick User Manual

> **TL;DR** — CLI that adds removable debug logging (flow, watchpoints, PERFORM snapshots) to Fujitsu COBOL sources for bug triage. Used from Cursor with YAML configs; not a cluster service—developers run cobolog install locally, compile, and read COMPROC logs in the clinic.

## Overview

`cobolog` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

cobolog instruments fixed-format COBOL files with removable logging lines tagged *>COBOLOG.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/cobolog](https://launchpad.globalshopsolutions.dev/apps/cobolog)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/cobolog](https://github.com/GlobalShopSolutions-InternalTools/cobolog)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

No setup details were available in the README. Start with the LaunchPad page or repository, then ask the owner for access and environment details.

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** Python
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-05-28
- **Repo topics:** `launchpad`, `owner-smiller-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [smiller@gssmail.com](mailto:smiller@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/cobolog](https://github.com/GlobalShopSolutions-InternalTools/cobolog)
- **App page:** [cobolog on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/cobolog)
- **Last reviewed:** 2026-07-02
