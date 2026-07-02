---
title: 'Beacon'
description: 'Internal app created via Launchpad by brennanhitchcock@gssmail.com'
sidebar_position: 61
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, launchpad]
---

# Beacon

> **TL;DR** — Internal app created via Launchpad by brennanhitchcock@gssmail.com

## Overview

`beacon` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Beacon is an internal MCP (Model Context Protocol) server that gives AI assistants semantic search across Global Shop Solutions' Helpjuice documentation and curated internal knowledge. Use it from Claude Code, Claude Desktop, Cursor, ChatGPT, or any MCP-capable client to ask questions in natural language and get back ranked, citable snippets from the knowledge base.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad app:** [https://launchpad.globalshopsolutions.dev/apps/beacon](https://launchpad.globalshopsolutions.dev/apps/beacon)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/beacon](https://github.com/GlobalShopSolutions-InternalTools/beacon)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

- Hosted on **Launchpad** (internal Kubernetes platform). The app runs as an MCP-type service on port 8080 with sidecar pods for PostgreSQL + pgvector and Ollama. See `.deploy/app.yaml` and `.deploy/dependencies.yaml` for the full manifests.
- [Launchpad status](https://status.globalshopsolutions.dev)
- Configuration uses ASP.NET Core's standard layered config (appsettings.json → environment variables → command-line). Environment variable names follow the `__`-separator convention for nested keys (e.g. `Helpjuice__ApiKey`).

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** C#
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-25
- **Repo topics:** `launchpad`, `owner-brennanhitchcock-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [brennanhitchcock@gssmail.com](mailto:brennanhitchcock@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/beacon](https://github.com/GlobalShopSolutions-InternalTools/beacon)
- **App page:** [beacon on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/beacon)
- **Last reviewed:** 2026-07-02
