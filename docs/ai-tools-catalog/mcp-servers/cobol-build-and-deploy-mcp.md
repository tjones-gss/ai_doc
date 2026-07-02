---
title: 'COBOL Build & Deploy MCP Server'
description: 'MCP server that will build COBOL programs, launch a Clinic company, and deploy built programs to Company'
sidebar_position: 11
last_updated: 2026-07-02
tags: [ai-tools, gss-internal, mcp-server]
---

# COBOL Build & Deploy MCP Server

> **TL;DR** — MCP server that will build COBOL programs, launch a Clinic company, and deploy built programs to Company

## Overview

`cobol-build-and-deploy-mcp` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

Exposes tools for AI agents to generate, compile, and deploy COBOL programs (Fujitsu NetCOBOL and AcuCOBOL/MicroFocus) to clinic test environments.

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **LaunchPad / MCP endpoint:** [https://launchpad.globalshopsolutions.dev/apps/cobol-build-and-deploy-mcp](https://launchpad.globalshopsolutions.dev/apps/cobol-build-and-deploy-mcp)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/cobol-build-and-deploy-mcp](https://github.com/GlobalShopSolutions-InternalTools/cobol-build-and-deploy-mcp)
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

1. Copy the `cobol-build-and-deploy-mcp` folder to your machine (e.g., `~/.cursor/mcp-servers/cobol-build-and-deploy-mcp/`).
2. Copy `config.example.json` to `config.json` and adjust any paths for your environment:
- `test_case_directory` — point to your personal QA test case subfolder
- (e.g., `\\\\gss2k19rnd\\Development\\BUGTeam\\AutomationReadyTestcases\\YourName`).
- Test plan files are matched by **issue number** or **call reference** in the

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

- **Primary language:** Python
- **Catalog trigger:** new — New repository detected in GlobalShopSolutions-InternalTools.
- **Repo last pushed:** 2026-06-05
- **Repo topics:** `launchpad`, `owner-smiller-at-gssmail-dot-com`, `starter`

## Owner & support

- **Owner:** [smiller@gssmail.com](mailto:smiller@gssmail.com)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/cobol-build-and-deploy-mcp](https://github.com/GlobalShopSolutions-InternalTools/cobol-build-and-deploy-mcp)
- **App page:** [cobol-build-and-deploy-mcp on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/cobol-build-and-deploy-mcp)
- **Last reviewed:** 2026-07-02
