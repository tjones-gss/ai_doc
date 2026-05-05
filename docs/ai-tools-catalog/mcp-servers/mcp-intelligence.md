---
title: "MCP Server: mcp-intelligence"
description: "The shared \"memory\" layer for AI tools at GSS. When Claude Code (or any other AI agent) finishes a meaningful piece of work — a deployment, a bug fix, a..."
sidebar_position: 7
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

> **TL;DR** — The shared "memory" layer for AI tools at GSS. When Claude Code (or any other AI agent) finishes a meaningful piece of work — a deployment, a bug fix, a design decision — it logs the outcome here. Future sessions can search that history and pick up where the last one left off.

## Overview

`mcp-intelligence` is the **development intelligence engine** for AI tools at GSS. It accumulates organizational knowledge from MCP interactions, serves team-aware AI artifacts (skills, rules, agents), and exposes a REST API for upstream consumers. It's a LaunchPad-hosted MCP server.

It gives AI agents four core actions:

- **`intel_add_timeline`** — log a significant event (deploy, feature ship, bug fix, spike).
- **`intel_write_doc`** — create or update an internal doc page (apps, infra, troubleshooting).
- **`intel_log_resolution`** — capture how a bug was resolved, so the next person triaging similar symptoms can find it.
- **`intel_log_decision`** — record an architecture or design decision and its rationale.

The data sits in a central store the whole team's AI tools can read from. The companion app [mcp-artifacts](../launchpad/mcp-artifacts.md) hosts the team-organized library of skills, rules, and agent definitions that intelligence reads at runtime.

## Why use it

Two big problems it solves:

1. **AI session amnesia.** Each Claude Code session starts fresh. Without `mcp-intelligence`, the next session has to rediscover everything — including stuff the prior session figured out 10 minutes ago.
2. **Tribal knowledge loss.** When someone resolves a tricky bug or makes a non-obvious design call, that knowledge usually lives only in their head (or an Outlook thread). Logging it here makes it searchable for everyone, human or AI.

## When to use it

The intelligence MCP is enabled in every Claude Code session by default. The instruction Claude reads (from your global `CLAUDE.md`) is:

- **`intel_add_timeline`** — after deploying, shipping a feature, fixing a major bug, or completing a spike.
- **`intel_write_doc`** — when creating or updating an app/infra/troubleshooting page.
- **`intel_log_resolution`** — when resolving a bug.
- **`intel_log_decision`** — when making an architecture or design decision.

Don't use it for:
- Trivial changes (a typo fix, a one-liner). Threshold is "meaningful progress."
- Sensitive material — anything you wouldn't paste into a regular internal doc.

## How to use it

You don't call `mcp-intelligence` directly — Claude Code does. As long as the server is running and connected (it is, by default in our setup), Claude will log events automatically when the work fits the criteria.

If you want to view or search the logged data, see the **mcp-artifacts** LaunchPad app (which is a UI on top of the intelligence store).

## Common questions

**Where is the data stored?**
Internally — not pushed to any third-party AI vendor. Ask the GSS engineering platform team for specifics on the backing store and retention policy.

**Who can see what I log?**
Anyone at GSS with access to the AI tooling — same audience as Helpjuice or Confluence.

**Can I edit a logged entry?**
Yes — `intel_write_doc` updates pages in place. Timeline entries and resolution logs are append-only by design (they're a record of what happened, when).

## How it works

`mcp-intelligence` is a small backend that exposes its functions over the Model Context Protocol (MCP). Claude (and other AI clients that support MCP) connects to it, sees the available tools, and calls them when prompted by user instructions or its system context.

## Owner & support

- **Owner:** GSS engineering platform team
- **App page:** [mcp-intelligence on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/mcp-intelligence) (it's a LaunchPad app + MCP server in one)
- **Related app:** [mcp-artifacts](../launchpad/mcp-artifacts.md) — the artifact library the intelligence layer reads from
- **Last reviewed:** 2026-05-05
