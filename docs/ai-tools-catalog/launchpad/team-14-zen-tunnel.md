---
title: "team-14-zen-tunnel"
description: "A read-only FastAPI + MCP service for querying GLOBAL51Q and COMMON51Q from Team 14's AI alerts platform. Lets AI agents draft SQL natively (Vanna-style..."
sidebar_position: 52
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A read-only FastAPI + MCP service for querying `GLOBAL51Q` and `COMMON51Q` from Team 14's AI alerts platform. Lets AI agents draft SQL natively (Vanna-style) for analyst prompts, with row-limited, JWT-protected access.

## Overview

Team 14 runs an AI alerts platform that surfaces anomalies in `GLOBAL51Q` and `COMMON51Q` data. Analysts need to drill into those datasets often — and asking an LLM to "look at this data and tell me X" works much better when the LLM can run real SQL against the live tables instead of guessing from snippets.

`team-14-zen-tunnel` is the gateway: a Streamable-HTTP MCP service plus REST endpoints, both read-only, both row-limited, both JWT-protected. It includes Vanna-native SQL drafting so an analyst's natural-language prompt can become a real SQL query, executed safely.

## Why use it

- **Real data, real SQL.** Beats LLM hallucination by orders of magnitude.
- **Read-only and row-limited.** Can't accidentally take down prod or export the world.
- **MCP-native.** Any MCP-compatible AI agent picks it up.

## When to use it

- You're an analyst on Team 14 (or adjacent) digging into AI-alert data.
- You want your AI agent to answer data questions with actual queries, not handwaving.

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/team-14-zen-tunnel](https://launchpad.globalshopsolutions.dev/apps/team-14-zen-tunnel)
- **MCP endpoint:** `/mcp` (Streamable HTTP, JWT-protected) and `/webhook/mcp` (no JWT, for testing)
- **Legacy SSE MCP:** `GET /sse` + `POST /messages/?session_id=…`
- **Login:** Global Shop Office 365 SSO (for the LaunchPad page); JWT for direct API.

## How it works

- **Stack:** FastAPI + Streamable HTTP MCP.
- **Data:** `GLOBAL51Q` and `COMMON51Q` via Zen, read-only and row-limited.
- **SQL drafting:** Vanna-native — the LLM gets schema context and produces real SQL.
- **Storage:** Persistent volume for Chroma + SQLite (Vanna agent memory survives pod restarts).
- **Secrets:** OpenAI / Anthropic / Zen — held in the platform secrets UI, never in git.

## Common questions

**Can it write to the database?**
No — read-only by design. Row limits prevent runaway queries.

**Does it work for any team's data?**
Today, scoped to Team 14's `GLOBAL51Q` / `COMMON51Q`. Other teams could spin up similar tunnels for their datasets.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page (Team 14).*
- **App page:** [team-14-zen-tunnel on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/team-14-zen-tunnel)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/team-14-zen-tunnel](https://github.com/GlobalShopSolutions-InternalTools/team-14-zen-tunnel)
- **Last reviewed:** 2026-05-07
