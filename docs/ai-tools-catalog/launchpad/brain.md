---
title: "brain"
description: "The institutional-memory layer for GSS. Passively collects knowledge from developer interactions, tickets, and tooling, then makes it queryable with hyb..."
sidebar_position: 13
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The institutional-memory layer for GSS. Passively collects knowledge from developer interactions, tickets, and tooling, then makes it queryable with hybrid search — so the next person who hits the same problem finds what the org already knows. This is the top of the GSS AI platform.

## Overview

Most of what GSS engineering knows lives in someone's head, a Slack thread, or a closed ticket. The Brain captures all of that into a queryable store and exposes it back to AI agents and humans alike.

Architecturally, the Brain sits at the top of a three-layer AI platform:

| Layer | Component | Role |
|---|---|---|
| Layer 0 | **The Brain** | Knowledge store + query engine *(this app)* |
| Layer 1 | [mcp-intelligence](../mcp-servers/mcp-intelligence.md) | MCP bridge to developer tools (Cursor, Claude Code, etc.) |
| Layer 2 | [mcp-artifacts](mcp-artifacts.md) | Team skills, rules, agent definitions |

When your AI agent needs context — a past resolution, a design decision, a tribal piece of knowledge — it asks `mcp-intelligence`, which queries the Brain.

## Why use it

- **Find what's already known.** Most "new" problems have been solved before; the Brain is where past resolutions live.
- **Continuity across people.** When somebody leaves a team, their know-how doesn't have to leave with them.
- **AI context, on tap.** Your agent automatically gets relevant org knowledge in its prompt without you copy-pasting from old tickets.

## When to use it

You don't usually call the Brain directly — your AI agent does, via mcp-intelligence. You might browse it directly when:

- You want to see what the org has captured about a topic.
- You're checking whether something is in the knowledge base before adding it.
- You're auditing what's getting captured.

## How to access it

- **Live:** [https://brain.globalshopsolutions.dev](https://brain.globalshopsolutions.dev)
- **LaunchPad page:** [https://launchpad.globalshopsolutions.dev/apps/brain](https://launchpad.globalshopsolutions.dev/apps/brain) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How it works

- Hybrid search: combines vector (semantic) and keyword (lexical) retrieval.
- Passively ingests from developer interactions, tickets, and connected tooling — no manual write step required for most content.
- Surfaces results to AI clients via the mcp-intelligence proxy.

## Owner & support

- **Owner:** hfutrell@gssmail.com
- **App page:** [brain on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/brain)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/brain](https://github.com/GlobalShopSolutions-InternalTools/brain)
- **Related apps:** [mcp-intelligence](../mcp-servers/mcp-intelligence.md), [mcp-artifacts](mcp-artifacts.md)
- **Last reviewed:** 2026-05-07
