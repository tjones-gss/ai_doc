---
title: "MCP Server: DevExpress Docs"
description: "Lets your AI agent search and pull DevExpress documentation directly into a session. Saves you from hunting through docs.devexpress.com when wiring up g..."
sidebar_position: 3
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

# MCP Server: DevExpress Docs

> **TL;DR** — Lets your AI agent search and pull DevExpress documentation directly into a session. Saves you from hunting through `docs.devexpress.com` when wiring up grids, charts, or report controls.

## Overview

DevExpress is the third-party UI control library underpinning much of the GSSERP WinForms surface (and the bane of the [designerconsolidation](../launchpad/designerconsolidation.md) tool's existence). Their docs are extensive and not always easy to navigate. The DevExpress Docs MCP server wraps the official documentation index so your AI agent can search and retrieve it on demand.

Two operations:

- **`devexpress_docs_search`** — full-text search across the DevExpress docs.
- **`devexpress_docs_get_content`** — fetch the body of a specific doc page.

## Why use it

- **Faster lookups.** Ask your AI agent how to wire a `GridControl` filter event; it pulls the doc page and answers.
- **Better answers.** Your AI agent has the actual current docs, not its training-data approximation.

## When to use it

You're working on WinForms code that uses DevExpress controls and have a "how do I…" question. Auto-triggers based on context.

## How to access it

- **MCP:** Connected by default in the GSS AI tooling setup.
- **Auth:** No special auth required (public docs).

## Common questions

**Does it cover all DevExpress products?**
The MCP indexes the public DevExpress docs. Coverage matches what's available at [docs.devexpress.com](https://docs.devexpress.com).

**How fresh is the index?**
Refreshes from upstream on a schedule. Confirm cadence with the owner if a brand-new feature isn't found.

## How it works

A small service that wraps the DevExpress documentation site and exposes search/fetch over MCP. Your AI agent calls those tools when relevant.

## Owner & support

- **Owner:** GSS engineering platform team
- **Last reviewed:** 2026-05-05
- **Note:** This MCP was disconnected during one of our sessions. If your AI agent can't reach it, the platform team can re-enable.
