---
title: "MCP Server: Notion"
description: "Lets your AI agent search and read Notion content (pages, databases, blocks) using your GSS Notion account. Useful when team docs/specs/notes live in No..."
sidebar_position: 10
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

# MCP Server: Notion

> **TL;DR** — Lets your AI agent search and read Notion content (pages, databases, blocks) using your GSS Notion account. Useful when team docs/specs/notes live in Notion rather than Helpjuice or SharePoint.

## Overview

Notion holds a lot of in-flight team work: specs, retros, meeting notes, internal product wiki. Without a bridge, AI agents can't see any of it. The Notion MCP exposes Notion's API to your AI agent so it can:

- Search across your workspace.
- Read specific pages or database entries.
- (Per token scope) update pages, append blocks, or create new content.

## Why use it

- **Spec / requirements lookups.** Pull the original product spec from Notion when asked to implement against it.
- **Meeting context.** "What did we decide about X in last week's retro?" becomes a one-question lookup.
- **Doc generation.** Your AI agent can summarize a page or roll up multiple pages into a status update.

## When to use it

Auto-triggers when a task involves content you keep in Notion. You can also ask explicitly: "search Notion for the launchpad app store spec."

## How to access it

- **MCP:** Available in the GSS AI tooling setup. **Requires auth on first use** — sign in with your GSS Notion account.
- **Auth flow:** OAuth. Tokens are scoped to your account; you only see what your account can see.

## Common questions

**Can it edit Notion pages?**
Depending on token scope. Read-only is the safest default. If write is granted, your AI agent will still confirm before destructive actions.

**Does my Notion content go to Anthropic?**
Your AI agent reads the parts it needs to answer your question, and those parts are sent to the model provider's API (Anthropic, OpenAI, etc.) for response generation. Treat per the usual data-handling rules.

## Owner & support

- **Vendor:** Notion (official MCP)
- **GSS account/access:** Use your GSS Notion account.
- **Last reviewed:** 2026-05-05
