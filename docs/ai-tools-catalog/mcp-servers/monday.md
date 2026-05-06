---
title: "MCP Server: monday.com"
description: "Lets your AI agent read and update monday.com boards, items, columns, and updates using your GSS monday account. The bridge between AI assistants and ou..."
sidebar_position: 9
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

# MCP Server: monday.com

> **TL;DR** — Lets your AI agent read and update monday.com boards, items, columns, and updates using your GSS monday account. The bridge between AI assistants and our active project boards.

## Overview

monday.com is one of GSS's project-tracking surfaces. The monday MCP exposes its API to AI agents so your AI agent can:

- Search boards and items.
- Read columns (status, owner, due date, custom fields).
- Add updates / comments to items.
- (Per token scope) move items between groups, change column values, create new items.

## Why use it

- **Board context in conversation.** "What's the status of the artifact-mirror migration on the Platform board?" without opening a browser.
- **Update from the terminal.** Drop a note on a monday item directly from an AI agent session.
- **Cross-tool automation.** Your AI agent can read a GitHub PR (via the [GitHub MCP](github.md)) and post a status update to the corresponding monday item in one move.

## When to use it

- You're tracking work on a monday board and want to read or update it.
- You want your AI agent to roll up board status into a summary.
- You're triaging incoming items and want a fast bulk view.

## How to access it

- **MCP:** Available in the GSS AI tooling setup. **Requires auth on first use** — sign in with your GSS monday account.
- **Auth flow:** OAuth.

## Common questions

**Can it create or move items?**
Yes, depending on token scope. Your AI agent will confirm before destructive actions.

**Does my monday data go to Anthropic?**
Same as any AI tool — what your AI agent reads is sent to the model provider's API (Anthropic, OpenAI, etc.) to produce a response. Don't ask your AI agent to read boards with customer-confidential data without clearance.

## Owner & support

- **Vendor:** monday.com (official MCP)
- **GSS account/access:** Use your GSS monday account.
- **Last reviewed:** 2026-05-05
