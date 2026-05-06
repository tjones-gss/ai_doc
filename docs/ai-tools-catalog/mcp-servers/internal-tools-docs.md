---
title: "MCP Server: internal-tools-docs"
description: "Lets AI assistants search the internal-tools documentation site from inside an AI agent session. Saves you from copy-pasting from the docs portal."
sidebar_position: 5
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

# MCP Server: internal-tools-docs

> **TL;DR** — Lets AI assistants search the internal-tools documentation site from inside an AI agent session. Saves you from copy-pasting from the docs portal.

## Overview

GSS has an internal documentation site for our home-grown tooling — runbooks, how-tos, troubleshooting guides. The `internal-tools-docs` MCP server exposes search and content-fetch over MCP so your AI agent can answer questions like "how do I rotate the artifact-mirroring credentials?" without you leaving the terminal.

## Why use it

- **No tab-switching.** Ask your AI agent a question; it pulls the docs and answers in context.
- **Better answers.** Your AI agent has the actual documentation, not a guess from training data.

## When to use it

Auto-triggers when you ask your AI agent something that sounds like it lives in the internal docs. You can also be explicit:

> "Check the internal-tools docs for how to set up a new LaunchPad app."

Don't use it for:
- Questions about external tooling (use the relevant docs MCP instead — e.g., `context7` for public library docs).
- Questions about the *code* behind a tool — that's `mcp-artifacts` or the actual repo.

## How to use it

You don't invoke it directly. As long as the MCP is connected (it is, by default), your AI agent can use it when the question fits.

## Common questions

**What docs does it cover?**
The internal-tools documentation site at GSS. Ask the engineering platform team for the canonical URL.

**Does it work with multiple AI agents?**
The MCP itself is generic — any MCP-compatible client can use it. All of our AI agents use it the same way through MCP.

## How it works

A backend that wraps the internal-tools docs index, exposing `search` and `get_content` operations over MCP. Your AI agent calls those when answering documentation-flavored questions.

## Owner & support

- **Owner:** GSS engineering platform team
- **Authentication:** Tied to your GSS SSO — first use prompts an auth flow.
- **Last reviewed:** 2026-05-04
