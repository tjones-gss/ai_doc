---
title: "cobol-mcp"
description: "An MCP server hosted on LaunchPad that lets AI assistants reach into our COBOL source code repo and SVN log history. The primary way Claude Code answers..."
sidebar_position: 15
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — An MCP server hosted on LaunchPad that lets AI assistants reach into our COBOL source code repo and SVN log history. The primary way Claude Code answers COBOL questions at GSS.

## Overview

cobol-mcp wraps a kept-up-to-date mirror of GSSERP's COBOL source code and SVN log data, exposing it over the Model Context Protocol (MCP). Once an AI client (like Claude Code) is connected, it can search programs by name, read file contents, and look up commit history without you doing the legwork.

It's the LaunchPad-hosted face of the [cobol-codebase MCP server](../mcp-servers/cobol-codebase.md) — same backing data, exposed for any team's Claude Code to use.

## Why use it

GSSERP's COBOL codebase is huge and old. Browsing it by hand to answer "what does program X do" or "when did the bug in PROC-LOAD-SCREEN start" eats hours. With cobol-mcp connected, those become one-question, one-answer interactions in Claude Code.

It's also the data source behind automated COBOL refactors (e.g., SP2-to-SCR100 grid conversions) that need to read source files and SVN log to do their work.

## When to use it

Anytime a Claude Code task involves COBOL source. The MCP is connected by default in the GSS Claude Code setup; no per-task action is needed.

Don't use it for:
- Modifying COBOL source — this MCP is read-only. Edits happen in your local working copy.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/cobol-mcp](https://launchpad.globalshopsolutions.dev/apps/cobol-mcp)
- **Login:** Sign in with your Global Shop Office 365 account (SAML).
- **Status:** Live.
- **As an MCP server:** Already wired into Claude Code if you're using the standard GSS setup. Otherwise, get the connection details from the LaunchPad app page.

## How to use it

You don't need to "use" the LaunchPad page directly except to confirm it's running or grab connection details. The tool is consumed by Claude Code (and any other MCP-compatible client) automatically.

Example Claude Code prompts that lean on it:

- "What does GL0025 do?"
- "Find every COBOL program that touches the IM_INVENTORY file."
- "When was PROC-LOAD-SCREEN last changed in JB0551?"

## Common questions

**How fresh is the data?**
The COBOL source mirror is kept up to date — exact refresh cadence is documented on the LaunchPad page. If something looks stale, ping the owner.

**Can it write or commit?**
No. Read-only.

**Is the SVN log searchable?**
Yes — Claude can ask for log entries by file, author, or date range.

## How it works

cobol-mcp is a small service (LaunchPad-hosted) that wraps the COBOL repo and SVN log behind MCP-compatible search and fetch operations. AI clients connect over MCP and call those operations as tools during a session.

## Owner & support

- **Owner:** *Owner listed on the LaunchPad app page — confirm before publishing.*
- **App page:** [cobol-mcp on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/cobol-mcp)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/cobol-mcp](https://github.com/GlobalShopSolutions-InternalTools/cobol-mcp)
- **Last reviewed:** 2026-05-05
