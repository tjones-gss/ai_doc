---
title: "MCP Server: cobol-codebase"
description: "Gives AI assistants read access to our COBOL source repository and SVN log history. The backbone behind the cobol-mcp LaunchPad app."
sidebar_position: 2
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

> **TL;DR** — Gives AI assistants read access to our COBOL source repository and SVN log history. The backbone behind the [cobol-mcp](../launchpad/cobol-mcp.md) LaunchPad app.

## Overview

GSSERP's COBOL codebase is the legacy heart of the product — millions of lines of COBOL across thousands of programs, with a long SVN history. Most AI tools can't reach into it directly. `cobol-codebase` is the MCP that bridges the gap: it exposes the COBOL repo and SVN log to Claude Code so the assistant can answer questions like "what does program GL0025 do?" or "when was function PROC-LOAD-SCREEN last touched, and by whom?".

## Why use it

- **Onboarding.** New devs (or anyone unfamiliar with a specific program) can ask Claude what it does instead of grepping by hand.
- **Bug archaeology.** "When did this behavior change?" becomes a question Claude can answer from the SVN log.
- **Conversion work.** Automated COBOL refactors (e.g., SP2-to-SCR100 grid conversions) lean on this MCP to read the source they're converting.

## When to use it

Anytime you're working on a COBOL program in Claude Code and need to look something up in the broader codebase. Auto-triggers based on context.

## How it works

The MCP server has read-only access to a maintained mirror of the COBOL source repo and the SVN log data. When Claude needs to look something up, it calls the MCP, which returns the relevant file contents or log entries.

## Owner & support

- **Owner:** *Confirm with cobol-mcp app owner on LaunchPad.*
- **Related app:** [cobol-mcp on LaunchPad](../launchpad/cobol-mcp.md)
- **Last reviewed:** 2026-05-05
