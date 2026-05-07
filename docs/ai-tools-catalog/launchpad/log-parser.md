---
title: "log-parser"
description: "An MCP server that parses GSS SP2 log files (CoreLog, GSSEO, OCTSRS, ACU COBOL traces) into structured, noise-filtered records that AI assistants can ac..."
sidebar_position: 35
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# log-parser

> **TL;DR** — An MCP server that parses GSS SP2 log files (CoreLog, GSSEO, OCTSRS, ACU COBOL traces) into structured, noise-filtered records that AI assistants can actually reason about. Replaces brittle line-by-line log reading with a proper parser.

## Overview

SP2 log files are messy. Each format has its own quirks:

- **CoreLog files** are about 50% noise — `Program Unoptimized` warnings on every other line.
- **GSSEO logs** have multi-line records where continuation lines (e.g., `Key value:`, `TranslatedMessage` blocks) lack the standard pipe prefix.
- **OCTSRS logs** embed large JSON payloads (HookFile contents) inline.
- **ACU COBOL traces** carry ~26% noise from DLL search-path probing and `DLL_CONVENTION` toggles, plus a header block with no timestamps.
- All formats routinely exceed 100K characters, which makes naive windowed reading unreliable.

`log-parser` does the parsing server-side and returns structured records. Your AI agent (or any MCP client) gets clean, query-friendly data instead of having to wade through raw text.

## Why use it

- **Faster triage.** Search a log by severity, program, time range, or pattern — no eyeballing.
- **Cross-log correlation.** Merge records from CoreLog + GSSEO + OCTSRS by timestamp into one stream.
- **Performance hotspots.** `log_performance_hotspots` finds the slowest operations and time gaps.
- **Call graph reconstruction.** `log_call_graph` builds a hierarchical program call tree from `Entering`/`Leaving` patterns.
- **Entity extraction.** Pulls company code, user, database type, feature flags, programs, and environment settings out of a log.

## When to use it

- Triaging a customer log dump and looking for the actual error among the noise.
- Comparing performance between two log captures.
- Reconstructing what a session was doing right before a crash.

Don't use it for:
- Real-time tailing — it's a parse-then-query model, not a live tail.
- Non-SP2 logs.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/log-parser](https://launchpad.globalshopsolutions.dev/apps/log-parser) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Configured (not yet Live — confirm with owner).
- **Kind:** MCP server.

## How to use it

1. **Upload the log.** Two paths:
   - **Preferred:** HTTP upload via the `mcp-intelligence` relay using a Bearer token. Files up to 50 MB; no token cost for content.
   - **Fallback:** small files (under 100 KB) can be inlined via the `upload_log` MCP tool.
2. **Parse and query.** Available tools include:
   - `parse_log(path)` — auto-detect format, return metadata.
   - `search_log(...)` — filter by severity / program / pattern / time range.
   - `get_errors(path)` — all ERROR/WARN records, grouped.
   - `summarize_log(path)` — one-shot triage overview.
   - `correlate_logs(paths[])` — merge multiple logs by timestamp.
   - `log_call_graph(path)` — program call tree (CoreLog only).
   - `log_performance_hotspots(path)` — slow ops and time gaps.
   - `log_entities(path)` — structured business entities (company code, user, etc.).

## Supported formats

| Format | File pattern | Multi-line records | Noise filtering |
|---|---|---|---|
| CoreLog | `CoreLog*.glog` | No | `Program Unoptimized` spam (~50%) |
| GSSEO | `GSSEOLog*.glog` | Yes (key/translated-message blocks) | — |
| OCTSRS | `octsrs.*.debug` | Yes (JSON payloads, IPM data) | — |
| ACU Trace | `TRAC*.trc` | No | DLL search probing, `DLL_CONVENTION` toggles (~26%) |

## How it works

A Python (FastMCP) service hosted on LaunchPad. Receives log uploads, parses them per-format, and exposes search/analysis tools over MCP Streamable HTTP. The `mcp-intelligence` relay handles the file-upload path so consumers don't need to manage tokens themselves for content transfer.

## Owner & support

- **Owner:** zsandford@gssmail.com
- **App page:** [log-parser on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/log-parser)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/log-parser](https://github.com/GlobalShopSolutions-InternalTools/log-parser)
- **Related apps:** [zen-log-parser](zen-log-parser.md) (Pervasive Zen DB logs, UI-based), [mcp-intelligence](../mcp-servers/mcp-intelligence.md) (the upload relay path)
- **Last reviewed:** 2026-05-06
