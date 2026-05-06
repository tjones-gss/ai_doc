---
title: "svn-ops (svn-mcp)"
description: "A general-purpose Subversion MCP server. Exposes the full SVN command surface (checkout, log, diff, blame, commit, branch, etc.) to Claude Code and othe..."
sidebar_position: 35
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A general-purpose Subversion MCP server. Exposes the full SVN command surface (checkout, log, diff, blame, commit, branch, etc.) to Claude Code and other MCP clients over Streamable HTTP. The repo is named `svn-ops`; the service identifies itself as `svn-mcp`.

## Overview

GSSERP still lives in SVN, but most modern AI tooling assumes Git. `svn-ops` bridges the gap: a self-contained service that runs SVN operations on managed working copies and exposes them as MCP tools. Any consumer that speaks MCP — `team-3-app`, Cursor agents, custom Claude Code skills — can call SVN ops via `call_proxy_tool(server="svn-mcp", ...)` through MCP-Intelligence.

## Why use it

- **AI-friendly SVN.** Lets agents do real source-control work without invoking the SVN CLI directly.
- **Workspace management.** The service handles the lifecycle of working copies — check out, update, clean up — so agents don't need to manage filesystem state.
- **One service, many consumers.** Registered downstream of MCP-Intelligence; any consumer can use it.

## When to use it

- You're building an AI workflow that needs to read or modify SVN-tracked code.
- You want history (`svn log`, `svn blame`, `svn diff`) accessible to Claude without the user pasting in command output.
- You're building an automated branch / patch / commit flow.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/svn-ops](https://launchpad.globalshopsolutions.dev/apps/svn-ops) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Configured (not yet Live).
- **Kind:** MCP server.

## Tools exposed

| Tool | Description |
|---|---|
| `health` | Server health check. |
| `svn_checkout` | Check out an SVN URL into a managed workspace. |
| `svn_update` | Update an existing working copy. |
| `svn_info` | Repository / file metadata. |
| `svn_log` | Commit history with filters. |
| `svn_diff` | Diff between revisions or working-copy changes. |
| `svn_cat` | Read a file at a specific revision (no checkout needed). |
| `svn_status` | Working-copy modification status. |
| `svn_blame` | Line-by-line annotation. |
| `svn_list` | List directory contents. |
| `svn_apply_patch` | Apply a unified diff to a working copy. |
| `svn_commit` | Commit working-copy changes. |
| `svn_revert` | Revert working-copy modifications. |
| `svn_copy` | Server-side copy (branch creation). |
| `svn_list_workspaces` | List managed workspaces. |
| `svn_delete_workspace` | Clean up a workspace. |

## How it works

A Python service exposing MCP Streamable HTTP at `/mcp/`. Workspaces live under a configurable root (`/tmp/svn-workspaces` by default), with a per-server cap on concurrent working copies. SVN credentials are injected via Kubernetes External Secrets — they're never written into the catalog or the repo.

## Common questions

**Can it commit on my behalf?**
`svn_commit` is exposed as a tool, but the consuming agent should always confirm before doing so. Treat write operations like any other destructive action.

**Does it work over the GSS internal SVN?**
Yes — that's the primary use case. The default repository URL points at `gss2k19svn3.gss.local/svn/gsserp/trunk/cobol`.

**Why is the repo named `svn-ops` but the service `svn-mcp`?**
The repo was created via the LaunchPad scaffold (which uses the app slug). The service was renamed to `svn-mcp` for clarity since the slug `svn-ops` is generic. They're the same project.

## Owner & support

- **Owner:** zsandford@gssmail.com
- **App page:** [svn-ops on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/svn-ops)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/svn-ops](https://github.com/GlobalShopSolutions-InternalTools/svn-ops)
- **Related apps:** [cobol-mcp](cobol-mcp.md) (which exposes COBOL repo + SVN log via a more focused interface)
- **Last reviewed:** 2026-05-06
