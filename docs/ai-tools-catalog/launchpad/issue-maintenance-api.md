---
title: "issue-maintenance-api"
description: "The JWT-authenticated, container-hosted version of the Issue Maintenance API. Same surface as the legacy IIS version, but runs on the LaunchPad platform..."
sidebar_position: 32
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The JWT-authenticated, container-hosted version of the Issue Maintenance API. Same surface as the legacy IIS version, but runs on the LaunchPad platform with platform-provided auth. Lets web apps and AI agents read/manage issues programmatically.

## Overview

Issue Maintenance has historically been an IIS-hosted .NET API with Windows Authentication. That works for desktop tools but is friction for web apps and AI agents. `issue-maintenance-api` is the same API, ported to .NET 8 Minimal API + container, deployed on LaunchPad with JWT bearer auth.

Both versions share the `/_/` URL prefix to keep code in lockstep — fix a bug here, port the same patch to the IIS one (or vice versa).

## Why use it

- **Web-friendly.** No Windows-Auth headache.
- **Container-hosted.** Same deploy story as everything else on LaunchPad.
- **AI-agent-ready.** JWT-protected endpoints play well with MCP clients.

## When to use it

- You're building a web tool that needs to read or write issues.
- You're wiring up an AI workflow that creates or updates issues.
- You're migrating off the legacy IIS Issue Maintenance API.

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/issue-maintenance-api](https://launchpad.globalshopsolutions.dev/apps/issue-maintenance-api)
- **Live:** `issue-maintenance-api.globalshopsolutions.dev`
- **Interactive docs (Scalar):** at `/scalar` on the live host
- **Login:** Browser SSO for `/scalar` and `/_/*`; JWT bearer for `/api/*`.

## Auth model

| Path | Auth | Source |
|---|---|---|
| `/_/*` | Browser SSO (session cookies) | Gateway injects `X-Forwarded-User` / `X-Forwarded-Email` |
| `/scalar` | Browser SSO | Interactive API docs |
| `/api/*` | JWT Bearer | Platform gateway requires it |
| Dev/test | `X-Test-User` header | Docker / Development environments only |

## Common questions

**Why two versions (IIS and this)?**
Migration path. The IIS version stays for clients on Windows Auth; the LaunchPad version unblocks web/AI clients. Same logic, same `/_/` URL paths so the codebases stay close.

**Does it have an MCP server?**
Confirm with owner. The related [issue-manager-mcp](issue-manager-mcp.md) covers the AI-agent-driven create/update flow.

## Owner & support

- **Owner:** cmasden@gssmail.com
- **App page:** [issue-maintenance-api on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/issue-maintenance-api)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/issue-maintenance-api](https://github.com/GlobalShopSolutions-InternalTools/issue-maintenance-api)
- **Related apps:** [issue-manager-mcp](issue-manager-mcp.md), [program-issue-tracker](program-issue-tracker.md), [CausingIssueDashboard](CausingIssueDashboard.md)
- **Last reviewed:** 2026-05-07
