---
title: "service-web-api"
description: "A modern .NET 9 Minimal API exposing all IHOPNet data across 17 domains. Replaces the legacy SWCallService.Api with pure inline SQL (no stored procedure..."
sidebar_position: 46
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A modern .NET 9 Minimal API exposing all IHOP_Net data across 17 domains. Replaces the legacy `SW_CallService.Api` with pure inline SQL (no stored procedures), interactive OpenAPI docs, telemetry, and a built-in MCP server for AI consumers.

## Overview

The legacy Service Web API (`SW_CallService.Api`) was a maze of stored-procedure calls. The new `service-web-api` is a clean ASP.NET Core 9 Minimal API:

- **17 domains covered** — calls, customers, programs, parts, schedules, etc.
- **Pure inline SQL** — no stored-procedure mystery layer; the query you see is what runs.
- **OpenAPI / Scalar docs** at `/scalar` — interactive, browser-testable.
- **Telemetry** built in.
- **MCP server** at `/mcp` — Streamable HTTP, stateless. AI agents (Cursor, Claude Code, Codex) can query Service Web data directly.

## Why use it

- **For developers.** Cleaner API surface than the legacy one. Easier to integrate against.
- **For AI agents.** The `/mcp` endpoint exposes Service Web tools to any MCP client. Combined with the [easy MCP setup](../mcp-servers/mcp-intelligence.md), agents pick this up automatically.
- **For ops.** Telemetry + clear queries make production debugging tractable.

## When to use it

- You're building a new internal tool that needs Service Web data.
- You're migrating a tool off the legacy `SW_CallService.Api`.
- You want AI agents to be able to answer Service Web questions natively.

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/service-web-api](https://launchpad.globalshopsolutions.dev/apps/service-web-api)
- **Health check:** `service-web-api.globalshopsolutions.dev/health`
- **API docs (Scalar):** `service-web-api.globalshopsolutions.dev/scalar`
- **MCP endpoint:** `service-web-api.globalshopsolutions.dev/mcp`
- **Login:** Global Shop Office 365 SSO.

## How it works

- **Stack:** .NET 9 Minimal API, ASP.NET Core, inline SQL via Dapper-style helpers, OpenAPI via Scalar.
- **MCP layer:** Streamable HTTP, stateless — any MCP client can connect.
- **Auth:** Browser SSO for `/scalar`; JWT bearer for `/api/*`.

## Common questions

**Does it replace `SW_CallService.Api` everywhere?**
That's the goal. Migration is in progress; the legacy API still exists for clients that haven't moved.

**Why no stored procedures?**
Inline SQL is easier to reason about, easier to change, easier to AI-assist. The legacy stored procedures hid logic in places far from the calling code.

**Is the MCP read-only?**
Confirm with the owner — most MCP read tools are safe; any write actions require explicit confirmation.

## Owner & support

- **Owner:** cmasden@gssmail.com
- **App page:** [service-web-api on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/service-web-api)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/service-web-api](https://github.com/GlobalShopSolutions-InternalTools/service-web-api)
- **Related apps:** [issue-maintenance-api](issue-maintenance-api.md), [bug-traffic-dashboard](bug-traffic-dashboard.md), [queue-routing](queue-routing.md)
- **Last reviewed:** 2026-05-07
