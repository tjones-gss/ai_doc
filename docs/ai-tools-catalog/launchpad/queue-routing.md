---
title: "queue-routing"
description: "An MCP server that triages the P&E Bugs queue on Service Web. Analyzes incoming calls, recommends which team queue they belong in, and (when authorized)..."
sidebar_position: 43
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# queue-routing

> **TL;DR** — An MCP server that triages the P&E Bugs queue on Service Web. Analyzes incoming calls, recommends which team queue they belong in, and (when authorized) executes the transfer with code-impact analysis notes. Has a webhook endpoint for Power Automate integration.

## Overview

P&E Bugs gets a constant flow of inbound service calls, and getting each call to the right team queue is a recurring drain on team leads. `queue-routing` automates the analysis: it reads the call, looks at the affected programs / modules, and recommends the team most likely to own the fix. Where authorized, it can execute the transfer too, adding code-impact notes to the call as it goes.

## Why use it

- **Faster intake.** Calls reach the right team without a human in the middle.
- **Better routing.** The MCP looks at code impact, not just keywords — so the routing tends to be more accurate than gut feel.
- **Automation-friendly.** The webhook endpoint lets Power Automate kick off routing as soon as a call is created.

## When to use it

- You're a team lead reviewing inbound P&E Bugs calls.
- You're building automation around bug intake.
- You're triaging a call and want a second opinion on which team should own it.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/queue-routing](https://launchpad.globalshopsolutions.dev/apps/queue-routing) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Kind:** MCP server.

## Common questions

**Will it ever auto-transfer without approval?**
Auto-transfer behavior depends on configuration. Confirm with the owner what's currently enabled for your team before assuming. Default is recommend-only.

**What does "code impact analysis" mean?**
The MCP looks at the programs affected by the call and figures out which team owns those programs. The recommendation is based on actual code ownership, not just text in the call.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [queue-routing on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/queue-routing)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/queue-routing](https://github.com/GlobalShopSolutions-InternalTools/queue-routing)
- **Related apps:** [bug-traffic-dashboard](bug-traffic-dashboard.md), [bug-triage-pack](bug-triage-pack.md)
- **Last reviewed:** 2026-05-05
