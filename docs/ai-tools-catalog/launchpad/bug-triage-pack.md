---
title: "bug-triage-pack"
description: "An MCP gateway server that bundles COBOL, GitHub, Service Web, and test-case tools behind a single endpoint, plus a curated set of Skills, Agents, and R..."
sidebar_position: 12
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# bug-triage-pack

> **TL;DR** — An MCP gateway server that bundles COBOL, GitHub, Service Web, and test-case tools behind a single endpoint, plus a curated set of Skills, Agents, and Rules tuned for triaging bugs. Drop one MCP into your AI agent and you've got the full bug-triage toolkit.

## Overview

Bug triage at GSS pulls from many sources: the COBOL repo, GitHub PRs, Service Web service calls, test cases. Wiring all those up individually for each AI agent is fiddly. `bug-triage-pack` is a single MCP gateway that aggregates them, plus packaged Skills/Agents/Rules so your AI agent approaches a bug the way a senior engineer would.

## Why use it

- **One MCP instead of five.** Reduces configuration sprawl in your AI agent.
- **Triage-tuned defaults.** The bundled rules and skills bias the agent toward useful triage behavior (find similar past bugs, suggest a routing team, identify code impact).
- **Power Automate integration.** A webhook endpoint lets Power Automate kick off triage flows automatically.

## When to use it

- A new service call lands on the P&E Bugs queue and you need it triaged fast.
- You're a team lead reviewing inbound calls before assignment.
- You're building automation around bug intake.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/bug-triage-pack](https://launchpad.globalshopsolutions.dev/apps/bug-triage-pack) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Kind:** MCP server.

## How to use it

The MCP exposes its tools to any connected AI agent session. Typical prompts:

- "Triage call GLO010-XXXX-XX — find similar past bugs and recommend routing."
- "What's the code impact of this fix? Which tests should run?"

## Common questions

**Does it modify Service Web records?**
It can execute transfers (queue routing) — that's part of its scope. Always review before approving.

**What's the Power Automate hook?**
A webhook endpoint that lets Power Automate workflows kick off triage. Useful for auto-triaging high-priority calls as soon as they're created.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [bug-triage-pack on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/bug-triage-pack)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/bug-triage-pack](https://github.com/GlobalShopSolutions-InternalTools/bug-triage-pack)
- **Related apps:** [queue-routing](queue-routing.md), [bug-traffic-dashboard](bug-traffic-dashboard.md), [cobol-mcp](cobol-mcp.md)
- **Last reviewed:** 2026-05-05
