---
title: "qTest-MCP-Server"
description: "A read-only MCP server that lets AI agents query qTest Manager — test cases, runs, requirements, defects, and more. 18 tools, all read-only; nothing in ..."
sidebar_position: 41
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A read-only MCP server that lets AI agents query qTest Manager — test cases, runs, requirements, defects, and more. 18 tools, all read-only; nothing in qTest gets modified. Designed for Cursor but works with any MCP-compatible AI agent.

## Overview

qTest Manager is GSS's test-management hub at `globalshop.qtestnet.com`. Browsing it by hand from inside an AI session is friction — you context-switch, copy-paste, lose flow. The qTest MCP Server gives your AI agent direct read access: ask "which test cases cover the customer login flow?" and the agent gets a structured answer.

All 18 tools are **read-only** by design. Nothing in qTest is modified by the MCP. This makes it safe to attach to any AI agent without worrying about accidental changes to test data.

## Why use it

- **Test discoverability.** "Are there existing tests for X?" becomes a one-question lookup.
- **Coverage analysis.** Cross-reference tests with code changes during PR review.
- **Defect context.** When investigating an issue, pull related defects without opening qTest manually.
- **Read-only safety.** No risk of an agent fat-fingering test data.

## When to use it

- You're triaging a bug and want related qTest data in your AI agent's context.
- You're scoping a new feature and want to see existing test coverage.
- You're preparing a release and need a coverage snapshot.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/qTest-MCP-Server](https://launchpad.globalshopsolutions.dev/apps/qTest-MCP-Server)
- **Login:** Global Shop Office 365 SSO (LaunchPad).
- **As an MCP:** Connected by default if you've followed the [easy MCP setup](../mcp-servers/mcp-intelligence.md) (`mcp-intelligence` proxies it). Otherwise, ~5 minutes of setup per the repo's README.
- **Prerequisites:** qTest access at `globalshop.qtestnet.com`, Cursor or another MCP client, Python.

## Common questions

**Can it create or modify test cases?**
No — read-only by design. All 18 tools are query / fetch only.

**Does it work with Cursor specifically?**
Yes, that's the primary target. Also works with Claude Code, Codex, and any other MCP-compatible client.

**What auth does it need?**
A qTest API token. The repo's README walks through getting one (~5 minutes).

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [qTest-MCP-Server on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/qTest-MCP-Server)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/qTest-MCP-Server](https://github.com/GlobalShopSolutions-InternalTools/qTest-MCP-Server)
- **Related apps:** [testarchitect-mcp](testarchitect-mcp.md), [Test-Case-Dashboard](Test-Case-Dashboard.md)
- **Last reviewed:** 2026-05-07
