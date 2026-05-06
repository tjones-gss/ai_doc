---
title: "testarchitect-mcp"
description: "An MCP server that gives AI assistants access to TestArchitect test management data. Lets AI agents answer questions about which tests exist, what they ..."
sidebar_position: 36
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# testarchitect-mcp

> **TL;DR** — An MCP server that gives AI assistants access to TestArchitect test management data. Lets AI agents answer questions about which tests exist, what they cover, and how they've been running.

## Overview

TestArchitect is GSS's automated UI test framework. The data it produces — test cases, runs, pass/fail history, coverage — is dense and useful but historically lived behind TA's own UI. `testarchitect-mcp` exposes that data over the Model Context Protocol so your AI agent can search and reason about it as part of normal coding/testing work.

## Why use it

- **Test discoverability.** "Are there existing tests that cover the customer login flow?" becomes a one-question lookup.
- **Coverage decisions.** When adding a feature, ask your AI agent which tests already touch the area and which gaps need new coverage.
- **Run history context.** "Has this test been flaky?" becomes answerable without opening the TA dashboard.

## When to use it

- You're writing or modifying a TestArchitect test and want to see related ones.
- You're triaging a test failure and want history.
- You're scoping coverage for a new feature.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/testarchitect-mcp](https://launchpad.globalshopsolutions.dev/apps/testarchitect-mcp) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Kind:** MCP server.

## How it works

The MCP wraps TestArchitect's data store with read-friendly operations (search, fetch, history) and exposes them as MCP tools. Connected AI agent sessions call those tools when relevant.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [testarchitect-mcp on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/testarchitect-mcp)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/testarchitect-mcp](https://github.com/GlobalShopSolutions-InternalTools/testarchitect-mcp)
- **Related apps:** [TABugTracking](TABugTracking.md), [TAD-Weekly-Stats](TAD-Weekly-Stats.md)
- **Last reviewed:** 2026-05-05
