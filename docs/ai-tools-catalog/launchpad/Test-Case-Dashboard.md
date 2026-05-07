---
title: "Test-Case-Dashboard"
description: "Unified test-coverage dashboard for qTest (manual test cases) and TestArchitect (automated tests). Search and filter by program, menu path, or tester; s..."
sidebar_position: 9
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — Unified test-coverage dashboard for **qTest** (manual test cases) and **TestArchitect** (automated tests). Search and filter by program, menu path, or tester; spot coverage gaps in one place instead of jumping between two tools.

## Overview

Manual test cases live in qTest. Automated tests live in TestArchitect. Asking "what's our test coverage for Program X?" used to mean two separate searches and a manual reconciliation. Test-Case-Dashboard joins them: one UI, one query, all the test data.

Filter by:
- Program / module
- Menu path
- Tester / owner
- Coverage status (covered / partial / gap)

## Why use it

- **Single pane of glass.** No more bouncing between qTest and TA.
- **Gap detection.** See clearly where neither tool has coverage.
- **Owner accountability.** Filter by tester to see what someone owns.

## When to use it

- Before shipping a feature: confirm both manual and automated coverage exist.
- During retrospectives: see how coverage trends week over week.
- When auditing technical debt: identify programs with thin coverage.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/Test-Case-Dashboard](https://launchpad.globalshopsolutions.dev/apps/Test-Case-Dashboard) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Confirm with owner.

## Common questions

**Where does the data come from?**
qTest API for manual cases, TestArchitect data for automated. Cached locally; can run with cached data without an API token (only needed for fetch scripts).

**Is this the same as TABugTracking?**
No. [TABugTracking](TABugTracking.md) tracks bugs caught by TA tests. This dashboard tracks test *coverage* across both qTest and TA.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [Test-Case-Dashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/Test-Case-Dashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/Test-Case-Dashboard](https://github.com/GlobalShopSolutions-InternalTools/Test-Case-Dashboard)
- **Related apps:** [TABugTracking](TABugTracking.md), [TAD-Weekly-Stats](TAD-Weekly-Stats.md), [qTest-MCP-Server](qTest-MCP-Server.md), [testarchitect-mcp](testarchitect-mcp.md)
- **Last reviewed:** 2026-05-07
