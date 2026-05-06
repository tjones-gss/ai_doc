---
title: "arc-scanner"
description: "A web tool that scans every repo in a GitHub org for SQL table references and join relationships pulled from GabScript and SQL files, then visualizes th..."
sidebar_position: 8
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A web tool that scans every repo in a GitHub org for SQL table references and join relationships pulled from GabScript and SQL files, then visualizes the result as a heatmap, ranked list, and per-repo breakdown. Helps you see which tables are hot and which repos touch them.

## Overview

Across our many repos, the same SQL tables get joined, queried, and updated in dozens of places. arc-scanner walks the entire GitHub org, parses GabScript and SQL files, and pulls out:

- Every `FROM`, `JOIN`, and table-reference clause it can find.
- The relationships between tables (which ones get joined together, and how often).
- Per-repo breakdowns of which tables a repo touches.

It then renders the result as an interactive dashboard — heatmap of usage, ranked list of "most-referenced tables," and drill-down by repo.

## Why use it

- **Impact analysis.** Before changing a table or column, see who else uses it.
- **Refactoring decisions.** Hot, widely-joined tables are riskier to change; arc-scanner shows you which ones.
- **Onboarding.** New engineer trying to understand the data model? The heatmap is faster than reading docs.

## When to use it

- You're planning a schema change and want to see the blast radius.
- You're auditing the codebase for a deprecated table or column.
- You want a high-level "here's where the data lives" picture.

Don't use it for:
- Performance analysis — arc-scanner shows usage, not query performance. For perf, use Zen Log Parser or your DB tooling.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/arc-scanner](https://launchpad.globalshopsolutions.dev/apps/arc-scanner) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. The home view shows the heatmap — table names sized/colored by reference count.
3. Click a table to drill into the repos that reference it.
4. Click a repo to see exactly which files reference the table and how.

## Common questions

**How fresh is the data?**
The scan refreshes on a schedule — confirm cadence with the owner. Recent commits may not be reflected immediately.

**Does it cover non-GabScript/SQL files?**
The current version focuses on GabScript and SQL. Other languages (e.g., embedded SQL in C++/COBOL) may not be detected.

**Is this AI-powered?**
The parsing is deterministic, not AI. The visualization is a standard web dashboard.

## Owner & support

- **Owner:** avaldez@gssmail.com
- **App page:** [arc-scanner on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/arc-scanner)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/arc-scanner](https://github.com/GlobalShopSolutions-InternalTools/arc-scanner)
- **Last reviewed:** 2026-05-04
