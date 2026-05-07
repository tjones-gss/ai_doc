---
title: "zen-log-parser"
description: "Parses and analyzes Pervasive Zen database log files. Turns a wall of cryptic log text into a tabbed interface where you can sift through queries, find ..."
sidebar_position: 56
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# zen-log-parser

> **TL;DR** — Parses and analyzes Pervasive Zen database log files. Turns a wall of cryptic log text into a tabbed interface where you can sift through queries, find slow ones, and identify optimization opportunities.

## Overview

Zen / Pervasive log files are dense and hard to read by hand. zen-log-parser transforms them into a navigable interface:

- **Tabbed views** for different log slices (queries, errors, summary stats).
- **Performance focus** — surfaces the queries that are slow or run too often.
- **Optimization hints** — patterns the parser flags as worth investigating.

It's aimed at DBAs and developers who need to understand what's actually happening on a Zen database, especially when troubleshooting customer performance complaints.

## Why use it

- **Faster diagnosis.** Skim summary tabs instead of grepping raw logs.
- **Performance leads.** The "slowest queries" view is usually where optimization wins live.
- **Shareable findings.** The structured output is easier to drop into a ticket or doc than raw log lines.

## When to use it

- A customer reports performance issues and you have a Zen log file.
- You're auditing a database for query patterns.
- You're verifying that a recent optimization actually helped.

Don't use it for:
- Non-Zen / non-Pervasive databases.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/zen-log-parser](https://launchpad.globalshopsolutions.dev/apps/zen-log-parser) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. Upload the Zen log file.
3. Browse the tabs — summary, queries, errors, performance.
4. Filter / sort to focus on the slowest queries or most-frequent patterns.

## Common questions

**How big a log can it handle?**
Reasonable file sizes — confirm specifics with the owner if you're handling unusually large files.

**Does data leave the browser?**
Confirm with the owner — depends on whether parsing happens client-side or server-side.

## Owner & support

- **Owner:** cvanroekel@gssmail.com
- **App page:** [zen-log-parser on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/zen-log-parser)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/zen-log-parser](https://github.com/GlobalShopSolutions-InternalTools/zen-log-parser)
- **Related app:** [zen-data-builder](zen-data-builder.md) (test data for the same database family).
- **Last reviewed:** 2026-05-04
