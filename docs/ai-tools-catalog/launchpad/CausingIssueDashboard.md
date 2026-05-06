---
title: "CausingIssueDashboard"
description: "A dashboard for viewing issues that have a \"Causing Issue\" relationship in Issue Maintenance — i.e., bugs whose root cause is another bug. Helps you see..."
sidebar_position: 1
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A dashboard for viewing issues that have a "Causing Issue" relationship in Issue Maintenance — i.e., bugs whose root cause is another bug. Helps you see the actual root, not just the symptom.

## Overview

Issue Maintenance lets you mark one issue as "caused by" another — useful when a customer-facing bug turns out to be a downstream effect of a deeper problem. CausingIssueDashboard surfaces those relationships visually so you can:

- See which issues are causing the most downstream noise.
- Trace from a symptom issue to its root.
- Identify root-cause clusters that need attention.

## Why use it

- **Root-cause focus.** Fix the cause, save fixing the next 10 symptoms.
- **Triage signal.** Issues with many "caused by" links are usually higher priority than they look in isolation.

## When to use it

- Triage / planning meetings where you decide what to work on next.
- Post-mortem investigations.
- Reporting on bug clusters to leadership.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/CausingIssueDashboard](https://launchpad.globalshopsolutions.dev/apps/CausingIssueDashboard) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. Browse the list / graph of issues with Causing-Issue links.
3. Click an issue to open the underlying Issue Maintenance record.

## Common questions

**Does this create or edit relationships?**
No — it's a viewer. Edits happen in Issue Maintenance.

**What if a relationship is wrong?**
Fix it in Issue Maintenance and the dashboard will reflect the change after its next refresh.

## Owner & support

- **Owner:** *Owner listed on the LaunchPad app page.*
- **App page:** [CausingIssueDashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/CausingIssueDashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/CausingIssueDashboard](https://github.com/GlobalShopSolutions-InternalTools/CausingIssueDashboard)
- **Last reviewed:** 2026-05-04
