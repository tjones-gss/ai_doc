---
title: "QA-resolved-issues"
description: "A dashboard of every issue resolved by QA. Useful for weekly status, retrospectives, and showing \"what we got done\" to leadership."
sidebar_position: 4
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# QA-resolved-issues

> **TL;DR** — A dashboard of every issue resolved by QA. Useful for weekly status, retrospectives, and showing "what we got done" to leadership.

## Overview

When QA resolves an issue (verified the fix, closed the call), that resolution shows up on this dashboard. Filter by week, by tester, by team — see what's been completed and at what cadence.

## Why use it

- **Visibility into QA throughput.** Closed issues are a tangible measure of work done.
- **Status updates.** Pull the past week's resolutions for the weekly status.
- **Trend analysis.** Are resolutions increasing? Stalling? The dashboard makes it visible.

## When to use it

- Weekly QA status / leadership check-ins.
- Retrospectives ("what did we ship this sprint?").
- One-on-ones to talk about individual contributions.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/QA-resolved-issues](https://launchpad.globalshopsolutions.dev/apps/QA-resolved-issues) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. Default view shows recent resolutions.
3. Filter by date / tester / team.
4. Click an issue to drill into its history in Issue Maintenance.

## Common questions

**What counts as "resolved by QA"?**
Issues moved to a resolved state where QA was the closer — exact rules per the app. Confirm with the owner if the metric is critical.

## Owner & support

- **Owner:** *Owner listed on the LaunchPad app page.*
- **App page:** [QA-resolved-issues on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/QA-resolved-issues)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/QA-resolved-issues](https://github.com/GlobalShopSolutions-InternalTools/QA-resolved-issues)
- **Related apps:** [CausingIssueDashboard](CausingIssueDashboard.md), [program-issue-tracker](program-issue-tracker.md), [bug-traffic-dashboard](bug-traffic-dashboard.md)
- **Last reviewed:** 2026-05-04
