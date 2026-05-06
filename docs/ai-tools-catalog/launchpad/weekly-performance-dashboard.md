---
title: "weekly-performance-dashboard"
description: "Weekly performance dashboard for the Maintenance Operations, Maintenance OE, and PPT-Bugs teams. Shows applied hours vs. call counts with status breakdo..."
sidebar_position: 37
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — Weekly performance dashboard for the Maintenance Operations, Maintenance OE, and PPT-Bugs teams. Shows applied hours vs. call counts with status breakdowns (Completed, In Testing) per employee and sub-team, with expandable call details.

## Overview

For maintenance-side teams, "how busy were we this week?" needs to be answered with numbers, not vibes. weekly-performance-dashboard cross-references applied hours from IHOP_Net (`si_action`) with call data from Issue Maintenance to show:

- **Applied hours per employee and sub-team** — how much time was actually logged.
- **Call counts by status** — Completed, In Testing, etc.
- **Expandable per-call drill-down** — click into the underlying tickets.

It's the source for weekly status reporting on the maintenance side of the org.

## Why use it

- **Manager visibility.** Each sub-team's load and output, in one view.
- **Status reporting.** Pull the week's numbers for the weekly status email.
- **Coaching.** Spot trends in individual workload and adjust.

## When to use it

- Friday/Monday weekly status checks for the maintenance teams.
- Mid-quarter retrospectives.
- Investigating outliers ("why was this team's hours so low this week?").

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/weekly-performance-dashboard](https://launchpad.globalshopsolutions.dev/apps/weekly-performance-dashboard) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. The default view shows the current week.
3. Drill into a sub-team or employee for detail.
4. Click a call/ticket to see its underlying record.

## Common questions

**Which teams are covered?**
Maintenance Operations, Maintenance OE, and PPT-Bugs.

**What's the data source?**
IHOP_Net `si_action` (applied hours) and Issue Maintenance (calls and statuses).

**How fresh is the data?**
Confirm with the owner — typically near-real-time, sourced from the underlying systems.

## Owner & support

- **Owner:** rravindranath@gssmail.com
- **App page:** [weekly-performance-dashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/weekly-performance-dashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/weekly-performance-dashboard](https://github.com/GlobalShopSolutions-InternalTools/weekly-performance-dashboard)
- **Last reviewed:** 2026-05-04
