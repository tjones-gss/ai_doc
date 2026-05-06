---
title: "bug-traffic-dashboard"
description: "A real-time dashboard that tracks how service calls flow through the P&E Bugs queue. Shows incoming vs. closed vs. in-QA-testing volumes by team, weekly..."
sidebar_position: 11
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A real-time dashboard that tracks how service calls flow through the P&E Bugs queue. Shows incoming vs. closed vs. in-QA-testing volumes by team, weekly trends, and call-level drill-down.

## Overview

The P&E Bugs queue is a constant stream of incoming service calls. Without a dashboard, "is the queue under control?" is a feeling rather than a number. bug-traffic-dashboard turns it into a live picture:

- **Incoming, closed, in-QA-testing** counts, broken down by team.
- **Weekly trends** so you can see whether the team is keeping up, falling behind, or catching up.
- **Call-level drill-down** to see exactly which calls make up a given metric.

## Why use it

- **Situational awareness.** Managers and team leads can see the queue health in one place.
- **Stand-up support.** Numbers to anchor weekly status conversations.
- **Trend spotting.** Spikes in incoming calls or stalls in QA testing surface quickly.

## When to use it

- Weekly team / leadership status reviews.
- Mid-week pulse checks — is anyone underwater?
- Investigating "why has this gotten slower?" with data.

Don't use it for:
- Customer-facing reporting. Internal only.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/bug-traffic-dashboard](https://launchpad.globalshopsolutions.dev/apps/bug-traffic-dashboard) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. The default view shows current-week metrics by team.
3. Switch to the trend view for week-over-week movement.
4. Click any number to drill into the underlying calls.

## Common questions

**How real-time is "real-time"?**
The dashboard refreshes on a short interval. Exact cadence depends on the underlying data sources (Issue Maintenance, IHOP_Net si_action). Confirm with the owner if the freshness matters for your use case.

**Does it cover other queues?**
The focus is the P&E Bugs queue. For other queues, see related dashboards (e.g., [QA-resolved-issues](QA-resolved-issues.md), [weekly-performance-dashboard](weekly-performance-dashboard.md)).

## Owner & support

- **Owner:** rravindranath@gssmail.com
- **App page:** [bug-traffic-dashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/bug-traffic-dashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/bug-traffic-dashboard](https://github.com/GlobalShopSolutions-InternalTools/bug-traffic-dashboard)
- **Last reviewed:** 2026-05-04
