---
title: "TAD-Weekly-Stats"
description: "Weekly TestArchitect testing statistics dashboard. Shows what got tested, what passed, what failed, and how that's trending — at a weekly cadence."
sidebar_position: 6
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — Weekly TestArchitect testing statistics dashboard. Shows what got tested, what passed, what failed, and how that's trending — at a weekly cadence.

## Overview

TestArchitect (TA) test runs produce a lot of data — pass/fail counts, durations, flakiness, coverage. TAD-Weekly-Stats rolls that up into a weekly report so you don't have to dig through individual run logs to know whether the test estate is healthy.

## Why use it

- **Health snapshot.** One page tells you if the weekly run was good, bad, or weird.
- **Trends.** Pass rate climbing, falling, or flat?
- **Flakiness signals.** Tests that pass-fail-pass-fail are visible.

## When to use it

- Weekly QA / engineering reviews.
- After a major release, to compare pre- and post-release test runs.
- Investigating "why did this week's CI feel slow / broken?".

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/TAD-Weekly-Stats](https://launchpad.globalshopsolutions.dev/apps/TAD-Weekly-Stats) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. The default view is the most recent week's stats.
3. Switch weeks via the navigation to compare or look back.

## Common questions

**Does it cover all TA suites?**
Confirm with the owner — typically yes, but specific suites may be excluded.

## Owner & support

- **Owner:** *Owner listed on the LaunchPad app page.*
- **App page:** [TAD-Weekly-Stats on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/TAD-Weekly-Stats)
- **Related app:** [TABugTracking](TABugTracking.md)
- **Last reviewed:** 2026-05-04
