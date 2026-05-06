---
title: "TABugTracking"
description: "Tracks bugs caught by Test Architect (TA) tests. The official place to see \"which production bugs would have been caught by automation if we had the rig..."
sidebar_position: 5
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# TABugTracking

> **TL;DR** — Tracks bugs caught by Test Architect (TA) tests. The official place to see "which production bugs would have been caught by automation if we had the right test?".

## Overview

Test Architect is GSS's automated UI test framework. When a TA test catches a regression — or a bug that snuck in despite the tests — TABugTracking records it. The dashboard turns those records into a visible inventory:

- Which tests caught which bugs.
- Which bugs slipped past the tests (and need new coverage).
- Trend over time of TA-caught issues.

## Why use it

- **Test ROI.** Concrete proof that automation catches things.
- **Coverage gaps.** Bugs that should have been caught but weren't = test cases to add.
- **QA reporting.** Bring real numbers to leadership conversations.

## When to use it

- Weekly QA reviews.
- Test coverage audits.
- Showing the value of test architecture investments.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/TABugTracking](https://launchpad.globalshopsolutions.dev/apps/TABugTracking) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. Browse caught bugs by test, by date, or by program.
3. Click a record to see the bug and the test.

## Common questions

**Where do the bug records come from?**
TA test failures are recorded automatically; manually-entered bugs may be added too. Confirm with the owner.

**Does this replace Issue Maintenance?**
No — Issue Maintenance is the system of record. This is a focused view onto TA-relevant bugs.

## Owner & support

- **Owner:** *Owner listed on the LaunchPad app page.*
- **App page:** [TABugTracking on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/TABugTracking)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/TABugTracking](https://github.com/GlobalShopSolutions-InternalTools/TABugTracking)
- **Related app:** [TAD-Weekly-Stats](TAD-Weekly-Stats.md)
- **Last reviewed:** 2026-05-04
