---
title: "program-issue-tracker"
description: "Search every issue tied to a specific Program. If you maintain a program in GSSERP and want a quick view of every bug, request, and customer call agains..."
sidebar_position: 30
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# program-issue-tracker

> **TL;DR** — Search every issue tied to a specific Program. If you maintain a program in GSSERP and want a quick view of every bug, request, and customer call against it — this is the search.

## Overview

GSSERP issues live in Issue Maintenance, but searching them by Program is awkward there. program-issue-tracker is a focused search UI: type a Program name, get every issue connected to it, with filters for status, severity, and date.

## Why use it

- **One-search context.** Instead of opening Issue Maintenance and clicking through filters, jump straight to "what's wrong with Program X?".
- **Triage and planning.** Pull the open issue list before a sprint planning session.
- **Onboarding to a program.** New owner of a program? Read the issue list for fast context.

## When to use it

- A customer reports an issue with a program and you want to see if it's been seen before.
- You're scoping work on a program and need to see open issues.
- You're auditing technical debt or trends in a program.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/program-issue-tracker](https://launchpad.globalshopsolutions.dev/apps/program-issue-tracker) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. Type the Program name.
3. Filter by status / date / severity as needed.
4. Click an issue to open it in Issue Maintenance.

## Common questions

**Does it show closed issues too?**
Yes — filter to switch between open/closed/all.

**Can I export the list?**
Confirm with the owner; export support may be limited.

## Owner & support

- **Owner:** pfuentes@gssmail.com
- **App page:** [program-issue-tracker on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/program-issue-tracker)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/program-issue-tracker](https://github.com/GlobalShopSolutions-InternalTools/program-issue-tracker)
- **Related apps:** [CausingIssueDashboard](CausingIssueDashboard.md), [QA-resolved-issues](QA-resolved-issues.md)
- **Last reviewed:** 2026-05-04
