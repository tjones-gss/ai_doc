---
title: "hookmaintenance"
description: "Defines hook ID ranges and manages script hooks within them — their details, settings, and activation status. The internal control panel for the hook sy..."
sidebar_position: 23
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — Defines hook ID ranges and manages script hooks within them — their details, settings, and activation status. The internal control panel for the hook system that customers and partners use to extend GSSERP.

## Overview

GSSERP supports "hooks" — extension points where customers/partners can attach custom scripts to standard behavior (e.g., "run this script when a sales order is saved"). Hook IDs are issued in ranges; tracking which IDs are taken, who owns them, and what each one does is what hookmaintenance is for.

The app lets you:

- Define and reserve hook ID ranges.
- See every hook within a range — name, owner, script, settings.
- Activate / deactivate individual hooks.
- Edit hook configuration.

## Why use it

- **Avoid ID collisions.** Reserving a range up front prevents two teams from picking the same IDs.
- **Inventory.** "What hooks are in production?" becomes a query, not an archeology project.
- **Quick disable.** Bad hook in production? Deactivate from this UI.

## When to use it

- You're a developer adding a new hook and need an ID range reserved for it.
- You're triaging a customer-reported issue that's downstream of a hook.
- You're auditing what hooks exist.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/hookmaintenance](https://launchpad.globalshopsolutions.dev/apps/hookmaintenance) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Access:** Restricted to engineers who work on hooks. Request via the app page.

## How to use it

1. Launch the app.
2. Browse hook ID ranges or search by hook name / ID.
3. Pick a hook to view or edit its details.

## Common questions

**Does deactivating a hook delete it?**
No — it leaves the record but stops the hook from firing.

**Can I see who edited a hook last?**
Audit info is on the hook detail page.

## Owner & support

- **Owner:** bbrambila@gssmail.com
- **App page:** [hookmaintenance on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/hookmaintenance)
- **Last reviewed:** 2026-05-04
