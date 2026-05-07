---
title: "tad-dashboard"
description: "The real-time monitoring and administration dashboard for Test Architect Distributed (TAD). Modern React SPA with live visibility into test execution, V..."
sidebar_position: 48
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The real-time monitoring and administration dashboard for **Test Architect Distributed (TAD)**. Modern React SPA with live visibility into test execution, VMs, host machines, and system health. The user-facing surface for the TAD platform.

## Overview

The TAD Dashboard is the primary UI for the TAD ecosystem. Where the database/host-manager/worker services do the heavy lifting under the hood, the dashboard is what humans look at — what's running, where, on what host, and how it's going.

It connects to [tad-database-service](tad-database-service.md) for state, [tad-host-manager-service](tad-host-manager-service.md) for orchestration, and uses SignalR for real-time updates so the UI stays current without manual refresh.

## Why use it

- **Live visibility.** No "let me refresh and see" — SignalR pushes updates as test runs and VM states change.
- **Cross-team picture.** All TAD test runs in one place, regardless of which team owns the test.
- **Admin surface.** Pause/resume hosts, inspect VM health, drill into individual test failures.

## When to use it

- You need to know whether a TAD test run is healthy *right now*.
- You're hunting down a failing test and want to see the run, the VM, and the host context together.
- You're managing host machines and need to coordinate operating windows.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/tad-dashboard](https://launchpad.globalshopsolutions.dev/apps/tad-dashboard) → **Launch**.
- **Live:** `tad-dashboard.globalshopsolutions.dev` (confirm with owner).
- **Login:** Global Shop Office 365 SSO.

## How it works

- **Stack:** React SPA (modern; TypeScript).
- **Data:** Pulls from `tad-database-service`'s REST API.
- **Real-time:** SignalR connection for push updates on state changes.
- **Auth:** Browser SSO via Keycloak.

## Common questions

**Is this the same as TestArchitect (the desktop tool)?**
No. TestArchitect is the test-authoring environment. TAD is the *distributed execution* platform that runs TA tests across many VMs. This dashboard monitors that execution.

**Where do bugs caught by tests show up?**
[TABugTracking](TABugTracking.md). The dashboard focuses on test-run state; bug records live there.

**Where do weekly metrics live?**
[TAD-Weekly-Stats](TAD-Weekly-Stats.md).

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [tad-dashboard on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/tad-dashboard)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tad-dashboard](https://github.com/GlobalShopSolutions-InternalTools/tad-dashboard)
- **Related apps:** [tad-database-service](tad-database-service.md), [tad-host-manager-service](tad-host-manager-service.md), [tad-worker-service](tad-worker-service.md), [TABugTracking](TABugTracking.md), [TAD-Weekly-Stats](TAD-Weekly-Stats.md), [Test-Case-Dashboard](Test-Case-Dashboard.md)
- **Last reviewed:** 2026-05-07
