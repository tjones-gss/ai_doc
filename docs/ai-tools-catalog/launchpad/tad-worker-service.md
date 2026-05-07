---
title: "tad-worker-service"
description: "The background job processor for Test Architect Distributed (TAD). Headless .NET 6 worker that assigns pending test issues to available VMs, retries fai..."
sidebar_position: 51
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The background job processor for **Test Architect Distributed (TAD)**. Headless .NET 6 worker that assigns pending test issues to available VMs, retries failed notifications, and coordinates cache builds. No user-facing surface beyond `/health`.

## Overview

The TAD dashboard shows tests; the host manager knows which VMs exist; somebody has to actually decide "test #X goes on VM Y, now." That's the worker service. It pulls pending work, picks a healthy VM that's allowed to run, dispatches the test, retries if something fails, and rebuilds caches in the background.

Sister services:
- [tad-database-service](tad-database-service.md) — central data store
- [tad-host-manager-service](tad-host-manager-service.md) — orchestration / control plane
- [tad-dashboard](tad-dashboard.md) — UI

## Why use it

Pure infrastructure — no UI, no API to call against. You'd interact with it when:
- TAD tests are sitting pending (the worker isn't picking them up).
- Notifications about test results are missing or duplicated.
- Cache rebuilds are stale.

In all those cases, the worker's logs are the answer.

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/tad-worker-service](https://launchpad.globalshopsolutions.dev/apps/tad-worker-service)
- **Health endpoint:** `/health`
- **Login:** Global Shop Office 365 SSO.

## How it works

- **Stack:** Headless .NET 6 worker service (no API surface).
- **Background jobs:** assignment, retries, cache builds.
- **Inputs:** `tad-database-service` (data) + `tad-host-manager-service` (VM availability).

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [tad-worker-service on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/tad-worker-service)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tad-worker-service](https://github.com/GlobalShopSolutions-InternalTools/tad-worker-service)
- **Related apps:** [tad-database-service](tad-database-service.md), [tad-host-manager-service](tad-host-manager-service.md), [tad-dashboard](tad-dashboard.md)
- **Last reviewed:** 2026-05-07
