---
title: "tad-host-manager-service"
description: "The orchestration / control-plane API for Test Architect Distributed (TAD). Sits between the dashboard and the individual HostMachineService instances o..."
sidebar_position: 50
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The orchestration / control-plane API for **Test Architect Distributed (TAD)**. Sits between the dashboard and the individual `HostMachineService` instances on each physical test host. Coordinates VM discovery, health monitoring, pause/resume, and operating-hours enforcement.

## Overview

TAD runs automated tests across many physical hosts, each running multiple VMs. `tad-host-manager-service` is the coordinator: it knows which hosts exist, what VMs are on them, who's healthy, and what's allowed to run when. The dashboard and worker service both consult the host manager when they need to know "what's available right now."

Sister services:
- [tad-database-service](tad-database-service.md) — central data store
- [tad-worker-service](tad-worker-service.md) — background job processor
- [tad-dashboard](tad-dashboard.md) — UI

## Why use it

Like `tad-database-service`, this is infrastructure underneath TAD. You'd touch it when:
- Adding/removing a physical test host.
- Debugging "VM is missing / unhealthy."
- Tuning operating-hours rules (e.g., "don't kick off tests on host X between 8pm-6am").

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/tad-host-manager-service](https://launchpad.globalshopsolutions.dev/apps/tad-host-manager-service)
- **Login:** Global Shop Office 365 SSO.

## How it works

Coordinates between the dashboard / `tad-worker-service` and the per-host `HostMachineService` agents. Tracks VM state, health, and operating hours; exposes pause/resume controls.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [tad-host-manager-service on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/tad-host-manager-service)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tad-host-manager-service](https://github.com/GlobalShopSolutions-InternalTools/tad-host-manager-service)
- **Related apps:** [tad-database-service](tad-database-service.md), [tad-worker-service](tad-worker-service.md), [tad-dashboard](tad-dashboard.md)
- **Last reviewed:** 2026-05-07
