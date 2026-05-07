---
title: "tad-database-service"
description: "The central data store and REST API for Test Architect Distributed (TAD). Provides persistent storage, real-time updates, and the primary API surface fo..."
sidebar_position: 49
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The central data store and REST API for **Test Architect Distributed (TAD)**. Provides persistent storage, real-time updates, and the primary API surface for the TAD dashboard and supporting services. ASP.NET Core 6 + Entity Framework Core + PostgreSQL.

## Overview

Test Architect Distributed (TAD) is a multi-service platform for distributed automated testing. `tad-database-service` is the data and API layer at its center — every other TAD component talks to this service to persist or read state.

Sister services in the TAD ecosystem:
- [tad-host-manager-service](tad-host-manager-service.md) — orchestration / control plane
- [tad-worker-service](tad-worker-service.md) — background job processor
- [tad-dashboard](tad-dashboard.md) — UI on top of the data
- [TAD-Weekly-Stats](TAD-Weekly-Stats.md) — weekly reporting view

## Why use it

You don't usually use this service directly — it's infrastructure under the TAD dashboard. Reach for it when:

- You're integrating a new TAD component and need the API surface.
- You're debugging "the dashboard says X but the database has Y."
- You're scoping a TAD feature change and need to know what fields exist.

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/tad-database-service](https://launchpad.globalshopsolutions.dev/apps/tad-database-service)
- **API docs:** Live `/scalar` endpoint (confirm with owner).
- **Login:** Global Shop Office 365 SSO.

## How it works

- **Stack:** ASP.NET Core 6.0 + Entity Framework Core + PostgreSQL.
- **Real-time updates:** Confirm SignalR / similar via the repo.
- **Persistence:** PostgreSQL via the LaunchPad `.deploy/dependencies.yaml` PVC.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [tad-database-service on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/tad-database-service)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/tad-database-service](https://github.com/GlobalShopSolutions-InternalTools/tad-database-service)
- **Related apps:** [tad-host-manager-service](tad-host-manager-service.md), [tad-worker-service](tad-worker-service.md), [tad-dashboard](tad-dashboard.md), [TABugTracking](TABugTracking.md), [TAD-Weekly-Stats](TAD-Weekly-Stats.md)
- **Last reviewed:** 2026-05-07
