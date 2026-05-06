---
title: "mobile-crm-status"
description: "Monitors MobileCRM endpoints (production, edge, and tenant APIs) and pings Microsoft Teams when one goes down. The \"is mobile CRM healthy right now?\" page."
sidebar_position: 28
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# mobile-crm-status

> **TL;DR** — Monitors MobileCRM endpoints (production, edge, and tenant APIs) and pings Microsoft Teams when one goes down. The "is mobile CRM healthy right now?" page.

## Overview

MobileCRM has multiple endpoints — production, edge, and per-tenant APIs. When any of them stops responding, someone needs to know fast. `mobile-crm-status` is a small monitoring app that checks each endpoint on a schedule, displays current status in a Blazor UI, and posts a notification to a configured Microsoft Teams channel when an endpoint flips to down.

## Why use it

- **No-look monitoring.** A glance at the page tells you whether MobileCRM is healthy across all endpoints.
- **Proactive alerts.** Teams notification means the on-call engineer hears about it without watching a dashboard.
- **Tenant-aware.** Tenants flagged as "watched" are tracked individually; outages on those trigger explicit alerts.

## When to use it

- You're investigating "is mobile CRM down for me, or is it the customer?"
- You're on call and want a Teams ping if anything breaks.
- You're verifying a deploy didn't take down a tenant.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/mobile-crm-status](https://launchpad.globalshopsolutions.dev/apps/mobile-crm-status) → **Launch**.
- **Production URL:** `internaltools-mobile-crm-status.globalshopsolutions.dev`
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Kind:** Tool.

## Configuration (non-secret)

Lives in `appsettings.json`:

| Setting | Purpose |
|---|---|
| `Endpoints` | Static endpoints to monitor |
| `WatchedTenants` | Tenant names/URLs that trigger Teams alerts |
| `CheckIntervalMinutes` | How often to check (default: 5 min) |
| `FailureThreshold` | Consecutive failures before alerting (default: 1) |

Secrets (DB connection, Teams webhook) come from Kubernetes secrets in production, not from this article.

## How it works

- **Stack:** .NET 9 Blazor Server, Tailwind CSS, SQL Server (for tenant endpoint discovery), Teams webhook (for failure notifications).
- **Loop:** Every `CheckIntervalMinutes`, ping each configured endpoint. When an endpoint passes the failure threshold, post a structured message to the Teams webhook.
- **Deploy:** Push to `main` triggers CI → Docker build → ECR → ArgoCD.

## Common questions

**Where do alerts go?**
A configured Microsoft Teams channel. The webhook URL is a secret — see the owner if you need it routed elsewhere.

**Can I add a new endpoint to monitor?**
Yes — add it to `appsettings.json` and redeploy. For tenant-specific monitoring, also add to `WatchedTenants`.

**Is there an alert escalation path?**
Currently Teams notifications only. For pager-style escalation, talk to the owner.

## Owner & support

- **Owner:** cmasden@gssmail.com
- **App page:** [mobile-crm-status on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/mobile-crm-status)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/mobile-crm-status](https://github.com/GlobalShopSolutions-InternalTools/mobile-crm-status)
- **Last reviewed:** 2026-05-06
