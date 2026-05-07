---
title: "launchpad (the host platform itself)"
description: "LaunchPad is the internal app-provisioning platform that hosts most of the apps in this catalog. It scaffolds new apps from templates, links GitHub OAut..."
sidebar_position: 34
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — LaunchPad is the internal app-provisioning platform that hosts most of the apps in this catalog. It scaffolds new apps from templates, links GitHub OAuth identities, manages job clients via Keycloak, and provides the deployment substrate. This is the meta-tool — the thing on which everything else runs.

## Overview

You've seen `launchpad.globalshopsolutions.dev` linked from every other catalog entry. This article documents LaunchPad itself: the service that hosts and provisions our internal apps. It's worth knowing about because:

- It's where you go to **create a new internal app** (and have it scaffolded from a template repo automatically).
- It's where the **list of apps lives** (the catalog you've been browsing is, ultimately, a curated cut of the LaunchPad app list).
- It's the **deployment substrate** for most internal services — Kubernetes, ArgoCD, persistent volumes via `.deploy/app.yaml`.

## Why use it

- **Scaffold a new app in minutes.** Pick a template; LaunchPad creates the GitHub repo, hooks up CI, sets up the deployment, and lists the app on the home page.
- **Single sign-on.** GitHub OAuth + Keycloak job clients are wired up for you.
- **Standardized deployments.** PVC mounts, environment secrets, health checks — all configured via `.deploy/app.yaml` in the app's own repo.

## When to use it

- You have an idea for an internal tool. Start with LaunchPad's "Create App" — it'll save you days of plumbing.
- You're hunting for an existing internal tool. Browse the home page; filter by Live, Configured, kind (Web App / Tool / MCP / API).
- You're managing job clients (Keycloak service accounts).

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/](https://launchpad.globalshopsolutions.dev/)
- **Login:** Global Shop Office 365 SAML SSO.
- **Create an app:** **Create App** button on the home page.
- **Browse:** **Dashboard** → filter by Live / kind / yours.

## How it works

- **Frontend:** Web app at `launchpad.globalshopsolutions.dev`.
- **Backend:** Internal app provisioning service. GitHub-backed scaffolding from template repos.
- **Auth:** GitHub OAuth account linking + Keycloak job-client management.
- **Persistence:** PostgreSQL (events, identity).
- **Deploy substrate:** Kubernetes + ArgoCD; apps configure via `.deploy/app.yaml` (with optional `dependencies.yaml` for PVCs / secrets).

## Common questions

**What's the difference between "Live" and "Configured" status?**
- **Configured** means the app is wired up on LaunchPad and has a repo / template.
- **Live** means it's actually deployed and reachable.

**How do I get my new app onto LaunchPad?**
Click **Create App** on the dashboard. Pick a template (the default `starter` works for most cases). The flow creates the repo, sets up CI, and lists the tile.

**What kinds (Web App / Tool / API / MCP) are there?**
- **Web App** — has a UI; users `Launch` it.
- **API** — backend service, no UI.
- **Tool** — CLI / desktop / installer.
- **MCP** — exposes an MCP server endpoint.
- **Library / Worker** — present in filters but rarely populated today.

The kind tag was added later, so older apps may show as "Web App" even though they're really MCPs (e.g., [cobol-mcp](cobol-mcp.md)).

## Owner & support

- **Owner:** GSS engineering platform team
- **App page:** [launchpad on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/launchpad) (yes, it's in its own list)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/launchpad](https://github.com/GlobalShopSolutions-InternalTools/launchpad)
- **Last reviewed:** 2026-05-07
