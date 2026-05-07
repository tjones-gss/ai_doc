---
title: "globe-toolbox"
description: "A cross-platform desktop tray app (Windows/macOS/Linux) that lives in your system tray and opens a popup of GSS apps you have access to. Built with Elec..."
sidebar_position: 24
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A cross-platform desktop tray app (Windows/macOS/Linux) that lives in your system tray and opens a popup of GSS apps you have access to. Built with Electron + React + TypeScript. OAuth login via Keycloak; works offline with cached entitlements.

## Overview

Most GSS internal apps live as URLs scattered across LaunchPad, bookmarks, and Slack messages. Globe Toolbox is a tray-first launcher: click the tray icon, see your authorized apps in a popup, click one, go.

The app handles authentication (OAuth2/OIDC via Keycloak), license/entitlement checking against an authority API, and offline fallback with signature-verified cached data so a brief disconnect doesn't lock you out.

## Why use it

- **Faster app access.** Tray icon vs. typing URLs or hunting bookmarks.
- **License-aware.** Only shows what you actually have access to.
- **Offline-tolerant.** Cached entitlements (signature-verified) keep the tray usable on the road.

## When to use it

- You use multiple GSS internal apps daily and want them one click away.
- You're often offline (travel, customer site) and need known-good app shortcuts to keep working.

## How to access it

- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/globe-toolbox](https://launchpad.globalshopsolutions.dev/apps/globe-toolbox) — installer download.
- **Platforms:** Windows, macOS, Linux.
- **Login:** OAuth2/OIDC via Keycloak (your GSS account).

## Tech stack

| Layer | Technology |
|---|---|
| Shell | Electron |
| UI | React + TypeScript |
| Auth | OAuth2/OIDC with PKCE (Keycloak) |
| Updates | `electron-updater` (self-update) |
| Offline | Signature-verified cached entitlements |

## Common questions

**Will I still see new apps automatically?**
Yes — the entitlements API is queried on launch. New apps you've been granted access to appear next time.

**What happens if my license expires?**
The cached entitlements have an expiry. Past it, you re-authenticate. Within the window, you keep working offline.

**Does it auto-update?**
Yes, via `electron-updater`.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [globe-toolbox on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/globe-toolbox)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/globe-toolbox](https://github.com/GlobalShopSolutions-InternalTools/globe-toolbox)
- **Last reviewed:** 2026-05-07
