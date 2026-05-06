---
title: "GSPLinkDecoder"
description: "A tiny web app that quickly decodes gsp:// links. Paste a link in, see what it points to. Saves you the hand-decoding when chasing bug reports or suppor..."
sidebar_position: 2
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A tiny web app that quickly decodes `gsp://` links. Paste a link in, see what it points to. Saves you the hand-decoding when chasing bug reports or support tickets.

## Overview

`gsp://` links are GSS's internal deep-link format — they encode a target program, screen, or record. They're not human-readable on their own. GSPLinkDecoder takes the link and shows you what it points to in plain English.

## Why use it

- **Triage faster.** A `gsp://` URL in a customer ticket goes from cryptic to obvious in two seconds.
- **No tooling overhead.** It's a webpage; no install needed.

## When to use it

- A bug report, support ticket, or chat message contains a `gsp://` link and you need to know what it points to.
- You're documenting steps that include a deep link and want the readable version.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/GSPLinkDecoder](https://launchpad.globalshopsolutions.dev/apps/GSPLinkDecoder) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Access:** All staff.

## How to use it

1. Launch the app.
2. Paste the `gsp://` link.
3. Read the decoded result.

## Owner & support

- **Owner:** *Owner listed on the LaunchPad app page.*
- **App page:** [GSPLinkDecoder on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/GSPLinkDecoder)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/GSPLinkDecoder](https://github.com/GlobalShopSolutions-InternalTools/GSPLinkDecoder)
- **Last reviewed:** 2026-05-04
