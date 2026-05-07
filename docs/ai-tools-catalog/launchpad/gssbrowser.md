---
title: "gssbrowser"
description: "GSS Browser, on the web. Same backend code as the desktop GSS Browser, but browser-accessible — so it can be used to reproduce bugs and run automated AI..."
sidebar_position: 28
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — GSS Browser, on the web. Same backend code as the desktop GSS Browser, but browser-accessible — so it can be used to reproduce bugs and run automated AI-driven testing without a Windows machine.

## Overview

GSS Browser is the long-standing tool for inspecting the GSS data layer and running queries. Historically, it was Windows-only desktop software. `gssbrowser` ports that experience to the web: same backend code, browser frontend.

Two big use cases unlock by being web-based:
- **Bug reproduction.** Someone hits an issue; you (or QA) can reproduce it from any device with a URL.
- **AI-driven automated testing.** Web means scriptable. AI agents (Cursor, Claude Code, Codex) can drive the browser via Playwright/MCP to exercise scenarios that would otherwise need a human at a keyboard.

## Why use it

- **No install.** Open the URL.
- **Works anywhere.** Linux, Mac, mobile-ish — wherever a browser runs.
- **Automatable.** AI agents and CI can drive it.

## When to use it

- You need to reproduce a customer-reported issue on the data side and don't want to fire up a Windows VM.
- You're scripting AI-driven test scenarios that exercise the browser surface.
- You're showing GSS Browser behavior to someone in a screen-share without needing to install anything on their end.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/gssbrowser](https://launchpad.globalshopsolutions.dev/apps/gssbrowser) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Confirm with owner.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [gssbrowser on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/gssbrowser)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gssbrowser](https://github.com/GlobalShopSolutions-InternalTools/gssbrowser)
- **Last reviewed:** 2026-05-07
