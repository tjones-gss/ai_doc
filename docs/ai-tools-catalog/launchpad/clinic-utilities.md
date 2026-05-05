---
title: "clinic-utilities"
description: "An MCP server that returns information about Clinic company codes, meant for AI assistants. Likely to grow with more Clinic-related lookups over time."
sidebar_position: 14
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — An MCP server that returns information about Clinic company codes, meant for AI assistants. Likely to grow with more Clinic-related lookups over time.

## Overview

Clinic environments at GSS each have a company code with associated metadata — version, modules enabled, configuration quirks. `clinic-utilities` exposes this info to Claude Code via MCP so you can ask questions like "what version is Clinic XYZ on?" without digging through admin panels.

## Why use it

- **Faster Clinic lookups.** Ask Claude instead of clicking around.
- **Context for support work.** When Claude is helping triage a customer issue, knowing the Clinic configuration up front saves a step.

## When to use it

- You're working on a Clinic-specific issue.
- You need to confirm a Clinic's company code or configuration.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/clinic-utilities](https://launchpad.globalshopsolutions.dev/apps/clinic-utilities) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Kind:** MCP server.

## Owner & support

- **Owner:** bcorbin@gssmail.com
- **App page:** [clinic-utilities on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/clinic-utilities)
- **Last reviewed:** 2026-05-05
