---
title: "pe-ai-library"
description: "The centralized source of truth for engineering AI rules and skills at GSS. Provides modular, reusable standards (global, guild, and team-specific) and ..."
sidebar_position: 38
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — The centralized source of truth for engineering AI rules and skills at GSS. Provides modular, reusable standards (global, guild, and team-specific) and generates curated rule sets for each team. Keeps AI context clean, prevents policy drift, and streamlines onboarding.

## Overview

When every engineer hand-rolls their own `AGENTS.md` / `.cursor/rules/` content, you get policy drift: one team's "always require tests" sits next to another team's "tests optional," and AI behavior varies wildly. `pe-ai-library` is the canonical store of GSS engineering AI rules — modular, layered, and team-scoped — that generates the right rule pack per team automatically.

The rule layers:
- **Global** — applies everywhere ("never paste customer-confidential data into prompts").
- **Guild** — by discipline (frontend, backend, COBOL, QA).
- **Team-specific** — overrides and additions for a particular team.

## Why use it

- **One source of truth.** Update the global rule once; all teams benefit on next pull.
- **Onboarding wins.** New engineer joins a team? They get the team's rule pack — no more "learn the conventions by osmosis."
- **Drift prevention.** Standards stay aligned even as people come and go.

## When to use it

- You're setting up a new repo and want it AI-rule-compliant from day one.
- You're updating a global engineering policy that should propagate.
- You're auditing a team's rule pack against the canonical standard.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/pe-ai-library](https://launchpad.globalshopsolutions.dev/apps/pe-ai-library) → **Launch**.
- **Login:** Global Shop Office 365 SSO.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [pe-ai-library on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/pe-ai-library)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/pe-ai-library](https://github.com/GlobalShopSolutions-InternalTools/pe-ai-library)
- **Related apps:** [mcp-artifacts](mcp-artifacts.md), [agents](agents.md)
- **Last reviewed:** 2026-05-07
