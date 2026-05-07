---
title: "DashboardDesAIgner"
description: "An AI agent that converts plain-English descriptions into DevExpress Dashboard Designer XML. Describe the dashboard you want; it produces the .xml file ..."
sidebar_position: 2
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — An AI agent that converts plain-English descriptions into DevExpress Dashboard Designer XML. Describe the dashboard you want; it produces the .xml file you can open in Dashboard Designer.

## Overview

DevExpress Dashboard Designer dashboards are XML files under the hood. Building one by hand is tedious — drag controls, configure data sources, wire expressions, format styling. DashboardDesAIgner is an AI agent (LLM-driven) that takes a natural-language description ("a sales dashboard with monthly revenue trend, top 10 customers by sales, and a regional breakdown") and emits the corresponding Dashboard Designer XML.

A renderer microservice (Phase 3) can also produce JPEG/XLSX previews of the dashboard so you can see what you're getting without opening the Designer.

## Why use it

- **Faster prototyping.** Test a dashboard idea in seconds instead of an afternoon of clicking.
- **Consistent style.** The agent uses a knowledge base of controls, layouts, and styling rules — so output follows our conventions.
- **Iteration-friendly.** Don't like the result? Refine the prompt; the XML regenerates.

## When to use it

- You're scoping out a new dashboard and want a quick first draft.
- You're standardizing a set of dashboards across teams and want consistent output.
- You're prototyping for a stakeholder demo before committing to manual polish.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/DashboardDesAIgner](https://launchpad.globalshopsolutions.dev/apps/DashboardDesAIgner) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Configured (confirm Live state with owner).

## How it works

- **`agent/`** — Python orchestration layer. Parses intent, calls the LLM, generates XML.
- **`knowledge/`** — Markdown knowledge base fed to the LLM at runtime (controls, data sources, layout, styling, expressions).
- **`renderer/`** — .NET 8 microservice for rendering dashboards to JPEG / XLSX (Phase 3).
- **`DashboardSampleXml/`** — Reference dashboard XMLs with rendered screenshots.
- **`DashboardToolsScreenshots/`** — Designer UI screenshots documenting all control options.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [DashboardDesAIgner on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/DashboardDesAIgner)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/DashboardDesAIgner](https://github.com/GlobalShopSolutions-InternalTools/DashboardDesAIgner)
- **Last reviewed:** 2026-05-07
