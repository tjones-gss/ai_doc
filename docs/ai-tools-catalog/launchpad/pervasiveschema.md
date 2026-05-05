---
title: "pervasiveschema"
description: "A LaunchPad-hosted MCP server exposing the schema of all Pervasive tables and columns, plus their mappings to the EO Objects namespace (object name, gro..."
sidebar_position: 29
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A LaunchPad-hosted MCP server exposing the schema of all Pervasive tables and columns, plus their mappings to the EO Objects namespace (object name, grouping, property names). Lets AI assistants answer schema questions without you opening a DB tool.

## Overview

The Pervasive schema is the foundation of GSSERP's data layer, and the EO Objects namespace is the application-level mapping on top. Understanding both is essential for anyone touching data, but neither is easy to browse by hand. `pervasiveschema` exposes both via MCP so Claude Code can answer questions like "what columns are on the IM_INVENTORY table?" or "which EO Object property maps to OE_HEADER.HEADER_ID?".

## Why use it

- **Schema lookups in conversation.** No need to open a DB browser to confirm a column type.
- **EO mapping clarity.** Quickly find which EO Object property corresponds to a Pervasive column.
- **Refactor support.** When changing a table or column, list everywhere it's referenced via the EO mapping.

## When to use it

- You're writing code that touches Pervasive tables and need quick column reference.
- You're translating between Pervasive table names and EO Object names.
- You're reviewing a PR and want to verify schema usage is correct.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/pervasiveschema](https://launchpad.globalshopsolutions.dev/apps/pervasiveschema) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Configured (not yet Live — confirm with owner before relying on it in critical workflows).
- **Kind:** MCP server.

## Owner & support

- **Owner:** aakram@gssmail.com
- **App page:** [pervasiveschema on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/pervasiveschema)
- **Related apps:** [arc-scanner](arc-scanner.md), [zen-data-builder](zen-data-builder.md)
- **Last reviewed:** 2026-05-05
