---
title: "bom-compare-generator"
description: "A Python tool that generates a Bill of Material .xlsx file with user-defined or randomly-generated parts, used as input for BOM Compare testing."
sidebar_position: 9
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# bom-compare-generator

> **TL;DR** — A Python tool that generates a Bill of Material `.xlsx` file with user-defined or randomly-generated parts, used as input for BOM Compare testing.

## Overview

When QA tests BOM Compare scenarios, they need realistic-looking BOM spreadsheets — with parts, quantities, structure — to feed into the tool. Building those by hand is tedious. bom-compare-generator lets you specify a few parameters (number of parts, structure depth, sample data style) and produces a populated `.xlsx`.

## Why use it

- **Consistent test inputs.** Same parameters produce comparable BOM files for repeat testing.
- **Variety on demand.** Generate edge-case BOMs (very large, very deep, very flat) to stress test.
- **Saves QA time.** Minutes instead of hours of manual data entry.

## When to use it

- You're testing BOM Compare behavior and need a fresh BOM file.
- You're regression-testing BOM-related changes and want a reproducible input.

Don't use it for:
- Real customer BOMs. This generates synthetic data.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/bom-compare-generator](https://launchpad.globalshopsolutions.dev/apps/bom-compare-generator)
- **Login:** Global Shop Office 365 SSO (LaunchPad).
- **Status:** Live.
- **Kind:** Tool (Python — typically run locally or via a CI step).

## How to use it

1. Download or run the tool from the LaunchPad page.
2. Provide parameters (part count, structure, randomness seed if you want repeatability).
3. The tool writes an `.xlsx` to the path you specify.
4. Use that file as input for BOM Compare.

## Common questions

**Can I supply my own parts list?**
Yes — supply a CSV of parts to use instead of random generation.

**Is the output deterministic?**
Yes if you set a seed. Without a seed, runs differ.

## Owner & support

- **Owner:** ashirley@gssmail.com
- **App page:** [bom-compare-generator on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/bom-compare-generator)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/bom-compare-generator](https://github.com/GlobalShopSolutions-InternalTools/bom-compare-generator)
- **Last reviewed:** 2026-05-04
