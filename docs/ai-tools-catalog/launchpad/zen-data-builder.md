---
title: "zen-data-builder"
description: "A Windows desktop tool that browses, generates, and manages test data inside Actian Zen / Pervasive PSQL databases (the legacy ERP backend). Built for Q..."
sidebar_position: 38
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# zen-data-builder

> **TL;DR** — A Windows desktop tool that browses, generates, and manages test data inside Actian Zen / Pervasive PSQL databases (the legacy ERP backend). Built for QA, dev, and data-migration testing on GSSERP.

## Overview

GSSERP's legacy backend runs on Actian Zen / Pervasive PSQL. Generating useful test data by hand — meaningful customer records, sales orders that exercise the right code paths — is tedious and error-prone. zen-data-builder is a Windows Forms app that connects to a Zen database via ODBC and lets you browse tables, generate rows with realistic-looking values, and manage the test data lifecycle (seed, refresh, tear down).

## Why use it

- **Faster QA setup.** Spin up a populated test database in minutes instead of writing INSERT statements by hand.
- **Reproducibility.** Save data templates and re-run them when you need a fresh baseline.
- **Migration testing.** Generate the kind of edge-case data that breaks migrations in dev so they don't break in customer environments.

## When to use it

- You're testing a feature that needs a populated GSSERP database.
- You're validating a data migration and want a wide variety of starting states.
- You're writing/maintaining test data for a long-running QA scenario.

Don't use it for:
- Production databases. This is for test environments only.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/zen-data-builder](https://launchpad.globalshopsolutions.dev/apps/zen-data-builder)
- **Login:** Global Shop Office 365 SSO (to view the LaunchPad page); the app itself is a Windows download.
- **Status:** Live.
- **Access:** Request access via the app page if you can't see the download.

## How to use it

1. From the LaunchPad app page, download the latest build.
2. Configure an ODBC connection to your local Zen / Pervasive test database.
3. Browse tables to confirm the connection is good.
4. Use the generate panel to seed rows — pick a table, choose row count, and either let the tool generate values or supply your own.

## Common questions

**Does it work with non-Zen databases?**
No — built specifically for Actian Zen / Pervasive PSQL via ODBC.

**Can I save and share my generation profiles?**
Yes — profiles are saved locally and can be shared between team members.

**Will this overwrite existing test data?**
Only if you tell it to. The tool defaults to additive inserts.

## How it works

A Windows Forms (.NET) application that uses ODBC to talk to Zen / Pervasive databases. Generation logic uses configurable templates per column type, with seed/replay support for reproducibility.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page — confirm before publishing.*
- **App page:** [zen-data-builder on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/zen-data-builder)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/zen-data-builder](https://github.com/GlobalShopSolutions-InternalTools/zen-data-builder)
- **Last reviewed:** 2026-05-04
