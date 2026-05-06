---
title: "quick-option-import-export-web"
description: "A web-based tool for importing, editing, and exporting Option JSON files. Spreadsheet-style editing with full round-trip preservation, so the JSON you e..."
sidebar_position: 33
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A web-based tool for importing, editing, and exporting Option JSON files. Spreadsheet-style editing with full round-trip preservation, so the JSON you export matches what the WinForm tool would produce.

## Overview

Editing Option JSON files by hand is painful — they're nested, have fields with specific formatting requirements, and one wrong character makes the file unusable. The original WinForm tool solved this on the desktop. `quick-option-import-export-web` is the web version: same workflow, but in a browser.

You open a JSON file, edit it in an Excel-like grid, and save it back. The tool preserves all the metadata you didn't touch (audit trail, legacy fields, DB config, datetime values), so the exported file is byte-compatible with what the WinForm tool would have produced.

## Why use it

- **No install required.** Open the URL; you're editing.
- **Familiar UX.** AG Grid gives you spreadsheet-style editing.
- **WinForm-compatible output.** Exact same format the legacy tool emits — including quirks like whole numbers without decimal points and 7-digit millisecond precision on timestamps.
- **Validation.** Server-side validation on every editable field; you don't ship a broken file.

## When to use it

- You need to edit an Option JSON file and don't want to install the WinForm tool.
- You're bulk-editing many rows and want spreadsheet-style cell editing.
- You're scripting a workflow that produces Option JSON and want to verify it visually before shipping.

Don't use it for:
- Files larger than 10 MB — the upload caps there.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/quick-option-import-export-web](https://launchpad.globalshopsolutions.dev/apps/quick-option-import-export-web) → **Launch**.
- **Production URL:** `quick-option-import-export-web.globalshopsolutions.dev`
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Click **Open** to upload a JSON file (up to 10 MB).
2. Edit cells inline. The grid supports add/delete row actions.
3. Click **Save As** to export the edited JSON.
4. The toolbar exposes: **Home**, **New** (blank doc), **Open**, **Clear** (drop all rows), **Save As**.

The grid prompts you with an "unsaved changes" warning if you try to navigate away mid-edit.

### Editable fields

| Column | JSON path | Type |
|---|---|---|
| Option Number | `OptionNumber` | int |
| Sequence | `OptionSequence` | int |
| Text | `Information.Text` | string |
| ASCII | `Information.AsciiFlag` | string |
| Boolean | `Information.Boolean` | bool |
| Long | `Information.Long` | long |
| Numeric | `Information.Numeric` | decimal |

Other properties (`SystemAudit`, `Obsolete`, `Db`, `Information.DateTime`, plus any unknown keys) are preserved exactly as imported.

## Common questions

**Will my edits drop fields I didn't touch?**
No — round-trip preservation is a core feature. Anything not in the editable set above is kept verbatim.

**Is the output the same as the WinForm tool's?**
Yes, by design. Numeric whole-number formatting, datetime precision, and special-character handling all match.

**What happens to my file after I close the tab?**
The web app processes uploads in-session. Once you save the export and leave, the server-side copy is gone.

## How it works

- **Stack:** ASP.NET Core (.NET 8) backend + React (TypeScript) + AG Grid frontend.
- **Validation:** Server-side, applied to every editable field on save.
- **Deploy:** Containerized (Docker) and deployed via the GSS Hoster self-service platform.

## Owner & support

- **Owner:** *Confirm with the LaunchPad app page (no individual owner listed on the tile).*
- **App page:** [quick-option-import-export-web on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/quick-option-import-export-web)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/quick-option-import-export-web](https://github.com/GlobalShopSolutions-InternalTools/quick-option-import-export-web)
- **Last reviewed:** 2026-05-06
