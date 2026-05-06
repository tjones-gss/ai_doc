---
title: "Globe TV"
description: "GSS's internal home for help videos. Think of it as a private YouTube — staff can find, watch, and (with permissions) upload short-form videos that expl..."
sidebar_position: 20
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — GSS's internal home for help videos. Think of it as a private YouTube — staff can find, watch, and (with permissions) upload short-form videos that explain how things work, walk through customer scenarios, or capture training content.

## Overview

Globe TV is a video platform built specifically for Global Shop Solutions. It hosts help and training videos in one searchable place — by topic, by department, by tag — so people don't have to chase links across SharePoint, Teams threads, and Outlook attachments. The frontend is a clean dark-mode-first interface modeled on YouTube's familiar grid layout.

## Why use it

Most "where do I find that video?" questions end with someone forwarding an email from 2023. Globe TV solves that:

- One link, one search, one home for all internal video content.
- Videos are tagged by department and topic so browsing is easy.
- Permissions and uploads are managed centrally.

## When to use it

- You need a quick refresher on a process and remember someone made a video about it.
- You're onboarding and want to see how a feature works rather than read a doc.
- You're a content creator (support, training, P&E) and want your video to be findable.

Don't use it for:
- Customer-facing video distribution. Globe TV is internal.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/globe-tv](https://launchpad.globalshopsolutions.dev/apps/globe-tv) → click **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Access:** All staff.

## How to use it

1. Click **Launch** from the LaunchPad tile.
2. Use the search bar at the top, or browse by department / tag from the navigation.
3. Click a video card to open the watch page.
4. To upload (if you have permissions), use the upload button in the header and follow the prompts.

## Common questions

**Who can upload?**
Upload permissions are controlled per department. If you need to upload and don't have access, ask your manager or the Globe TV owner.

**Where are the videos stored?**
On internal GSS infrastructure. Not on YouTube or external cloud video hosts.

**Is there a mobile experience?**
The web app is responsive — mobile/tablet should work. There's no dedicated mobile app at this time.

## How it works

Globe TV is a React 19 + Vite + Tailwind frontend wired to an internal API that handles video metadata, search indexing, and storage. Videos play through a standard web player; thumbnails and metadata are indexed for fast search. AI features (transcription, search by spoken content, etc.) may be added over time.

## Owner & support

- **Owner:** Travis Jones (tjones@gssmail.com)
- **App page:** [globe-tv on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/globe-tv)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/globe-tv](https://github.com/GlobalShopSolutions-InternalTools/globe-tv)
- **Last reviewed:** 2026-05-05
