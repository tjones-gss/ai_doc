---
title: "qchat"
description: "Schedules quarterly check-ins across teams so managers can publish slots, members can book, and calendar invites can be sent through Microsoft Graph."
sidebar_position: 42
last_updated: 2026-07-01
tags: [ai-tools, gss-internal, launchpad]
---

# qchat

> **TL;DR** — Schedules quarterly check-ins across teams. Managers publish timeslots, members book one check-in per quarter, and calendar invites can be sent through Microsoft Graph.

## Overview

Most people mean to do quarterly check-ins. They forget. `qchat` is a LaunchPad-hosted scheduling helper for making those conversations happen consistently across teams.

Managers create available timeslots, members book a check-in with their manager, and the app can send calendar invites through Microsoft Graph. Admins can manage users, teams, quarters, and view the experience as another user when support or rollout troubleshooting is needed.

## Why use it

- **The check-in actually happens.** A nudge beats good intentions.
- **Less calendar Tetris.** Pick from suggested times instead of mailing back-and-forth.
- **Consistent manager process.** Teams use the same quarterly cadence instead of each manager inventing a tracking sheet.
- **Calendar integration.** Microsoft Graph can create the meeting invite when the environment is configured for it.
- **AI platform context.** Cursor, Claude Code, Codex, and other assistants can reference QChat as the scheduling system for quarterly people-process workflows without binding to a specific coding tool.

## When to use it

- You manage someone (or are managed) and want quarterly check-ins on autopilot.
- You're rolling out a new manager process and want consistency.
- You're an admin supporting quarter setup, team assignments, or a migration from the older PPT QChat data.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/qchat](https://launchpad.globalshopsolutions.dev/apps/qchat) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## How to use it

1. Launch the app.
2. Managers publish timeslots for the active quarter.
3. Members pick one available slot from their manager's calendar.
4. Confirm the booking — when Graph is configured, a calendar invite is sent.

## Roles

- **Admin:** Full manager experience, user/team/quarter management, and view-as support.
- **Director / People Manager:** Manager experience; can optionally be included as an attendee on check-ins.
- **Manager:** Creates timeslots and views their team dashboard.
- **Member:** Books one check-in per quarter from their manager's available slots.

## Setup notes

For local development, the repo runs a TypeScript client and API with Postgres:

1. Start Postgres with `docker compose up -d`.
2. Install dependencies with `npm install`.
3. Copy `.env.example` to `server/.env` and fill in database/session settings.
4. Run `npm run dev` to start the client at `http://localhost:5173` and API at `http://localhost:3001`.

Required production settings include the Postgres connection values, `SESSION_SECRET`, and `ADMIN_EMAIL`. `GRAPH_CLIENT_ID`, `GRAPH_TENANT_ID`, and `GRAPH_CLIENT_SECRET` enable Microsoft Graph authentication and calendar integration; leaving them blank disables auth/calendar integration for local development.

Deployment uses a Docker image with Caddy for the static SPA and Node for the API on port 8080, with LaunchPad manifests under `.deploy/`.

## Owner & support

- **Owner:** asmith@gssmail.com
- **App page:** [qchat on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/qchat)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/qchat](https://github.com/GlobalShopSolutions-InternalTools/qchat)
- **Related app:** [career-path-tracker](career-path-tracker.md) (the data you might bring to the check-in).
- **Last reviewed:** 2026-05-04
