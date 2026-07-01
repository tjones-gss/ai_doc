---
title: 'featuretoggles'
description: 'Controls which features are available in the system so teams can ship dark, roll out gradually, or kill-switch behavior without redeploying.'
sidebar_position: 21
last_updated: 2026-07-01
tags: [ai-tools, gss-internal, launchpad]
---

# featuretoggles

> **TL;DR** — Controls which features are available in the system. Lets teams ship code dark, roll functionality out gradually, or kill-switch behavior quickly without redeploying.

## Overview

Feature toggles (also called feature flags) decouple "code is deployed" from "feature is live." `featuretoggles` is the GSS internal admin UI for managing those switches so teams can control which functionality is visible in the system.

The app is a LaunchPad-hosted React + ASP.NET service. The React UI builds into the `FeatureToggle.Api` web root, and the integrated container serves the SPA and API from the same host on port 8080. That matters because the UI calls the API on the page origin unless it was built with a separate `VITE_API_URL`.

## Why use it

- **Ship code dark.** Merge a half-finished feature behind a flag, finish it later, only flip it on when ready.
- **Gradual rollout.** Enable for one customer, then five, then everyone.
- **Kill switch.** When something goes wrong in production, flip the flag instead of rolling back a deploy.
- **A/B testing.** Compare two implementations against each other.
- **AI-assisted delivery.** Cursor, Claude Code, Codex, and other assistants can add or adjust feature-flagged code while keeping release activation in human-controlled LaunchPad workflows.

## When to use it

- Before shipping a new feature — wrap it in a flag.
- During an incident — turn off a misbehaving feature without a deploy.
- During a customer-specific rollout — enable per-customer.
- When an AI-assisted change needs a safe activation path after review.

Don't use it for:

- Permanent configuration that's not actually a "toggle." Real config belongs elsewhere.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/featuretoggles](https://launchpad.globalshopsolutions.dev/apps/featuretoggles) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Access:** Restricted — typically engineers and release managers. Request via the app page.

## How to use it

1. Launch the app.
2. Find the toggle you want by name.
3. Edit its rules (on/off, per-customer overrides, percentage rollouts).
4. Save — the new state propagates to running systems.

## Setup notes

- **Integrated LaunchPad deploy:** Use the root `Dockerfile` when you want one image that builds the SPA, embeds it in `FeatureToggle.Api`, and runs `dotnet FeatureToggle.Api.dll`.
- **Same-host API:** Leave `VITE_API_URL` empty when the public UI and API share the same host, such as `https://featuretoggles.globalshopsolutions.dev`.
- **Static-only deploys:** If LaunchPad builds only the static frontend, set `VITE_API_URL` to the public API base URL and rebuild, or use the Caddy image with `API_UPSTREAM`.
- **Runtime config:** Provide `ConnectionStrings__DefaultConnection` and required secrets through the LaunchPad / Kubernetes environment.

## Common questions

**How fast does a change take effect?**
Near-real-time. Confirm specifics with the owner.

**Can I see who toggled what?**
Audit history is available — see the app for details.

**What if I need a flag that doesn't exist yet?**
Engineering creates flags as part of the code that uses them. Talk to the team adding the feature.

**The page loads, but toggles do not. What should I check?**
Confirm LaunchPad is serving either the integrated API + SPA image or a proxy/static build with the correct API URL. The UI expects `/_/feature-toggles` to resolve on the same origin unless `VITE_API_URL` points elsewhere.

## Owner & support

- **Owner:** [bbrambila@gssmail.com](mailto:bbrambila@gssmail.com)
- **App page:** [featuretoggles on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/featuretoggles)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/featuretoggles](https://github.com/GlobalShopSolutions-InternalTools/featuretoggles)
- **Last reviewed:** 2026-07-01
