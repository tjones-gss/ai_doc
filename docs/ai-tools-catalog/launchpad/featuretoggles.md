---
title: "featuretoggles"
description: "Controls which features are turned on in the system. Lets us ship code dark, roll a feature out gradually, or kill-switch something quickly without rede..."
sidebar_position: 21
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# featuretoggles

> **TL;DR** — Controls which features are turned on in the system. Lets us ship code dark, roll a feature out gradually, or kill-switch something quickly without redeploying.

## Overview

Feature toggles (also called feature flags) let you decouple "code is deployed" from "feature is live." featuretoggles is the GSS internal admin UI for managing those flags — turning a feature on for everyone, on for a single customer, off for everyone, or on for a specific test user.

## Why use it

- **Ship code dark.** Merge a half-finished feature behind a flag, finish it later, only flip it on when ready.
- **Gradual rollout.** Enable for one customer, then five, then everyone.
- **Kill switch.** When something goes wrong in production, flip the flag instead of rolling back a deploy.
- **A/B testing.** Compare two implementations against each other.

## When to use it

- Before shipping a new feature — wrap it in a flag.
- During an incident — turn off a misbehaving feature without a deploy.
- During a customer-specific rollout — enable per-customer.

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

## Common questions

**How fast does a change take effect?**
Near-real-time. Confirm specifics with the owner.

**Can I see who toggled what?**
Audit history is available — see the app for details.

**What if I need a flag that doesn't exist yet?**
Engineering creates flags as part of the code that uses them. Talk to the team adding the feature.

## Owner & support

- **Owner:** bbrambila@gssmail.com
- **App page:** [featuretoggles on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/featuretoggles)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/featuretoggles](https://github.com/GlobalShopSolutions-InternalTools/featuretoggles)
- **Last reviewed:** 2026-05-04
