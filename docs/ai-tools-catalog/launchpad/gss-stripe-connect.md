---
title: "gss-stripe-connect"
description: "A tool to onboard customers as GSS Stripe Connect Accounts — the workflow that lets a customer accept payments through GSS using Stripe under the hood."
sidebar_position: 21
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

# gss-stripe-connect

> **TL;DR** — A tool to onboard customers as GSS Stripe Connect Accounts — the workflow that lets a customer accept payments through GSS using Stripe under the hood.

## Overview

Stripe Connect is Stripe's product for letting platforms (like GSS) onboard their own customers as merchants. gss-stripe-connect wraps that onboarding into a guided internal workflow — collecting the right info, creating the Stripe account, linking it to the customer record, and handling the verification dance.

## Why use it

- **One workflow for the whole onboarding.** No bouncing between Stripe's dashboard, our internal tools, and the customer.
- **Consistency.** Every customer goes through the same steps in the same order.
- **Fewer support tickets.** A guided flow catches errors up front (missing tax info, bad bank routing) instead of weeks later.

## When to use it

- A customer is signing up for GSS payment processing and needs a Stripe Connect account.
- An existing customer needs to update or re-verify their Stripe account.

Don't use it for:
- Direct Stripe account management for GSS itself — that's a different workflow.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/gss-stripe-connect](https://launchpad.globalshopsolutions.dev/apps/gss-stripe-connect) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Access:** Restricted to staff who handle customer onboarding. Request via the app page.

## How to use it

1. Launch the app.
2. Start a new onboarding for the customer.
3. Walk through the guided steps — info, verification, account creation.
4. Confirm the Stripe account is linked correctly to the customer record in our system.

## Common questions

**What if the customer is in a country Stripe doesn't support?**
Check Stripe's supported-countries list before starting onboarding. Unsupported = can't proceed.

**Where do payouts go?**
To the customer's connected bank account, per their Stripe Connect setup.

## Owner & support

- **Owner:** mfranzen@gssmail.com
- **App page:** [gss-stripe-connect on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/gss-stripe-connect)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-stripe-connect](https://github.com/GlobalShopSolutions-InternalTools/gss-stripe-connect)
- **Last reviewed:** 2026-05-04
