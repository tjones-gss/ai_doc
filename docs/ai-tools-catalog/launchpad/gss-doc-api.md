---
title: "gss-doc-api"
description: "An API that uses OpenAI to parse data out of documents. Currently supports invoices and sales orders, with more document types planned. Live at docs.glo..."
sidebar_position: 26
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — An API that uses OpenAI to parse data out of documents. Currently supports invoices and sales orders, with more document types planned. Live at `docs.globalshopsolutions.ai`.

## Overview

Customer-supplied documents (invoices, sales orders) often need their data lifted into structured JSON for ingestion into GSSERP or downstream tools. Doing that by hand is slow and error-prone. `gss-doc-api` runs the documents through OpenAI's vision/parsing models and returns structured JSON.

Today it understands invoices and sales orders. Other document types are on the roadmap.

## Why use it

- **Removes manual data entry.** Drop in a PDF or image; get structured JSON out.
- **Consistent extraction.** AI handles formatting variation across customers.
- **API-first.** Integrate it into any workflow that needs document parsing.

## When to use it

- You're integrating customer document ingestion into a workflow.
- You're prototyping a feature that requires structured data from docs.
- You're auditing an existing parser's accuracy.

## How to access it

- **Live API:** [https://docs.globalshopsolutions.ai/](https://docs.globalshopsolutions.ai/)
- **LaunchPad:** [https://launchpad.globalshopsolutions.dev/apps/gss-doc-api](https://launchpad.globalshopsolutions.dev/apps/gss-doc-api)
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.

## Example

Send an invoice (image or PDF). Response shape:

```json
{
  "message": "Document parsing complete.",
  "documents": [
    {
      "document_id": "6b1fc25d-328d-435d-998a-492b3a358e0f",
      "document_type": "INVOICE",
      "company_id": "DEV",
      "...": "structured fields extracted from the invoice"
    }
  ]
}
```

## Common questions

**What document types are supported?**
Invoices and sales orders today. Roadmap includes more — confirm with the owner.

**Does my customer's data go to OpenAI?**
Yes — the API calls OpenAI to do the parsing. Treat it like any cloud AI: don't pass anything that exceeds your customer-data-handling clearance.

**How accurate is the extraction?**
LLM-quality, which is high for clean docs and lower for blurry / handwritten ones. Always have a verification step downstream.

## Owner & support

- **Owner:** msvystun@gssmail.com
- **App page:** [gss-doc-api on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/gss-doc-api)
- **Repo:** [https://github.com/GlobalShopSolutions-InternalTools/gss-doc-api](https://github.com/GlobalShopSolutions-InternalTools/gss-doc-api)
- **Last reviewed:** 2026-05-07
