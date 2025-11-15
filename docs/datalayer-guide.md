---
title: DataLayer Guide
description: How our proprietary DataLayer bridges COBOL programs with VB.NET and modern C# services
tags: [datalayer, cobol, vbnet]
sidebar_position: 13
last_updated: 2025-02-20
---

# DataLayer Guide

DataLayer is our proprietary extension of the Acu/Fujitsu COBOL runtime that exposes VB.NET objects/methods so legacy code can participate in OOP workflows.

## Core Concepts

- **Shim classes:** VB.NET classes (under `src/services/datalayer/Shims/`) wrap COBOL entry points and expose method-style APIs.
- **Marshalling:** Use the `DLMarshaller` helpers to convert copybook records ↔ VB.NET DTOs. Keep schemas in `src/services/datalayer/Schemas/`.
- **Context slots:** Store unit-of-work context (user, plant, workstation) using `DLContext.Set()`; never rely on global variables inside COBOL paragraphs.

## Authoring Workflow

1. Define/extend the copybook and update the JSON schema to keep modern services aligned.
2. Add or update the VB.NET shim method, documenting input/output contracts.
3. Reference the shim from COBOL via `CALL 'DLENTRY' USING BY CONTENT 'Shim.Method' ...`.
4. Add integration tests in `src/services/datalayer/tests/` plus COBOL harness cases.

## Best Practices

- **Version everything:** Use semantic version folders (`Schemas/v2/`) and note breaking changes in `docs/templates.md` release note sections.
- **Avoid business logic inside shims:** Keep them thin orchestration layers; COBOL or C# modules should own logic.
- **Telemetry hooks:** Call `DLTelemetry.Track()` with correlation IDs so modern observability tooling can trace hybrid transactions.
- **AI context:** When prompting Cursor or Codex, include both the copybook and shim file using `@Files` so the AI understands the contract end-to-end.

## Common Patterns

| Scenario                                      | Approach                                                                                                          |
| --------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| COBOL needs async notification                | Shim pushes message to Azure Service Bus via `DLAsyncPublisher`; COBOL waits on status flag.                      |
| Modern service must reuse COBOL business rule | Call DataLayer shim via `Datalayer.Client` in C#; include fallback to managed implementation behind feature flag. |
| VB.NET needs strong typing                    | Generate partial classes via `dotnet run --project scripts/DatalayerSchemaGen.csproj`.                            |

## Troubleshooting

- **Mismatch errors:** If COBOL throws `DL-MAP-ERR`, compare the JSON schema hash in the shim with the copybook version.
- **Performance hotspots:** Profile with `DLProfiler` (wrap calls in `USING DL-PROFILE-ON`). Consider caching read-only datasets in the shim rather than COBOL.
- **Deployment drift:** Ensure Octopus Deploy packages both COBOL artefacts and VB.NET assemblies so DataLayer stays in sync.
