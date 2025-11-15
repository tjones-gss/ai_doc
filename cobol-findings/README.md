# COBOL Repository Findings

## Directory inventory

- `cobol/cobol/anssource/` - ANSI-era COBOL programs (BRWRC*, DL*, BSI\*) that predate Fujitsu conversions; useful for regression comparisons or Codex diffs when customers ask how things used to run.
- `cobol/cobol/fujsource/` - primary Fujitsu NetCOBOL library (~2k programs) with `@OPTIONS`, modern copybook usage, and .NET-facing business modules.
- `cobol/cobol/fujsource64/` - select 64-bit builds that mirror `fujsource` for x64 runtimes; keep aligned with `fujcopy64` definitions.
- `cobol/cobol/source/` - legacy NetCOBOL mirror for many program IDs; taps modernization deltas or resurrects removed PERFORM paths.
- `cobol/cobol/copy/` - legacy copybooks, panels, and QPR layouts that expose historical record shapes.
- `cobol/cobol/fujcopy/` - modern copybook catalog (FD/SL/NM/LK/SP2/REP/EO/EIO/WS) referenced by almost every Fujitsu program.
- `cobol/cobol/fujcopy64/` - x64-specific copybooks; double-check numeric storage or ALIGNMENT rules when mixing with 32-bit code.
- `cobol/cobol/fujprojects/` - 300+ Visual Studio / NetCOBOL project folders containing `.cobproj`, `.sln`, `Properties/`, `listing.txt`, and build configuration files.
- `cobol/cobol/Projects/` - active conversion workspaces (AP0043GI, CalculateShipmentTotals, etc.) with runnable `.cobproj`, `bin/`, `obj/`, icons, and listings.
- `cobol/cobol/nonfujprojects/` - .NET helper libraries (CacheDLLs, FujitsuHelper, GSSERP.Shared, etc.) that wrap pinvokes, caching layers, and external service hooks.
- `cobol/cobol/panels/` - SP2 `.PNL` screen sources tied 1:1 to UI programs such as `AP0043GI`.
- `cobol/cobol/sp2panels/` - folderized SP2 panel definitions (e.g., `ORD147APanel`) used by Cursor when drilling into UI flows.
- `cobol/cobol/distributables/` - deployment payloads split into `files/` (seed data such as `FUJPM`, `USSTE`), `globalbin/` (SP2 runtime DLLs), and `plugins/`.
- `cobol/cobol/fujitsu/` - compiler response templates (`stddebug.cfg`, `stdcomp64.cfg`) consumed by project files via `CompilerResponseFile`.
- `cobol/cobol/fixes/`, `internal/`, `obsolete/`, `obsoleteACU/`, `obsoletefuj*/` - corrals for emergency patches, private investigations, and archived Fujitsu/ACU artifacts.
- `cobol/cobol/distributables/files/*` - curated datasets copied into QA or customer drops; mention them when automating scenario specs.

## Reference docs already in repo

- `cobol/cobol/AI_Support_Guide.md` captures orientation, build details, Codex/Cursor usage, and next steps; treat it as the baseline playbook every AI request should import.

## Scale metrics (fast counts)

- 8,267 `.CBL` programs across `fujsource`, `source`, and `anssource`.
- 3,470 `.FD` copybooks and 474 `.CPY` files (plus `.SL/.NM/.LK` siblings) primarily under `fujcopy/`.
- 5,743 `.SP2` panel files spanning legacy `panels/`, modern `sp2panels/`, and copybook-driven UI definitions.
- 325 `.cobproj` files and 324 `.sln` files inside `fujprojects/` and `Projects/`.
- Hundreds of binary assets (icons, DLLs, datasets) inside `distributables/` and per-project folders.

## Observations for AI adoption

- Copybook churn necessitates explicit context: every AI prompt should bundle the relevant `FD/SL/NM/LK` plus the program body, otherwise field-level edits will hallucinate.
- `Projects/*/*/listing.txt` is the canonical Fujitsu compiler report; surfacing it in Cursor chats accelerates diagnostics.
- `nonfujprojects/` holds the .NET bridges that many Fujitsu programs invoke through `CALL "GSSERP.Shared"`-style wrappers, so Codex can reason about cross-language hops when snippets are supplied.
- SP2 panels live both as `.PNL` and `.SP2`; highlighting both paths lets Cursor map UI events to working-storage fields.
- Legacy folders (`anssource/`, `source/`, `copy/`) are excellent for AI-driven regression summaries or auto-built specs that explain modernization differences.

## Fast-follow opportunities

- Generate high-value program one-pagers (AP0043GI, SYS080, etc.) by feeding Codex both Fujitsu and legacy sources, then store them next to `AI_Support_Guide.md`.
- Script Cursor tasks that capture `listing.txt` runs plus diffed copybook snapshots whenever a change request touches `fujcopy/`.
- Build an index (CSV/JSON) of copybook-to-program relationships with Codex; this enables smarter prompt scaffolding for bug hunts and customer requests.
