#!/usr/bin/env bash
# Daily doc-refresh — autonomous catalog refresh.
# Runs from .github/workflows/daily-doc-refresh.yml on a Mon-Fri morning cron.
#
#   1. Lists repos in GlobalShopSolutions-InternalTools (activity, add/remove,
#      pushed_at changes, and catalog coverage vs. saved state).
#   2. Fetches Cursor / Claude Code / Codex / ChatGPT version info.
#   3. Opens (or comments on) a daily GitHub issue. If any repo needs a new or
#      refreshed article, the issue is labeled `needs-articles`.
#   4. For each brand-new repo, changed repo, or repo missing catalog coverage,
#      writes a HANDOFF FILE into this repo at data/new-tools/<slug>.json — repo
#      metadata + README pulled with GH_PAT.
#   5. Applies the handoffs into catalog articles, deletes consumed handoffs,
#      regenerates the overview, and commits the docs + state STRAIGHT TO MAIN.
#
# No LLM / API key here. Cursor org access NOT required — this workflow does the
# cross-org reading with GH_PAT and writes catalog docs in-repo.
#
# Required env:
#   GH_TOKEN            - token for this repo (issues); push fallback
#   GITHUB_REPOSITORY   - owner/repo (provided by Actions)
#   GH_PAT              - PAT: READ on the org + WRITE on this repo
#                         (classic `repo` scope covers both)
# Optional env:
#   DRY_RUN=1           - print findings; don't write/commit/push
#   SKIP_DRAFT=1        - skip writing/applying handoffs (still posts issue + state)

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
STATE_FILE="$ROOT/data/known-versions.json"
OVERVIEW_PATH="$ROOT/docs/ai-tools-catalog/overview.md"
NEWTOOLS_DIR="$ROOT/data/new-tools"
DATE_UTC="$(date -u +%Y-%m-%d)"
ISSUE_TITLE="AI Tools deltas — $DATE_UTC"

DRY_RUN="${DRY_RUN:-0}"
SKIP_DRAFT="${SKIP_DRAFT:-0}"
ORG="GlobalShopSolutions-InternalTools"
EXTRA_LABEL=""

NEW_REPOS_FILE="/tmp/refresh-new-repos.txt"
MISSING_CATALOG_FILE="/tmp/refresh-missing-catalog.txt"
UPDATED_REPOS_FILE="/tmp/refresh-updated-repos.txt"
HANDOFF_QUEUE_FILE="/tmp/refresh-handoff-queue.jsonl"
: > "$NEW_REPOS_FILE"
: > "$MISSING_CATALOG_FILE"
: > "$UPDATED_REPOS_FILE"
: > "$HANDOFF_QUEUE_FILE"
echo 0 > /tmp/refresh-handoffs.txt

log()  { printf "==> %s\n" "$*" >&2; }
warn() { printf "WARN: %s\n" "$*" >&2; }
state_get() { jq -r "$1 // \"\"" "$STATE_FILE" 2>/dev/null || echo ""; }
fetch() { curl -fsSL --max-time 30 -A "ai_doc daily-refresh bot" "$1" 2>/dev/null || echo ""; }
section() { printf "\n## %s\n\n%s\n" "$1" "$2"; }
slugify() { echo "$1" | tr '[:upper:]' '[:lower:]' | sed -e 's/[^a-z0-9]\+/-/g' -e 's/^-//' -e 's/-$//'; }

fetch_internal_tools_repos() {
  local token="$1" page=1 page_json all_json="[]"
  while :; do
    page_json=$(curl -fsSL --max-time 30 \
      -H "Authorization: Bearer $token" \
      -H "Accept: application/vnd.github+json" \
      "https://api.github.com/orgs/$ORG/repos?per_page=100&page=$page" 2>/dev/null) || page_json="[]"
    if [ -z "$page_json" ] || [ "$page_json" = "null" ]; then
      page_json="[]"
    fi
    all_json=$(jq -s '.[0] + .[1]' <(printf "%s" "$all_json") <(printf "%s" "$page_json"))
    [ "$(printf "%s" "$page_json" | jq 'length')" -lt 100 ] && break
    page=$((page+1))
  done
  printf "%s" "$all_json" | jq '[.[] | {name, description, pushed_at, created_at, archived, html_url, topics}]'
}

write_catalog_coverage() {
  local current_names_file="$1" output_file="$2"
  python3 - "$ROOT" "$current_names_file" "$output_file" <<'PY'
import re
import sys
from pathlib import Path

root = Path(sys.argv[1])
current_names_file = Path(sys.argv[2])
output_file = Path(sys.argv[3])
catalog_root = root / "docs" / "ai-tools-catalog"

repos = [line.strip() for line in current_names_file.read_text().splitlines() if line.strip()]
covered = set()

repo_pattern = re.compile(r"github\.com/GlobalShopSolutions-InternalTools/([^\)\]\s]+)")
app_pattern = re.compile(r"launchpad\.globalshopsolutions\.dev/apps/([^\)\]\s]+)")

for path in catalog_root.glob("**/*.md"):
    covered.add(path.stem)
    covered.add(path.stem.lower())
    text = path.read_text(encoding="utf-8")
    for match in repo_pattern.finditer(text):
        repo = match.group(1).rstrip("/.,")
        covered.add(repo)
        covered.add(repo.lower())
    for match in app_pattern.finditer(text):
        app = match.group(1).rstrip("/.,")
        covered.add(app)
        covered.add(app.lower())

missing = [repo for repo in repos if repo not in covered and repo.lower() not in covered]
output_file.write_text("\n".join(missing) + ("\n" if missing else ""), encoding="utf-8")
PY
}

build_handoff_queue() {
  python3 - "$NEW_REPOS_FILE" "$MISSING_CATALOG_FILE" "$UPDATED_REPOS_FILE" "$HANDOFF_QUEUE_FILE" <<'PY'
import json
import sys
from pathlib import Path

new_file, missing_file, updated_file, queue_file = [Path(arg) for arg in sys.argv[1:]]

priority = {"new": 0, "missing_catalog": 1, "refresh": 2}
reason_by_type = {
    "new": "New repository detected in GlobalShopSolutions-InternalTools.",
    "missing_catalog": "Repository is present in InternalTools but has no catalog page, repo link, or LaunchPad app reference.",
    "refresh": "Repository pushed_at changed since the last catalog scan.",
}

queue = {}
for change_type, path in [
    ("new", new_file),
    ("missing_catalog", missing_file),
    ("refresh", updated_file),
]:
    if not path.exists():
        continue
    for raw in path.read_text(encoding="utf-8").splitlines():
        name = raw.strip()
        if not name:
            continue
        current = queue.get(name)
        if current is None or priority[change_type] < priority[current["change_type"]]:
            queue[name] = {
                "name": name,
                "change_type": change_type,
                "reason": reason_by_type[change_type],
            }

queue_file.write_text(
    "".join(json.dumps(queue[name], sort_keys=True) + "\n" for name in sorted(queue, key=str.lower)),
    encoding="utf-8",
)
PY
}

# ---- 1. Internal tools GitHub org activity -------------------------------
check_internal_tools() {
  log "Checking $ORG repos…"
  local now_epoch; now_epoch="$(date -u +%s)"
  local token="${GH_PAT:-$GH_TOKEN}"
  local repos_json
  repos_json=$(fetch_internal_tools_repos "$token") || repos_json="[]"

  if [ -z "$repos_json" ] || [ "$repos_json" = "[]" ] || [ "$repos_json" = "null" ]; then
    section "Internal tools (GitHub org)" "$(printf '_Could not fetch \`%s\` org repos._ Add a PAT with org read as `GH_PAT`.' "$ORG")"
    return
  fi
  printf "%s" "$repos_json" > /tmp/refresh-repos.json

  local recent_pushes
  recent_pushes=$(echo "$repos_json" | jq -r --argjson now "$now_epoch" '
    .[] | select((.pushed_at | fromdateiso8601) > ($now - 86400) and (.archived | not)) |
    "- [\(.name)](\(.html_url)) — \((.description // "_no description_") | tostring) (pushed \(.pushed_at | sub("T.*$"; "")))"' | head -25)

  local current_names previous_names added removed updated missing_catalog queue_count
  current_names=$(echo "$repos_json" | jq -r '[.[] | .name] | sort | .[]')
  previous_names=$(jq -r '.internal_tools.repos_seen[]?' "$STATE_FILE" 2>/dev/null | sort)
  added=$(comm -23 <(echo "$current_names") <(echo "$previous_names"))
  removed=$(comm -13 <(echo "$current_names") <(echo "$previous_names"))
  [ -n "$added" ] && echo "$added" | sed '/^$/d' > "$NEW_REPOS_FILE"

  echo "$current_names" > /tmp/refresh-current-repos.txt
  echo "$repos_json" | jq '
    map({
      key: .name,
      value: {
        description,
        pushed_at,
        created_at,
        archived,
        html_url,
        topics: (.topics // [])
      }
    }) | from_entries
  ' > /tmp/refresh-current-repo-snapshots.json

  updated=$(jq -r --slurpfile current /tmp/refresh-current-repo-snapshots.json '
    .internal_tools.repo_snapshots // {} as $previous
    | $current[0]
    | to_entries[]
    | select((.value.archived | not) and ($previous[.key] != null) and ($previous[.key].pushed_at != .value.pushed_at))
    | .key
  ' "$STATE_FILE" 2>/dev/null | sort)
  [ -n "$updated" ] && echo "$updated" | sed '/^$/d' > "$UPDATED_REPOS_FILE"

  write_catalog_coverage /tmp/refresh-current-repos.txt "$MISSING_CATALOG_FILE"
  missing_catalog=$(cat "$MISSING_CATALOG_FILE" 2>/dev/null || true)
  build_handoff_queue
  queue_count=$(wc -l < "$HANDOFF_QUEUE_FILE" | tr -d ' ')
  [ "${queue_count:-0}" -gt 0 ] 2>/dev/null && EXTRA_LABEL=",needs-articles"

  local body=""
  body+="### Coverage summary\n\n"
  body+="- Repos seen in \`$ORG\`: **$(echo "$current_names" | sed '/^$/d' | wc -l | tr -d ' ')**\n"
  body+="- Queued catalog handoffs: **${queue_count:-0}**\n"
  body+="- Missing catalog coverage: **$(echo "$missing_catalog" | sed '/^$/d' | wc -l | tr -d ' ')**\n"
  [ -n "$recent_pushes" ] && body+="\n### Pushed in last 24h\n\n$recent_pushes\n"
  if [ -n "$added" ]; then
    body+="\n### Added — queued for article drafting\n\n"; body+=$(echo "$added" | head -25 | sed 's/^/- /'); body+="\n"
  fi
  if [ -n "$missing_catalog" ]; then
    body+="\n### Missing catalog coverage — queued\n\n"; body+=$(echo "$missing_catalog" | head -50 | sed 's/^/- /'); body+="\n"
  fi
  if [ -n "$updated" ]; then
    body+="\n### Updated since last scan — queued for refresh\n\n"; body+=$(echo "$updated" | head -50 | sed 's/^/- /'); body+="\n"
  fi
  if [ -n "$removed" ]; then
    body+="\n### Removed since last refresh\n\n"; body+=$(echo "$removed" | head -25 | sed 's/^/- /'); body+="\n"
  fi
  if [ "${queue_count:-0}" -gt 0 ] 2>/dev/null; then
    body+="\nStage 2 should create or refresh catalog articles for the queued handoffs. This keeps the docs aligned with the eventual RAG-backed GSS Catalog.\n"
  fi
  section "Internal tools (GitHub org)" "$(printf "%b" "$body")"
}

# ---- 2-5. version sources (unchanged) ------------------------------------
check_cursor() {
  log "Checking Cursor changelog…"
  local html; html=$(fetch "https://cursor.com/changelog")
  if [ -z "$html" ]; then section "Cursor" "_Could not fetch Cursor changelog._"; return; fi
  local latest_date; latest_date=$(echo "$html" | grep -oE '20[0-9]{2}-[0-9]{2}-[0-9]{2}' | head -1)
  local known; known=$(state_get '.cursor.latest_changelog_date')
  local body
  if [ -z "$latest_date" ]; then body="_Couldn't parse a date. https://cursor.com/changelog_"
  elif [ "$latest_date" = "$known" ]; then body="Latest changelog entry: **$latest_date** (no change)"
  else body="Latest changelog entry: **$latest_date** (was: \`$known\`) — https://cursor.com/changelog"; fi
  echo "$latest_date" > /tmp/refresh-cursor-date.txt
  section "Cursor" "$body"
}
check_claude_code() {
  log "Checking Claude Code releases…"
  local rj; rj=$(gh api repos/anthropics/claude-code/releases/latest 2>/dev/null) || rj=""
  if [ -z "$rj" ]; then section "Claude Code" "_Could not fetch anthropics/claude-code latest._"; return; fi
  local tag name published known
  tag=$(echo "$rj" | jq -r '.tag_name // ""'); name=$(echo "$rj" | jq -r '.name // ""')
  published=$(echo "$rj" | jq -r '.published_at // "" | sub("T.*$"; "")'); known=$(state_get '.anthropic.claude_code_cli_release')
  local body="Latest CLI release: **$tag** ($name) published $published"
  [ -n "$known" ] && [ "$tag" != "$known" ] && body+="\n\n> ⚠ Bump from \`$known\`. https://github.com/anthropics/claude-code/releases/tag/$tag"
  echo "$tag" > /tmp/refresh-claude-tag.txt
  section "Claude Code" "$(printf "%b" "$body")"
}
check_codex() {
  log "Checking Codex CLI on npm…"
  local v; v=$(curl -fsSL --max-time 20 "https://registry.npmjs.org/@openai/codex/latest" 2>/dev/null | jq -r '.version // ""') || v=""
  local known; known=$(state_get '.openai.codex_cli_npm')
  local body
  if [ -z "$v" ]; then body="_Couldn't fetch @openai/codex from npm._"
  else body="Latest \`@openai/codex\` on npm: **$v**"; [ -n "$known" ] && [ "$v" != "$known" ] && body+="\n\n> ⚠ Bump from \`$known\`. https://www.npmjs.com/package/@openai/codex"; fi
  echo "$v" > /tmp/refresh-codex-version.txt
  section "Codex / OpenAI" "$(printf "%b" "$body")"   # FIX: render \n (was literal)
}
check_chatgpt() {
  log "Checking ChatGPT release notes…"
  local html; html=$(fetch "https://platform.openai.com/docs/changelog"); [ -z "$html" ] && html=$(fetch "https://openai.com/news/")
  local latest=""; [ -n "$html" ] && latest=$(echo "$html" | grep -oE '(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{1,2},? 20[0-9]{2}' | head -1)
  local body; if [ -n "$latest" ]; then body="Most-recent dated mention: **$latest**. https://platform.openai.com/docs/changelog"
  else body="_Couldn't auto-fetch a date._ https://platform.openai.com/docs/changelog"; fi
  section "ChatGPT / OpenAI" "$(printf '%b' "$body")"
}

# ---- 6. Issue creation (labels include needs-articles when relevant) -----
build_and_post_issue() {
  local body="$1" labels="automated,doc-refresh$EXTRA_LABEL"
  if [ "$DRY_RUN" = "1" ]; then log "[DRY_RUN] Would post issue: $ISSUE_TITLE (labels: $labels)"; printf "%s\n" "$body"; return; fi
  local existing
  existing=$(gh issue list --search "$ISSUE_TITLE in:title" --state all --json number,title \
    --jq ".[] | select(.title==\"$ISSUE_TITLE\") | .number" 2>/dev/null | head -1)
  if [ -n "$existing" ]; then
    log "Issue #$existing exists — commenting + syncing labels"
    gh issue comment "$existing" --body "$body" >/dev/null
    [ -n "$EXTRA_LABEL" ] && gh issue edit "$existing" --add-label "needs-articles" >/dev/null 2>&1
    return
  fi
  gh issue create --title "$ISSUE_TITLE" --body "$body" --label "$labels" 2>/dev/null \
    || gh issue create --title "$ISSUE_TITLE" --body "$body"
  log "Issue created."
}

# ---- 7. Write handoff files for Stage 2 (data only, no article) ----------
stage_handoff() {
  if [ ! -s "$HANDOFF_QUEUE_FILE" ]; then log "No catalog handoffs to write."; return; fi
  if [ "$SKIP_DRAFT" = "1" ]; then log "Handoff skipped (SKIP_DRAFT=1)."; return; fi
  if [ "$DRY_RUN" = "1" ]; then
    log "[DRY_RUN] Would write handoffs for: $(jq -r '.name' "$HANDOFF_QUEUE_FILE" | tr '\n' ' ')"
    return
  fi

  local token="${GH_PAT:-$GH_TOKEN}"
  mkdir -p "$NEWTOOLS_DIR"
  local count=0 item name change_type reason meta desc lang homepage readme folder slug target pushed_at created_at archived
  while IFS= read -r item; do
    [ -z "$item" ] && continue
    name=$(echo "$item" | jq -r '.name')
    change_type=$(echo "$item" | jq -r '.change_type')
    reason=$(echo "$item" | jq -r '.reason')
    [ -z "$name" ] && continue
    meta=$(curl -fsSL --max-time 20 -H "Authorization: Bearer $token" -H "Accept: application/vnd.github+json" "https://api.github.com/repos/$ORG/$name" 2>/dev/null)
    [ -z "$meta" ] && { warn "No metadata for $name; skipping."; continue; }
    desc=$(echo "$meta" | jq -r '.description // ""')
    lang=$(echo "$meta" | jq -r '.language // ""')
    homepage=$(echo "$meta" | jq -r '.homepage // ""')
    pushed_at=$(echo "$meta" | jq -r '.pushed_at // ""')
    created_at=$(echo "$meta" | jq -r '.created_at // ""')
    archived=$(echo "$meta" | jq -r '.archived // false')
    readme=$(curl -fsSL --max-time 20 -H "Authorization: Bearer $token" -H "Accept: application/vnd.github.raw+json" "https://api.github.com/repos/$ORG/$name/readme" 2>/dev/null | head -c 8000)
    if echo "$name $desc $(echo "$meta" | jq -r '(.topics // [])|join(" ")')" | grep -qiE 'mcp'; then folder="mcp-servers"; else folder="launchpad"; fi
    slug=$(slugify "$name")
    target="$NEWTOOLS_DIR/$slug.json"
    jq -n \
      --arg name "$name" --arg url "https://github.com/$ORG/$name" \
      --arg desc "$desc" --arg lang "$lang" --arg home "$homepage" \
      --arg folder "$folder" --arg slug "$slug" --arg detected "$DATE_UTC" \
      --arg change_type "$change_type" --arg reason "$reason" \
      --arg pushed_at "$pushed_at" --arg created_at "$created_at" --arg archived "$archived" \
      --argjson topics "$(echo "$meta" | jq '.topics // []')" \
      --arg readme "$readme" \
      '{name:$name, repo_url:$url, description:$desc, language:$lang, homepage:$home,
        target_folder:$folder, slug:$slug, detected:$detected, change_type:$change_type,
        reason:$reason, pushed_at:$pushed_at, created_at:$created_at,
        archived:($archived == "true"), topics:$topics, readme:$readme}' \
      > "$target"
    log "Handoff written: data/new-tools/$slug.json ($folder, $change_type)"
    count=$((count+1))
  done < "$HANDOFF_QUEUE_FILE"
  echo "$count" > /tmp/refresh-handoffs.txt
}

apply_handoffs() {
  if [ "$SKIP_DRAFT" = "1" ]; then log "Catalog article generation skipped (SKIP_DRAFT=1)."; return; fi
  if [ "$DRY_RUN" = "1" ]; then log "[DRY_RUN] Would apply generated handoffs into catalog articles."; return; fi
  if ! compgen -G "$NEWTOOLS_DIR/*.json" >/dev/null; then log "No handoff files to apply."; return; fi

  node "$ROOT/scripts/apply-tool-handoffs.js" || warn "Catalog article generation failed."
}

# ---- 8. Update state + commit handoffs STRAIGHT TO MAIN ------------------
update_and_commit() {
  if [ "$DRY_RUN" = "1" ]; then
    log "[DRY_RUN] Would update state, regenerate overview, and commit docs/state to main."
    return
  fi

  local cd ct cx cr rs
  cd=$(cat /tmp/refresh-cursor-date.txt 2>/dev/null || echo ""); ct=$(cat /tmp/refresh-claude-tag.txt 2>/dev/null || echo "")
  cx=$(cat /tmp/refresh-codex-version.txt 2>/dev/null || echo ""); cr="[]"
  rs="{}"
  [ -s /tmp/refresh-current-repos.txt ] && cr=$(jq -R . < /tmp/refresh-current-repos.txt | jq -s .)
  [ -s /tmp/refresh-current-repo-snapshots.json ] && rs=$(cat /tmp/refresh-current-repo-snapshots.json)
  jq --arg date "$DATE_UTC" --arg c "$cd" --arg cc "$ct" --arg cxv "$cx" --argjson repos "$cr" --argjson snapshots "$rs" '
    ._updated=$date
    | (if $c!="" then .cursor.latest_changelog_date=$c else . end)
    | (if $cc!="" then .anthropic.claude_code_cli_release=$cc else . end)
    | (if $cxv!="" then .openai.codex_cli_npm=$cxv else . end)
    | (if ($repos|length)>0 then .internal_tools.repos_seen=$repos else . end)
    | (if ($snapshots|length)>0 then .internal_tools.repo_snapshots=$snapshots else . end)
  ' "$STATE_FILE" > "$STATE_FILE.tmp" && mv "$STATE_FILE.tmp" "$STATE_FILE"

  if command -v node >/dev/null 2>&1; then
    node "$ROOT/scripts/update-catalog-overview.js" || warn "Catalog overview regeneration failed."
  else
    warn "Node is unavailable; skipping catalog overview regeneration."
  fi

  git config user.email "doc-refresh-bot@globalshopsolutions.dev"
  git config user.name  "doc-refresh-bot"
  git add "$STATE_FILE" "$NEWTOOLS_DIR" "$OVERVIEW_PATH" "$ROOT/docs/ai-tools-catalog" 2>/dev/null
  if git diff --cached --quiet; then log "Nothing to commit."; return; fi
  local n; n=$(cat /tmp/refresh-handoffs.txt 2>/dev/null || echo 0)
  local msg="Auto: refresh known-versions.json — $DATE_UTC [skip ci]"
  [ "${n:-0}" -gt 0 ] 2>/dev/null && msg="Auto: refresh $n catalog article(s) + state — $DATE_UTC"
  git commit -m "$msg" >/dev/null
  if git push origin HEAD:main 2>/dev/null; then log "Committed to main: $msg"
  else warn "Push to main failed (branch protection, or GH_PAT lacks write here)."; fi
}

# ---- Main ----------------------------------------------------------------
ISSUE_BODY="_Generated by \`scripts/check-updates.sh\` on $DATE_UTC. New, changed, and missing-coverage tools are converted into catalog articles automatically._"
ISSUE_BODY+="$(check_internal_tools)"
ISSUE_BODY+="$(check_cursor)"
ISSUE_BODY+="$(check_claude_code)"
ISSUE_BODY+="$(check_codex)"
ISSUE_BODY+="$(check_chatgpt)"
ISSUE_BODY+=$'\n\n---\n\n_The daily workflow ingests InternalTools metadata, creates or refreshes catalog articles, regenerates the overview, and pushes to main. The goal is complete LaunchPad / InternalTools coverage for the future GSS Catalog and RAG retrieval layer._'

build_and_post_issue "$ISSUE_BODY"
stage_handoff
apply_handoffs
update_and_commit
log "Done."
