#!/usr/bin/env bash
# Daily doc-refresh — STAGE 1 (the scout).
# Runs from .github/workflows/daily-doc-refresh.yml on a Mon-Fri morning cron.
#
# Stage 1 gathers; Stage 2 (a Cursor Automation) writes.
#
#   1. Lists repos in GlobalShopSolutions-InternalTools (activity + add/remove
#      vs. saved state).
#   2. Fetches Cursor / Claude Code / Codex / ChatGPT version info.
#   3. Opens (or comments on) a daily GitHub issue. If new tools were found,
#      the issue is labeled `needs-articles` (a hook a Cursor Automation can
#      trigger on).
#   4. For each BRAND-NEW tool, writes a HANDOFF FILE into this repo at
#      data/new-tools/<slug>.json — repo metadata + README pulled with GH_PAT.
#      Stage 2 reads these (needs access only to THIS repo) and writes the
#      polished catalog articles, then deletes the consumed handoffs.
#   5. Commits the handoffs + data/known-versions.json STRAIGHT TO MAIN
#      (with [skip ci] — these are data, not published pages).
#
# No LLM / API key here. Cursor org access NOT required — Stage 1 does the
# cross-org reading with GH_PAT and drops the raw material into this repo.
#
# Required env:
#   GH_TOKEN            - token for this repo (issues); push fallback
#   GITHUB_REPOSITORY   - owner/repo (provided by Actions)
#   GH_PAT              - PAT: READ on the org + WRITE on this repo
#                         (classic `repo` scope covers both)
# Optional env:
#   DRY_RUN=1           - print findings; don't write/commit/push
#   SKIP_DRAFT=1        - skip writing handoff files (still posts issue + state)

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
STATE_FILE="$ROOT/data/known-versions.json"
NEWTOOLS_DIR="$ROOT/data/new-tools"
DATE_UTC="$(date -u +%Y-%m-%d)"
ISSUE_TITLE="AI Tools deltas — $DATE_UTC"

DRY_RUN="${DRY_RUN:-0}"
SKIP_DRAFT="${SKIP_DRAFT:-0}"
ORG="GlobalShopSolutions-InternalTools"
EXTRA_LABEL=""

NEW_REPOS_FILE="/tmp/refresh-new-repos.txt"
: > "$NEW_REPOS_FILE"
echo 0 > /tmp/refresh-handoffs.txt

log()  { printf "==> %s\n" "$*" >&2; }
warn() { printf "WARN: %s\n" "$*" >&2; }
state_get() { jq -r "$1 // \"\"" "$STATE_FILE" 2>/dev/null || echo ""; }
fetch() { curl -fsSL --max-time 30 -A "ai_doc daily-refresh bot" "$1" 2>/dev/null || echo ""; }
section() { printf "\n## %s\n\n%s\n" "$1" "$2"; }
slugify() { echo "$1" | tr '[:upper:]' '[:lower:]' | sed -e 's/[^a-z0-9]\+/-/g' -e 's/^-//' -e 's/-$//'; }

# ---- 1. Internal tools GitHub org activity -------------------------------
check_internal_tools() {
  log "Checking $ORG repos…"
  local now_epoch; now_epoch="$(date -u +%s)"
  local token="${GH_PAT:-$GH_TOKEN}"
  local repos_json
  repos_json=$(curl -fsSL --max-time 30 -H "Authorization: Bearer $token" -H "Accept: application/vnd.github+json" \
    "https://api.github.com/orgs/$ORG/repos?per_page=100" 2>/dev/null \
    | jq '[.[] | {name, description, pushed_at, created_at, archived, html_url, topics}]') || repos_json="[]"

  if [ -z "$repos_json" ] || [ "$repos_json" = "[]" ] || [ "$repos_json" = "null" ]; then
    section "Internal tools (GitHub org)" "$(printf '_Could not fetch \`%s\` org repos._ Add a PAT with org read as `GH_PAT`.' "$ORG")"
    return
  fi

  local recent_pushes
  recent_pushes=$(echo "$repos_json" | jq -r --argjson now "$now_epoch" '
    .[] | select((.pushed_at | fromdateiso8601) > ($now - 86400) and (.archived | not)) |
    "- [\(.name)](\(.html_url)) — \((.description // "_no description_") | tostring) (pushed \(.pushed_at | sub("T.*$"; "")))"' | head -25)

  local current_names previous_names added removed
  current_names=$(echo "$repos_json" | jq -r '[.[] | .name] | sort | .[]')
  previous_names=$(jq -r '.internal_tools.repos_seen[]?' "$STATE_FILE" 2>/dev/null | sort)
  added=$(comm -23 <(echo "$current_names") <(echo "$previous_names") | head -20)
  removed=$(comm -13 <(echo "$current_names") <(echo "$previous_names") | head -20)
  [ -n "$added" ] && echo "$added" | sed '/^$/d' | head -8 > "$NEW_REPOS_FILE"
  [ -s "$NEW_REPOS_FILE" ] && EXTRA_LABEL=",needs-articles"

  local body=""
  [ -n "$recent_pushes" ] && body+="### Pushed in last 24h\n\n$recent_pushes\n"
  if [ -n "$added" ]; then
    body+="\n### Added — queued for article drafting\n\n"; body+=$(echo "$added" | sed 's/^/- /'); body+="\n"
  fi
  if [ -n "$removed" ]; then
    body+="\n### Removed since last refresh\n\n"; body+=$(echo "$removed" | sed 's/^/- /'); body+="\n"
  fi
  [ -z "$body" ] && body="_No internal-tools repo changes in the last 24h._"
  section "Internal tools (GitHub org)" "$(printf "%b" "$body")"
  echo "$current_names" > /tmp/refresh-current-repos.txt
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
  if [ ! -s "$NEW_REPOS_FILE" ]; then log "No new tools to hand off."; return; fi
  if [ "$SKIP_DRAFT" = "1" ]; then log "Handoff skipped (SKIP_DRAFT=1)."; return; fi
  if [ "$DRY_RUN" = "1" ]; then log "[DRY_RUN] Would write handoffs for: $(tr '\n' ' ' < "$NEW_REPOS_FILE")"; return; fi

  local token="${GH_PAT:-$GH_TOKEN}"
  mkdir -p "$NEWTOOLS_DIR"
  local count=0 name meta desc lang homepage readme folder slug target
  while IFS= read -r name; do
    [ -z "$name" ] && continue
    meta=$(curl -fsSL --max-time 20 -H "Authorization: Bearer $token" -H "Accept: application/vnd.github+json" "https://api.github.com/repos/$ORG/$name" 2>/dev/null)
    [ -z "$meta" ] && { warn "No metadata for $name; skipping."; continue; }
    desc=$(echo "$meta" | jq -r '.description // ""')
    lang=$(echo "$meta" | jq -r '.language // ""')
    homepage=$(echo "$meta" | jq -r '.homepage // ""')
    readme=$(curl -fsSL --max-time 20 -H "Authorization: Bearer $token" -H "Accept: application/vnd.github.raw+json" "https://api.github.com/repos/$ORG/$name/readme" 2>/dev/null | head -c 8000)
    if echo "$name $desc $(echo "$meta" | jq -r '(.topics // [])|join(" ")')" | grep -qiE 'mcp'; then folder="mcp-servers"; else folder="launchpad"; fi
    slug=$(slugify "$name")
    target="$NEWTOOLS_DIR/$slug.json"
    jq -n \
      --arg name "$name" --arg url "https://github.com/$ORG/$name" \
      --arg desc "$desc" --arg lang "$lang" --arg home "$homepage" \
      --arg folder "$folder" --arg slug "$slug" --arg detected "$DATE_UTC" \
      --argjson topics "$(echo "$meta" | jq '.topics // []')" \
      --arg readme "$readme" \
      '{name:$name, repo_url:$url, description:$desc, language:$lang, homepage:$home,
        target_folder:$folder, slug:$slug, detected:$detected, topics:$topics, readme:$readme}' \
      > "$target"
    log "Handoff written: data/new-tools/$slug.json ($folder)"
    count=$((count+1))
  done < "$NEW_REPOS_FILE"
  echo "$count" > /tmp/refresh-handoffs.txt
}

# ---- 8. Update state + commit handoffs STRAIGHT TO MAIN ------------------
update_and_commit() {
  local cd ct cx cr
  cd=$(cat /tmp/refresh-cursor-date.txt 2>/dev/null || echo ""); ct=$(cat /tmp/refresh-claude-tag.txt 2>/dev/null || echo "")
  cx=$(cat /tmp/refresh-codex-version.txt 2>/dev/null || echo ""); cr="[]"
  [ -s /tmp/refresh-current-repos.txt ] && cr=$(jq -R . < /tmp/refresh-current-repos.txt | jq -s .)
  jq --arg date "$DATE_UTC" --arg c "$cd" --arg cc "$ct" --arg cxv "$cx" --argjson repos "$cr" '
    ._updated=$date
    | (if $c!="" then .cursor.latest_changelog_date=$c else . end)
    | (if $cc!="" then .anthropic.claude_code_cli_release=$cc else . end)
    | (if $cxv!="" then .openai.codex_cli_npm=$cxv else . end)
    | (if ($repos|length)>0 then .internal_tools.repos_seen=$repos else . end)
  ' "$STATE_FILE" > "$STATE_FILE.tmp" && mv "$STATE_FILE.tmp" "$STATE_FILE"

  if [ "$DRY_RUN" = "1" ]; then log "[DRY_RUN] Would commit state (+ any handoffs) to main."; return; fi
  git config user.email "doc-refresh-bot@globalshopsolutions.dev"
  git config user.name  "doc-refresh-bot"
  git add "$STATE_FILE" "$NEWTOOLS_DIR" 2>/dev/null
  if git diff --cached --quiet; then log "Nothing to commit."; return; fi
  local n; n=$(cat /tmp/refresh-handoffs.txt 2>/dev/null || echo 0)
  local msg="Auto: refresh known-versions.json — $DATE_UTC [skip ci]"
  [ "${n:-0}" -gt 0 ] 2>/dev/null && msg="Auto: queue $n new tool(s) for drafting + state — $DATE_UTC [skip ci]"
  git commit -m "$msg" >/dev/null
  if git push origin HEAD:main 2>/dev/null; then log "Committed to main: $msg"
  else warn "Push to main failed (branch protection, or GH_PAT lacks write here)."; fi
}

# ---- Main ----------------------------------------------------------------
ISSUE_BODY="_Generated by \`scripts/check-updates.sh\` on $DATE_UTC. New tools are queued in data/new-tools/ for the Stage-2 drafting agent._"
ISSUE_BODY+="$(check_internal_tools)"
ISSUE_BODY+="$(check_cursor)"
ISSUE_BODY+="$(check_claude_code)"
ISSUE_BODY+="$(check_codex)"
ISSUE_BODY+="$(check_chatgpt)"
ISSUE_BODY+=$'\n\n---\n\n_Stage 1 of 2 — Stage 2 (Cursor Automation) turns data/new-tools/ into catalog articles._'

build_and_post_issue "$ISSUE_BODY"
stage_handoff
update_and_commit
log "Done."
