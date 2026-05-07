#!/usr/bin/env bash
# Daily doc-refresh check. Runs from the .github/workflows/daily-doc-refresh.yml
# workflow on a Mon-Fri morning cron.
#
# What it does:
#   1. Lists GitHub repos in GlobalShopSolutions-InternalTools updated in the
#      last 24h (and any new repos compared to the saved state).
#   2. Fetches the Cursor changelog and notes the most-recent dated entry.
#   3. Fetches Anthropic Claude Code releases from GitHub.
#   4. Fetches OpenAI / Codex updates (Codex CLI npm, GPT release notes).
#   5. Compares findings against data/known-versions.json.
#   6. Opens a GitHub issue titled "AI Tools deltas — YYYY-MM-DD" with sections
#      per source (always — even if nothing changed, you get an "all quiet"
#      heartbeat).
#   7. If a clean version bump is detected (e.g., Composer 2 → Composer 3,
#      Opus 4.7 → Opus 4.8), opens a PR that updates the state file + applies
#      a sed replace across docs. Auto-PRs are flagged "needs review."
#
# All sources are best-effort: a failure in one source never kills the run.
#
# Required env:
#   GH_TOKEN              — GitHub token with repo + issues + PR write
#   GITHUB_REPOSITORY     — owner/repo (provided by Actions)
#
# Optional env:
#   DRY_RUN=1             — print what would happen without opening issue/PR
#   SKIP_PR=1             — skip the auto-PR step entirely (issue only)

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
STATE_FILE="$ROOT/data/known-versions.json"
DATE_UTC="$(date -u +%Y-%m-%d)"
ISSUE_TITLE="AI Tools deltas — $DATE_UTC"

DRY_RUN="${DRY_RUN:-0}"
SKIP_PR="${SKIP_PR:-0}"

ORG="GlobalShopSolutions-InternalTools"

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

log()  { printf "==> %s\n" "$*" >&2; }
warn() { printf "WARN: %s\n" "$*" >&2; }

# Read a JSON path from the state file (jq syntax). Empty if missing.
state_get() {
  jq -r "$1 // \"\"" "$STATE_FILE" 2>/dev/null || echo ""
}

# Fetch a URL with a 30-second timeout. Empty on failure.
fetch() {
  curl -fsSL --max-time 30 -A "ai_doc daily-refresh bot" "$1" 2>/dev/null || echo ""
}

# Build a section in the issue body. Args: heading, content
section() {
  local heading="$1"
  local content="$2"
  printf "\n## %s\n\n%s\n" "$heading" "$content"
}

# ----------------------------------------------------------------------------
# 1. Internal tools GitHub org activity
# ----------------------------------------------------------------------------
check_internal_tools() {
  log "Checking $ORG repos…"
  local now_epoch
  now_epoch="$(date -u +%s)"

  # The default GITHUB_TOKEN can't read other orgs. Prefer GH_PAT if set —
  # it should be a fine-grained or classic PAT with `read:org` and `repo`
  # scopes for GlobalShopSolutions-InternalTools.
  local token="${GH_PAT:-$GH_TOKEN}"

  # Pull all repos (paginated). Use curl directly so we can choose which token.
  local repos_json
  repos_json=$(curl -fsSL --max-time 30 \
    -H "Authorization: Bearer $token" \
    -H "Accept: application/vnd.github+json" \
    "https://api.github.com/orgs/$ORG/repos?per_page=100" 2>/dev/null \
    | jq '[.[] | {name, description, pushed_at, created_at, archived, html_url, topics}]') || repos_json="[]"

  if [ -z "$repos_json" ] || [ "$repos_json" = "[]" ] || [ "$repos_json" = "null" ]; then
    section "Internal tools (GitHub org)" "$(printf '_Could not fetch \`%s\` org repos._\n\nThe default `GITHUB_TOKEN` has no cross-org read access. To enable: create a fine-grained PAT with **read access to GlobalShopSolutions-InternalTools repos** and add it as a repo secret named `GH_PAT`.' "$ORG")"
    return
  fi

  # Repos pushed in the last 24h
  local recent_pushes
  recent_pushes=$(echo "$repos_json" | jq -r --argjson now "$now_epoch" '
    .[] | select((.pushed_at | fromdateiso8601) > ($now - 86400) and (.archived | not)) |
    "- [\(.name)](\(.html_url)) — \((.description // "_no description_") | tostring) (pushed \(.pushed_at | sub("T.*$"; "")))"
  ' | head -25)

  # Repos created in the last 7 days (catch new tools)
  local new_repos
  new_repos=$(echo "$repos_json" | jq -r --argjson now "$now_epoch" '
    .[] | select((.created_at | fromdateiso8601) > ($now - 604800)) |
    "- **NEW:** [\(.name)](\(.html_url)) — \((.description // "_no description_") | tostring) (created \(.created_at | sub("T.*$"; "")))"
  ' | head -25)

  # Diff against state — repos added/removed since last run
  local current_names
  current_names=$(echo "$repos_json" | jq -r '[.[] | .name] | sort | .[]')
  local previous_names
  previous_names=$(jq -r '.internal_tools.repos_seen[]?' "$STATE_FILE" 2>/dev/null | sort)

  local added removed
  added=$(comm -23 <(echo "$current_names") <(echo "$previous_names") | head -20)
  removed=$(comm -13 <(echo "$current_names") <(echo "$previous_names") | head -20)

  local body=""
  if [ -n "$new_repos" ]; then
    body+="### New repos (last 7 days)\n\n$new_repos\n"
  fi
  if [ -n "$recent_pushes" ]; then
    body+="\n### Pushed in last 24h\n\n$recent_pushes\n"
  fi
  if [ -n "$added" ]; then
    body+="\n### Added since last refresh\n\n"
    body+=$(echo "$added" | sed 's/^/- /')
    body+="\n"
  fi
  if [ -n "$removed" ]; then
    body+="\n### Removed since last refresh\n\n"
    body+=$(echo "$removed" | sed 's/^/- /')
    body+="\n"
  fi
  if [ -z "$body" ]; then
    body="_No internal-tools repo changes in the last 24h._"
  fi
  section "Internal tools (GitHub org)" "$(printf "%b" "$body")"

  # Stash current names for state-update step
  echo "$current_names" > /tmp/refresh-current-repos.txt
}

# ----------------------------------------------------------------------------
# 2. Cursor changelog
# ----------------------------------------------------------------------------
check_cursor() {
  log "Checking Cursor changelog…"
  local html
  html=$(fetch "https://cursor.com/changelog")
  if [ -z "$html" ]; then
    section "Cursor" "_Could not fetch Cursor changelog._"
    return
  fi
  # Pull the first dated heading-ish block. Cursor's changelog uses h2/h3 dates.
  local latest_date
  latest_date=$(echo "$html" | grep -oE '20[0-9]{2}-[0-9]{2}-[0-9]{2}' | head -1)
  local known
  known=$(state_get '.cursor.latest_changelog_date')

  local body
  if [ -z "$latest_date" ]; then
    body="_Couldn't parse a date from the page. Check the page manually: https://cursor.com/changelog_"
  elif [ "$latest_date" = "$known" ]; then
    body="Latest changelog entry: **$latest_date** (no change since last refresh)"
  else
    body="Latest changelog entry: **$latest_date** (was: \`$known\`) — review at https://cursor.com/changelog"
  fi
  echo "$latest_date" > /tmp/refresh-cursor-date.txt
  section "Cursor" "$body"
}

# ----------------------------------------------------------------------------
# 3. Anthropic Claude Code (GitHub releases)
# ----------------------------------------------------------------------------
check_claude_code() {
  log "Checking Claude Code releases…"
  local release_json
  release_json=$(gh api repos/anthropics/claude-code/releases/latest 2>/dev/null) || release_json=""
  if [ -z "$release_json" ]; then
    section "Claude Code" "_Could not fetch latest release from anthropics/claude-code._"
    return
  fi
  local tag name published
  tag=$(echo "$release_json" | jq -r '.tag_name // ""')
  name=$(echo "$release_json" | jq -r '.name // ""')
  published=$(echo "$release_json" | jq -r '.published_at // "" | sub("T.*$"; "")')
  local known
  known=$(state_get '.anthropic.claude_code_cli_release')

  local body="Latest CLI release: **$tag** ($name) published $published"
  if [ -n "$known" ] && [ "$tag" != "$known" ]; then
    body+="\n\n> ⚠ Version bump from \`$known\`. Review release notes: https://github.com/anthropics/claude-code/releases/tag/$tag"
  fi

  # Also try Anthropic models page (best-effort)
  local models_page
  models_page=$(fetch "https://docs.claude.com/en/docs/about-claude/models")
  if [ -n "$models_page" ]; then
    local mentioned_opus
    mentioned_opus=$(echo "$models_page" | grep -oE 'Claude Opus [0-9.]+' | sort -u | head -3 | tr '\n' ',' | sed 's/,$//')
    if [ -n "$mentioned_opus" ]; then
      body+="\n\n_Models page mentions:_ $mentioned_opus"
    fi
  fi

  echo "$tag" > /tmp/refresh-claude-tag.txt
  section "Claude Code" "$(printf "%b" "$body")"
}

# ----------------------------------------------------------------------------
# 4. Codex / OpenAI
# ----------------------------------------------------------------------------
check_codex() {
  log "Checking Codex CLI on npm…"
  local npm_version
  npm_version=$(curl -fsSL --max-time 20 "https://registry.npmjs.org/@openai/codex/latest" 2>/dev/null \
    | jq -r '.version // ""') || npm_version=""

  local known
  known=$(state_get '.openai.codex_cli_npm')

  local body
  if [ -z "$npm_version" ]; then
    body="_Couldn't fetch @openai/codex from npm._"
  else
    body="Latest \`@openai/codex\` on npm: **$npm_version**"
    if [ -n "$known" ] && [ "$npm_version" != "$known" ]; then
      body+="\n\n> ⚠ Version bump from \`$known\`. Review: https://www.npmjs.com/package/@openai/codex"
    fi
  fi

  echo "$npm_version" > /tmp/refresh-codex-version.txt
  section "Codex / OpenAI" "$body"
}

# ----------------------------------------------------------------------------
# 5. ChatGPT release notes (best-effort)
# ----------------------------------------------------------------------------
check_chatgpt() {
  log "Checking ChatGPT release notes…"
  # OpenAI's help-center page is JS-rendered and Cloudflare-shielded; curl
  # can't reach it reliably. Fall back to the OpenAI changelog page which
  # is static enough to grep. If both fail, just print the manual link.
  local html
  html=$(fetch "https://platform.openai.com/docs/changelog")
  if [ -z "$html" ]; then
    html=$(fetch "https://openai.com/news/")
  fi
  local latest=""
  if [ -n "$html" ]; then
    latest=$(echo "$html" | grep -oE '(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{1,2},? 20[0-9]{2}' | head -1)
  fi
  local body
  if [ -n "$latest" ]; then
    body="Most-recent dated mention found: **$latest**.\n\nManual links:\n- https://platform.openai.com/docs/changelog\n- https://help.openai.com/en/articles/6825453-chatgpt-release-notes\n- https://openai.com/news/"
  else
    body="_Couldn't auto-fetch a date (OpenAI's help center is JS-rendered)._\n\nManual links:\n- https://platform.openai.com/docs/changelog\n- https://help.openai.com/en/articles/6825453-chatgpt-release-notes\n- https://openai.com/news/"
  fi
  section "ChatGPT / OpenAI" "$(printf '%b' "$body")"
}

# ----------------------------------------------------------------------------
# 6. Issue creation
# ----------------------------------------------------------------------------
build_and_post_issue() {
  local body="$1"
  if [ "$DRY_RUN" = "1" ]; then
    log "[DRY_RUN] Would create issue: $ISSUE_TITLE"
    printf "%s\n" "$body"
    return
  fi
  # Skip if today's issue already exists
  local existing
  existing=$(gh issue list --search "$ISSUE_TITLE in:title" --state all --json number,title \
    --jq ".[] | select(.title==\"$ISSUE_TITLE\") | .number" 2>/dev/null | head -1)
  if [ -n "$existing" ]; then
    log "Issue for $DATE_UTC already exists (#$existing) — appending comment instead"
    gh issue comment "$existing" --body "$body" >/dev/null
    return
  fi
  gh issue create --title "$ISSUE_TITLE" --body "$body" --label "automated,doc-refresh" 2>/dev/null \
    || gh issue create --title "$ISSUE_TITLE" --body "$body"  # fall back if labels missing
  log "Issue created."
}

# ----------------------------------------------------------------------------
# 7. State file update + auto-PR for clean version bumps
# ----------------------------------------------------------------------------
maybe_open_version_bump_pr() {
  if [ "$SKIP_PR" = "1" ] || [ "$DRY_RUN" = "1" ]; then return; fi

  local cursor_date claude_tag codex_version
  cursor_date=$(cat /tmp/refresh-cursor-date.txt 2>/dev/null || echo "")
  claude_tag=$(cat /tmp/refresh-claude-tag.txt 2>/dev/null || echo "")
  codex_version=$(cat /tmp/refresh-codex-version.txt 2>/dev/null || echo "")

  local prev_cursor prev_claude prev_codex
  prev_cursor=$(state_get '.cursor.latest_changelog_date')
  prev_claude=$(state_get '.anthropic.claude_code_cli_release')
  prev_codex=$(state_get '.openai.codex_cli_npm')

  local changed=()
  [ -n "$cursor_date"   ] && [ "$cursor_date"   != "$prev_cursor"   ] && changed+=("cursor:$prev_cursor → $cursor_date")
  [ -n "$claude_tag"    ] && [ "$claude_tag"    != "$prev_claude"   ] && changed+=("claude-code:$prev_claude → $claude_tag")
  [ -n "$codex_version" ] && [ "$codex_version" != "$prev_codex"    ] && changed+=("codex:$prev_codex → $codex_version")

  # Always update state file (so the issue's "no change" reads stay accurate next run)
  local current_repos="[]"
  if [ -s /tmp/refresh-current-repos.txt ]; then
    current_repos=$(jq -R . < /tmp/refresh-current-repos.txt | jq -s .)
  fi
  jq --arg date "$DATE_UTC" \
     --arg c   "$cursor_date" \
     --arg cc  "$claude_tag" \
     --arg cx  "$codex_version" \
     --argjson repos "$current_repos" '
    ._updated = $date
    | (if $c  != "" then .cursor.latest_changelog_date = $c   else . end)
    | (if $cc != "" then .anthropic.claude_code_cli_release = $cc else . end)
    | (if $cx != "" then .openai.codex_cli_npm = $cx else . end)
    | (if ($repos | length) > 0 then .internal_tools.repos_seen = $repos else . end)
  ' "$STATE_FILE" > "$STATE_FILE.tmp" && mv "$STATE_FILE.tmp" "$STATE_FILE"

  if [ ${#changed[@]} -eq 0 ]; then
    log "No version bumps to PR."
    return
  fi

  log "Detected version changes: ${changed[*]}"

  # Open a PR that updates the state file. Doc-content sed replaces are
  # intentionally NOT done here — model versions in prose are noisy to
  # auto-replace and the issue captures the change for human review.
  local branch="auto-version-bump-$DATE_UTC"
  git config user.email "doc-refresh-bot@globalshopsolutions.dev"
  git config user.name  "doc-refresh-bot"
  git checkout -b "$branch"
  git add "$STATE_FILE"
  git commit -m "Auto: update known-versions.json — $DATE_UTC

Detected:
$(printf '  - %s\n' "${changed[@]}")

This is the bot updating its memory of last-seen versions. Doc prose
changes (model names, version numbers in articles) require human
review — see the issue for the same date for context."
  git push origin "$branch"
  gh pr create \
    --title "Auto: known-versions bump — $DATE_UTC" \
    --body "Bot detected version changes. See [issue for $DATE_UTC](../issues?q=is%3Aissue+%22$ISSUE_TITLE%22) for context. Merging this PR only updates the state file — doc prose changes are still on you to review and apply." \
    --label "automated,doc-refresh" 2>/dev/null \
    || gh pr create --title "Auto: known-versions bump — $DATE_UTC" --body "Bot detected: ${changed[*]}"
}

# ----------------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------------
ISSUE_BODY="_Generated by \`scripts/check-updates.sh\` on $DATE_UTC. Triage and close when handled. Doc updates are still on you — this bot only surfaces what's worth looking at._"
ISSUE_BODY+="$(check_internal_tools)"
ISSUE_BODY+="$(check_cursor)"
ISSUE_BODY+="$(check_claude_code)"
ISSUE_BODY+="$(check_codex)"
ISSUE_BODY+="$(check_chatgpt)"
ISSUE_BODY+=$'\n\n---\n\n_Repo: [scripts/check-updates.sh](../blob/main/scripts/check-updates.sh) · State: [data/known-versions.json](../blob/main/data/known-versions.json)_'

build_and_post_issue "$ISSUE_BODY"
maybe_open_version_bump_pr

log "Done."
