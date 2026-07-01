#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const CATALOG_DIR = path.join(ROOT, 'docs', 'ai-tools-catalog');
const OVERVIEW_PATH = path.join(CATALOG_DIR, 'overview.md');
const KNOWN_VERSIONS_PATH = path.join(ROOT, 'data', 'known-versions.json');
const TODAY = new Date().toISOString().slice(0, 10);

const CATEGORY_CONFIG = [
  {
    heading: 'LaunchPad apps',
    dirName: 'launchpad',
    intro:
      'Internal tools built at GSS and usually hosted on LaunchPad with Office 365 / SAML login.',
  },
  {
    heading: 'MCP servers',
    dirName: 'mcp-servers',
    intro:
      'GSS-specific MCP servers that any MCP-compatible AI agent can use, including Cursor, Claude Code, Codex, and other clients.',
  },
  {
    heading: 'AI agent concepts',
    dirName: 'concepts',
    intro: 'Shared vocabulary for configuring, extending, and operating AI coding agents at GSS.',
  },
];

function toPosix(filePath) {
  return filePath.split(path.sep).join('/');
}

function readFileIfExists(filePath) {
  return fs.existsSync(filePath) ? fs.readFileSync(filePath, 'utf8') : '';
}

function parseFrontMatter(markdown) {
  if (!markdown.startsWith('---\n')) {
    return {};
  }

  const end = markdown.indexOf('\n---', 4);
  if (end === -1) {
    return {};
  }

  const frontMatter = markdown.slice(4, end).split(/\r?\n/);
  const data = {};

  for (const line of frontMatter) {
    const match = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
    if (!match) {
      continue;
    }

    const [, key, rawValue] = match;
    data[key] = rawValue.replace(/^['"]|['"]$/g, '').trim();
  }

  return data;
}

function extractTldr(markdown) {
  const line = markdown.split(/\r?\n/).find(candidate => candidate.includes('**TL;DR**'));

  if (!line) {
    return '';
  }

  return line
    .replace(/^>\s*/, '')
    .replace(/\*\*TL;DR\*\*\s*[-—]\s*/, '')
    .trim();
}

function excerpt(markdown) {
  const withoutFrontMatter = markdown.replace(/^---[\s\S]*?\n---\n/, '');
  const paragraph = withoutFrontMatter
    .split(/\n{2,}/)
    .map(block => block.trim())
    .find(block => block && !block.startsWith('#') && !block.startsWith('>'));

  return paragraph ? paragraph.replace(/\s+/g, ' ') : '';
}

function normalizeDescription(description) {
  const value = (description || '').replace(/\\"/g, '"').replace(/\s+/g, ' ').trim();

  if (value.endsWith('...')) {
    return '';
  }

  return value;
}

function summarize(description) {
  const value = normalizeDescription(description);

  if (value.length <= 180) {
    return value;
  }

  const sentenceEnd = value.slice(0, 220).search(/[.!?]\s/);
  if (sentenceEnd > 80) {
    return value.slice(0, sentenceEnd + 1);
  }

  return `${value.slice(0, 177).replace(/\s+\S*$/, '')}...`;
}

function getTitleFromPath(filePath) {
  return path.basename(filePath, '.md');
}

function getDocInfo(filePath) {
  const markdown = fs.readFileSync(filePath, 'utf8');
  const frontMatter = parseFrontMatter(markdown);
  const title = frontMatter.title || getTitleFromPath(filePath);
  const description =
    summarize(frontMatter.description) ||
    summarize(extractTldr(markdown)) ||
    summarize(excerpt(markdown));
  const link = toPosix(path.relative(path.dirname(OVERVIEW_PATH), filePath));

  return {
    filePath,
    title,
    description,
    link,
  };
}

function listMarkdownFiles(dirPath) {
  if (!fs.existsSync(dirPath)) {
    return [];
  }

  return fs
    .readdirSync(dirPath)
    .filter(fileName => fileName.endsWith('.md'))
    .map(fileName => path.join(dirPath, fileName))
    .sort((a, b) =>
      getTitleFromPath(a).localeCompare(getTitleFromPath(b), undefined, {
        sensitivity: 'base',
      })
    );
}

function bulletList(docs) {
  if (docs.length === 0) {
    return '_No entries yet._';
  }

  return docs
    .map(doc => {
      const summary = doc.description ? ` - ${doc.description}` : '';
      return `- [${doc.title}](${doc.link})${summary}`;
    })
    .join('\n');
}

function readKnownRepos() {
  const raw = readFileIfExists(KNOWN_VERSIONS_PATH);
  if (!raw) {
    return [];
  }

  try {
    const parsed = JSON.parse(raw);
    return parsed.internal_tools?.repos_seen || [];
  } catch (error) {
    console.warn(`Could not parse ${KNOWN_VERSIONS_PATH}: ${error.message}`);
    return [];
  }
}

function coveredInternalToolsRepos() {
  const covered = new Set();
  const repoPattern = /github\.com\/GlobalShopSolutions-InternalTools\/([^\)\]\s]+)/g;
  const appPattern = /launchpad\.globalshopsolutions\.dev\/apps\/([^\)\]\s]+)/g;

  for (const config of CATEGORY_CONFIG) {
    for (const filePath of listMarkdownFiles(path.join(CATALOG_DIR, config.dirName))) {
      const stem = getTitleFromPath(filePath);
      covered.add(stem);
      covered.add(stem.toLowerCase());

      const markdown = fs.readFileSync(filePath, 'utf8');
      for (const pattern of [repoPattern, appPattern]) {
        for (const match of markdown.matchAll(pattern)) {
          const repo = match[1].replace(/[\/.,]+$/, '');
          covered.add(repo);
          covered.add(repo.toLowerCase());
        }
      }
    }
  }

  return covered;
}

function missingCoverageRepos(knownRepos) {
  const covered = coveredInternalToolsRepos();
  return knownRepos.filter(repo => !covered.has(repo) && !covered.has(repo.toLowerCase()));
}

function externalToolsSection() {
  return [
    '- [Cursor](../cursor.md) - AI-first code editor used for in-IDE pair programming.',
    '- [ChatGPT](../chatgpt.md) - OpenAI chat assistant used for analysis, drafting, and problem solving.',
    '- [Codex](../codex.md) - OpenAI coding agent and CLI workflow.',
    '- [Claude Code](../claude-code.md) - Anthropic coding agent and CLI workflow.',
    '- [Augment AI](../augment-ai.md) - AI coding assistant used by GSS teams.',
  ].join('\n');
}

function backlogSection(missingRepos) {
  if (missingRepos.length === 0) {
    return '_No known InternalTools repos are missing catalog coverage._';
  }

  return missingRepos
    .map(repo => `- [${repo}](https://github.com/GlobalShopSolutions-InternalTools/${repo})`)
    .join('\n');
}

function buildOverview() {
  const categoryDocs = CATEGORY_CONFIG.map(config => ({
    ...config,
    docs: listMarkdownFiles(path.join(CATALOG_DIR, config.dirName)).map(getDocInfo),
  }));

  const knownRepos = readKnownRepos();
  const missingRepos = missingCoverageRepos(knownRepos);
  const totalArticles = categoryDocs.reduce((total, category) => total + category.docs.length, 0);

  const sections = categoryDocs
    .map(
      category => `## ${category.heading}

${category.intro}

${bulletList(category.docs)}`
    )
    .join('\n\n---\n\n');

  return `---
title: 'AI Tools at GSS'
description: 'Directory of GSS AI tools, LaunchPad apps, MCP servers, agent concepts, and InternalTools catalog coverage.'
sidebar_position: 1
last_updated: ${TODAY}
tags: [ai-tools, gss-internal, catalog, index]
---

# AI Tools at GSS

This is the front door for the GSS AI tooling catalog. It indexes LaunchPad apps, MCP servers, AI-agent concepts, and the current InternalTools coverage backlog that feeds the future GSS Catalog and RAG retrieval layer.

The detailed articles in this section explain what each tool is, who it is for, how to access it at GSS, and how it fits into the AI platform. The sidebar is generated from the catalog folders, and this overview is generated from the same docs plus \`data/known-versions.json\`.

## Catalog status

- Published catalog articles: **${totalArticles}**
- LaunchPad app articles: **${categoryDocs.find(category => category.dirName === 'launchpad').docs.length}**
- MCP server articles: **${categoryDocs.find(category => category.dirName === 'mcp-servers').docs.length}**
- AI agent concept articles: **${categoryDocs.find(category => category.dirName === 'concepts').docs.length}**
- InternalTools repos tracked: **${knownRepos.length}**
- InternalTools repos still missing catalog coverage: **${missingRepos.length}**
- Catalog last regenerated: **${TODAY}**

## How this stays current

The daily catalog scout scans \`GlobalShopSolutions-InternalTools\`, compares the result with \`data/known-versions.json\`, and queues handoff files in \`data/new-tools/\` for:

1. brand-new repos,
2. repos whose metadata or \`pushed_at\` changed since the last scan, and
3. repos that are present in InternalTools but do not yet have a catalog page, repo link, or LaunchPad app reference.

Stage 2 consumes those handoffs to create or refresh articles. This page is regenerated by \`scripts/update-catalog-overview.js\` so the published overview shows both documented tools and the remaining coverage backlog.

---

${sections}

---

## External AI services

Third-party AI tools the company uses or documents.

${externalToolsSection()}

---

## InternalTools coverage backlog

These repos are known in \`data/known-versions.json\` but are not yet matched to a catalog article, InternalTools repo link, or LaunchPad app link. The daily automation queues them for article drafting or triage so the catalog can move toward complete coverage.

${backlogSection(missingRepos)}

---

## Missing something?

If a tool is not listed and you think it should be, contact the catalog owner with the tool name, link if any, and a one-line description.

- **Catalog owner:** [tjones@gssmail.com](mailto:tjones@gssmail.com)
`;
}

fs.writeFileSync(OVERVIEW_PATH, buildOverview(), 'utf8');
console.log(`Updated ${path.relative(ROOT, OVERVIEW_PATH)}`);
