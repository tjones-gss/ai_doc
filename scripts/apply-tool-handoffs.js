#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const HANDOFF_DIR = path.join(ROOT, 'data', 'new-tools');
const CATALOG_DIR = path.join(ROOT, 'docs', 'ai-tools-catalog');
const TODAY = new Date().toISOString().slice(0, 10);

function slugify(value) {
  return value
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '');
}

function readJson(filePath) {
  return JSON.parse(fs.readFileSync(filePath, 'utf8'));
}

function listMarkdownFiles(dirPath) {
  if (!fs.existsSync(dirPath)) {
    return [];
  }

  return fs
    .readdirSync(dirPath)
    .filter(fileName => fileName.endsWith('.md'))
    .map(fileName => path.join(dirPath, fileName));
}

function parseFrontMatter(markdown) {
  if (!markdown.startsWith('---\n')) {
    return {};
  }

  const end = markdown.indexOf('\n---', 4);
  if (end === -1) {
    return {};
  }

  return Object.fromEntries(
    markdown
      .slice(4, end)
      .split(/\r?\n/)
      .map(line => line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/))
      .filter(Boolean)
      .map(match => [match[1], match[2].replace(/^['"]|['"]$/g, '').trim()])
  );
}

function maxSidebarPosition(dirPath) {
  return listMarkdownFiles(dirPath).reduce((max, filePath) => {
    const markdown = fs.readFileSync(filePath, 'utf8');
    const frontMatter = parseFrontMatter(markdown);
    const position = Number(frontMatter.sidebar_position);
    return Number.isFinite(position) ? Math.max(max, position) : max;
  }, 0);
}

function existingSidebarPosition(filePath, dirPath) {
  if (!fs.existsSync(filePath)) {
    return maxSidebarPosition(dirPath) + 1;
  }

  const frontMatter = parseFrontMatter(fs.readFileSync(filePath, 'utf8'));
  const position = Number(frontMatter.sidebar_position);
  return Number.isFinite(position) ? position : maxSidebarPosition(dirPath) + 1;
}

function normalizeDescription(description, fallback) {
  const value = (description || '').replace(/\s+/g, ' ').trim();
  return value || fallback;
}

function readmeTitle(readme) {
  const match = readme.match(/^#\s+(.+)$/m);
  return match ? match[1].trim() : '';
}

function firstParagraph(readme) {
  const withoutTitle = readme.replace(/^#\s+.+$/m, '').trim();
  const paragraph = withoutTitle
    .split(/\n{2,}/)
    .map(block => block.trim())
    .find(block => block && !block.startsWith('#') && !block.startsWith('```'));

  if (!paragraph) {
    return '';
  }

  return paragraph
    .replace(/\*\*/g, '')
    .replace(/`([^`]+)`/g, '$1')
    .replace(/\[([^\]]+)\]\([^\)]+\)/g, '$1')
    .replace(/\s+/g, ' ')
    .trim();
}

function extractSection(readme, headingPattern) {
  const lines = readme.split(/\r?\n/);
  const start = lines.findIndex(line => headingPattern.test(line));
  if (start === -1) {
    return '';
  }

  const body = [];
  for (let index = start + 1; index < lines.length; index += 1) {
    if (/^#{2,}\s+/.test(lines[index])) {
      break;
    }
    body.push(lines[index]);
  }

  return body.join('\n').trim();
}

function conciseLines(markdown, limit = 8) {
  return markdown
    .split(/\r?\n/)
    .map(line => line.trim())
    .filter(line => line && !line.startsWith('|') && !line.match(/^[-|: ]+$/))
    .slice(0, limit);
}

function ownerFromTopics(topics = []) {
  const ownerTopic = topics.find(topic => topic.startsWith('owner-'));
  if (!ownerTopic) {
    return '';
  }

  return ownerTopic
    .replace(/^owner-/, '')
    .replace('-at-', '@')
    .replace('-dot-com', '.com')
    .replace(/-dot-/g, '.');
}

function tagsFor(folder) {
  if (folder === 'mcp-servers') {
    return '[ai-tools, gss-internal, mcp-server]';
  }

  return '[ai-tools, gss-internal, launchpad]';
}

function formatSourceNotes(tool) {
  const lines = [];

  if (tool.language) {
    lines.push(`- **Primary language:** ${tool.language}`);
  }
  if (tool.change_type) {
    lines.push(
      `- **Catalog trigger:** ${tool.change_type} — ${tool.reason || 'Queued by the catalog scout.'}`
    );
  }
  if (tool.pushed_at) {
    lines.push(`- **Repo last pushed:** ${tool.pushed_at.replace(/T.*$/, '')}`);
  }
  if (tool.topics?.length) {
    lines.push(`- **Repo topics:** ${tool.topics.map(topic => `\`${topic}\``).join(', ')}`);
  }

  return lines.length
    ? lines.join('\n')
    : '- Metadata is limited; refresh this page after the owner adds repo docs.';
}

function formatSetupNotes(readme) {
  const sections = [
    extractSection(
      readme,
      /^#{2,}\s+(Local Development|Development|Setup|Installation|Getting Started)/i
    ),
    extractSection(readme, /^#{2,}\s+(Deployment|Deploy)/i),
    extractSection(readme, /^#{2,}\s+(Environment Variables|Configuration)/i),
  ].filter(Boolean);

  const lines = sections.flatMap(section => conciseLines(section, 5)).slice(0, 12);

  if (lines.length === 0) {
    return 'No setup details were available in the README. Start with the LaunchPad page or repository, then ask the owner for access and environment details.';
  }

  return lines
    .map(line => {
      if (/^[-*]\s+/.test(line) || /^\d+\.\s+/.test(line) || line.startsWith('```')) {
        return line;
      }
      return `- ${line}`;
    })
    .join('\n');
}

function accessUrl(tool) {
  if (tool.homepage) {
    return tool.homepage;
  }

  return `https://launchpad.globalshopsolutions.dev/apps/${tool.name}`;
}

function articleFor(tool, targetFile, targetDir) {
  const folder = tool.target_folder || 'launchpad';
  const name = tool.name || tool.slug;
  const title = readmeTitle(tool.readme || '') || name;
  const summary = normalizeDescription(
    tool.description,
    firstParagraph(tool.readme || '') ||
      `${name} is an InternalTools repository tracked by the GSS catalog.`
  );
  const owner = ownerFromTopics(tool.topics || []);
  const sidebarPosition = existingSidebarPosition(targetFile, targetDir);
  const repoUrl = tool.repo_url || `https://github.com/GlobalShopSolutions-InternalTools/${name}`;
  const url = accessUrl(tool);
  const isMcp =
    folder === 'mcp-servers' || /mcp/i.test(`${name} ${summary} ${(tool.topics || []).join(' ')}`);
  const accessLabel = isMcp ? 'LaunchPad / MCP endpoint' : 'LaunchPad app';

  return `---
title: '${title.replace(/'/g, "''")}'
description: '${summary.replace(/'/g, "''")}'
sidebar_position: ${sidebarPosition}
last_updated: ${TODAY}
tags: ${tagsFor(folder)}
---

# ${title}

> **TL;DR** — ${summary}

## Overview

\`${name}\` is part of the GSS InternalTools catalog. This page was generated from repository metadata and README content so the tool is discoverable in the AI Tools at GSS catalog and future RAG-backed GSS Catalog.

${firstParagraph(tool.readme || '') || summary}

## Why use it

- **Discoverability.** Makes the tool visible from the central GSS catalog instead of leaving it hidden in GitHub or LaunchPad.
- **AI platform context.** Gives Cursor, Claude Code, Codex, and other assistants a stable page to retrieve when answering questions about GSS tooling.
- **Operational handoff.** Captures the owner, repo, access path, and setup clues in one place.

## When to use it

- You need the capability described by the README or repository description.
- You are onboarding to the owning team and need to find the repo, LaunchPad page, or setup notes.
- You are asking an AI assistant about this tool and want it to ground answers in catalog content.

## How to access it

- **${accessLabel}:** [${url}](${url})
- **Repo:** [${repoUrl}](${repoUrl})
- **Status:** Confirm with the owner or LaunchPad tile.

## Setup and usage notes

${formatSetupNotes(tool.readme || '')}

## How it fits the AI platform

The catalog treats InternalTools and LaunchPad apps as first-class AI context. Keeping this page current helps agents retrieve the right owner, repo, and operational notes before they suggest code changes, runbooks, or workflows.

## Source metadata

${formatSourceNotes(tool)}

## Owner & support

${owner ? `- **Owner:** [${owner}](mailto:${owner})` : '- **Owner:** Confirm from the repository topics or LaunchPad app page.'}
- **Repo:** [${repoUrl}](${repoUrl})
- **App page:** [${name} on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/${name})
- **Last reviewed:** ${TODAY}
`;
}

function applyHandoff(filePath) {
  const tool = readJson(filePath);
  const folder = tool.target_folder || 'launchpad';
  const slug = tool.slug || slugify(tool.name);
  const targetDir = path.join(CATALOG_DIR, folder);
  const targetFile = path.join(targetDir, `${slug}.md`);

  fs.mkdirSync(targetDir, { recursive: true });
  fs.writeFileSync(targetFile, articleFor(tool, targetFile, targetDir), 'utf8');
  fs.unlinkSync(filePath);

  return path.relative(ROOT, targetFile);
}

if (!fs.existsSync(HANDOFF_DIR)) {
  console.log('No data/new-tools directory found.');
  process.exit(0);
}

const handoffs = fs
  .readdirSync(HANDOFF_DIR)
  .filter(fileName => fileName.endsWith('.json'))
  .map(fileName => path.join(HANDOFF_DIR, fileName))
  .sort();

if (handoffs.length === 0) {
  console.log('No tool handoffs to apply.');
  process.exit(0);
}

for (const filePath of handoffs) {
  const target = applyHandoff(filePath);
  console.log(`Applied ${path.relative(ROOT, filePath)} -> ${target}`);
}
