#!/usr/bin/env node

/**
 * Link Validation Script
 *
 * Validates all internal markdown links in documentation
 * Checks that:
 * - Internal links point to existing files
 * - Anchor links point to existing headings
 * - No broken references
 */

const fs = require('fs');
const path = require('path');
const glob = require('glob');

const errors = [];

/**
 * Extract markdown links from content
 */
function extractLinks(content) {
  // Match [text](url) and [text](url "title")
  const linkRegex = /\[([^\]]+)\]\(([^)]+)\)/g;
  const links = [];
  let match;

  while ((match = linkRegex.exec(content)) !== null) {
    links.push({
      text: match[1],
      url: match[2].split(' ')[0].replace(/["']/g, ''), // Remove quotes and titles
      fullMatch: match[0],
    });
  }

  return links;
}

/**
 * Extract headings from markdown content
 */
function extractHeadings(content) {
  const headingRegex = /^#{1,6}\s+(.+)$/gm;
  const headings = [];
  let match;

  while ((match = headingRegex.exec(content)) !== null) {
    const heading = match[1].trim();
    // Convert to anchor ID (simplified Docusaurus algorithm)
    const anchor = heading
      .toLowerCase()
      .replace(/[^\w\s-]/g, '')
      .replace(/\s+/g, '-');
    headings.push(anchor);
  }

  return headings;
}

/**
 * Check if a link is internal (relative or starts with /)
 */
function isInternalLink(url) {
  // External links (http://, https://, mailto:, etc.)
  if (/^[a-z]+:/i.test(url)) {
    return false;
  }
  // Internal relative or absolute links
  return true;
}

/**
 * Resolve link path relative to source file
 */
function resolveLinkPath(sourceFile, linkUrl) {
  // Remove anchor
  const [filePath, anchor] = linkUrl.split('#');

  if (!filePath) {
    // Just an anchor link in same file
    return { resolvedPath: sourceFile, anchor };
  }

  const sourceDir = path.dirname(sourceFile);
  let resolvedPath;

  if (filePath.startsWith('/')) {
    // Absolute path from project root
    resolvedPath = path.join(__dirname, '..', filePath);
  } else {
    // Relative path
    resolvedPath = path.join(sourceDir, filePath);
  }

  // Handle .md extension
  if (!path.extname(resolvedPath)) {
    resolvedPath += '.md';
  }

  return { resolvedPath, anchor };
}

/**
 * Validate a single link
 */
function validateLink(sourceFile, link, allHeadingsByFile) {
  if (!isInternalLink(link.url)) {
    return; // Skip external links
  }

  const { resolvedPath, anchor } = resolveLinkPath(sourceFile, link.url);

  // Check if file exists
  if (!fs.existsSync(resolvedPath)) {
    errors.push({
      file: sourceFile,
      link: link.url,
      text: link.text,
      error: `Linked file does not exist: ${resolvedPath}`,
    });
    return;
  }

  // If there's an anchor, check if heading exists
  if (anchor) {
    const headings = allHeadingsByFile[resolvedPath] || [];
    if (!headings.includes(anchor)) {
      errors.push({
        file: sourceFile,
        link: link.url,
        text: link.text,
        error: `Anchor #${anchor} not found in ${path.basename(resolvedPath)}`,
      });
    }
  }
}

/**
 * Main validation function
 */
function main() {
  console.log('🔗 Validating internal links...\n');

  // Find all markdown files
  const docsPattern = path.join(__dirname, '../docs/**/*.md');
  const docFiles = glob.sync(docsPattern);

  console.log(`Found ${docFiles.length} documentation files\n`);

  // First pass: extract all headings from all files
  const allHeadingsByFile = {};
  docFiles.forEach(file => {
    const content = fs.readFileSync(file, 'utf8');
    allHeadingsByFile[file] = extractHeadings(content);
  });

  // Second pass: validate all links
  let totalLinks = 0;
  docFiles.forEach(file => {
    const content = fs.readFileSync(file, 'utf8');
    const links = extractLinks(content);
    const internalLinks = links.filter(link => isInternalLink(link.url));

    totalLinks += internalLinks.length;

    internalLinks.forEach(link => {
      validateLink(file, link, allHeadingsByFile);
    });
  });

  // Report results
  console.log(`Checked ${totalLinks} internal links\n`);

  if (errors.length === 0) {
    console.log('✅ All links are valid!\n');
    process.exit(0);
  } else {
    console.log(`❌ Found ${errors.length} broken link(s):\n`);
    errors.forEach((error, i) => {
      const relativePath = path.relative(path.join(__dirname, '..'), error.file);
      console.log(`${i + 1}. ${relativePath}`);
      console.log(`   Link: ${error.link}`);
      console.log(`   Text: "${error.text}"`);
      console.log(`   Error: ${error.error}\n`);
    });
    process.exit(1);
  }
}

// Run validation
main();
