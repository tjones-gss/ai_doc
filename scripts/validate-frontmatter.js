#!/usr/bin/env node

/**
 * Front Matter Validation Script
 *
 * Validates that all documentation files have required front matter fields
 * and that values are properly formatted
 */

const fs = require('fs');
const path = require('path');
const glob = require('glob');
const matter = require('gray-matter');

const errors = [];
const warnings = [];

// Required front matter fields
const REQUIRED_FIELDS = ['title', 'description', 'sidebar_position'];

// Optional but recommended fields
const RECOMMENDED_FIELDS = ['last_updated', 'tags'];

/**
 * Validate front matter for a single file
 */
function validateFrontMatter(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const { data: frontMatter, isEmpty } = matter(content);

  const relativePath = path.relative(path.join(__dirname, '..'), filePath);

  // Check if front matter exists
  if (isEmpty) {
    errors.push({
      file: relativePath,
      error: 'No front matter found',
    });
    return;
  }

  // Check required fields
  REQUIRED_FIELDS.forEach(field => {
    if (!(field in frontMatter)) {
      errors.push({
        file: relativePath,
        field,
        error: `Missing required field: ${field}`,
      });
    } else if (!frontMatter[field] || frontMatter[field] === '') {
      errors.push({
        file: relativePath,
        field,
        error: `Field "${field}" is empty`,
      });
    }
  });

  // Check recommended fields
  RECOMMENDED_FIELDS.forEach(field => {
    if (!(field in frontMatter)) {
      warnings.push({
        file: relativePath,
        field,
        warning: `Missing recommended field: ${field}`,
      });
    }
  });

  // Validate specific field formats
  if (frontMatter.sidebar_position !== undefined) {
    if (typeof frontMatter.sidebar_position !== 'number') {
      errors.push({
        file: relativePath,
        field: 'sidebar_position',
        error: 'sidebar_position must be a number',
      });
    }
  }

  if (frontMatter.tags !== undefined) {
    if (!Array.isArray(frontMatter.tags)) {
      errors.push({
        file: relativePath,
        field: 'tags',
        error: 'tags must be an array',
      });
    }
  }

  if (frontMatter.last_updated !== undefined) {
    const date = new Date(frontMatter.last_updated);
    if (isNaN(date.getTime())) {
      errors.push({
        file: relativePath,
        field: 'last_updated',
        error: 'last_updated is not a valid date',
      });
    }
  }

  // Check for duplicate sidebar positions (will be checked globally)
  return {
    file: relativePath,
    frontMatter,
  };
}

/**
 * Check for duplicate sidebar positions
 */
function checkDuplicateSidebarPositions(allDocs) {
  const positionMap = new Map();

  allDocs.forEach(doc => {
    if (doc.frontMatter.sidebar_position !== undefined) {
      const pos = doc.frontMatter.sidebar_position;
      if (positionMap.has(pos)) {
        errors.push({
          file: doc.file,
          error: `Duplicate sidebar_position ${pos} (also used in ${positionMap.get(pos)})`,
        });
      } else {
        positionMap.set(pos, doc.file);
      }
    }
  });
}

/**
 * Main validation function
 */
function main() {
  console.log('📋 Validating front matter...\n');

  // Find all markdown files
  const docsPattern = path.join(__dirname, '../docs/**/*.md');
  const docFiles = glob.sync(docsPattern);

  console.log(`Found ${docFiles.length} documentation files\n`);

  // Validate each file
  const allDocs = docFiles.map(file => validateFrontMatter(file)).filter(Boolean);

  // Check for duplicate sidebar positions
  checkDuplicateSidebarPositions(allDocs);

  // Report results
  console.log(`Checked front matter in ${docFiles.length} files\n`);

  if (warnings.length > 0) {
    console.log(`⚠️  ${warnings.length} warning(s):\n`);
    warnings.forEach((warning, i) => {
      console.log(`${i + 1}. ${warning.file}`);
      console.log(`   ${warning.warning}\n`);
    });
  }

  if (errors.length === 0) {
    console.log('✅ All front matter is valid!\n');
    process.exit(0);
  } else {
    console.log(`❌ Found ${errors.length} error(s):\n`);
    errors.forEach((error, i) => {
      console.log(`${i + 1}. ${error.file}`);
      console.log(`   ${error.error}\n`);
    });
    process.exit(1);
  }
}

// Run validation
main();
