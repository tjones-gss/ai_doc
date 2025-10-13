#!/usr/bin/env node

/**
 * Project Structure Validation Script
 *
 * Validates the overall project structure:
 * - Required directories exist
 * - Sidebar configuration matches actual files
 * - No orphaned files
 * - Proper file naming conventions
 */

const fs = require('fs');
const path = require('path');
const glob = require('glob');

const errors = [];
const warnings = [];

// Required directories
const REQUIRED_DIRS = ['docs', 'src', 'static', 'static/img', 'scripts'];

// Required files
const REQUIRED_FILES = ['package.json', 'docusaurus.config.js', 'sidebars.js', 'README.md'];

/**
 * Check if required directories exist
 */
function validateDirectories() {
  console.log('📁 Checking required directories...');

  REQUIRED_DIRS.forEach(dir => {
    const dirPath = path.join(__dirname, '..', dir);
    if (!fs.existsSync(dirPath)) {
      errors.push({
        type: 'missing_directory',
        path: dir,
        error: `Required directory missing: ${dir}`,
      });
    }
  });

  console.log(
    `   ${REQUIRED_DIRS.length - errors.filter(e => e.type === 'missing_directory').length}/${REQUIRED_DIRS.length} required directories found\n`
  );
}

/**
 * Check if required files exist
 */
function validateRequiredFiles() {
  console.log('📄 Checking required files...');

  REQUIRED_FILES.forEach(file => {
    const filePath = path.join(__dirname, '..', file);
    if (!fs.existsSync(filePath)) {
      errors.push({
        type: 'missing_file',
        path: file,
        error: `Required file missing: ${file}`,
      });
    }
  });

  console.log(
    `   ${REQUIRED_FILES.length - errors.filter(e => e.type === 'missing_file').length}/${REQUIRED_FILES.length} required files found\n`
  );
}

/**
 * Extract doc IDs from sidebar items recursively
 */
function extractDocIds(items, sidebarDocIds) {
  items.forEach(item => {
    if (typeof item === 'string') {
      sidebarDocIds.add(item);
    } else if (item.type === 'doc') {
      sidebarDocIds.add(item.id);
    } else if (item.type === 'category' && item.items) {
      extractDocIds(item.items, sidebarDocIds);
    }
  });
}

/**
 * Validate sidebar configuration matches actual files
 */
function validateSidebarConfig() {
  console.log('📋 Validating sidebar configuration...');

  const sidebarsPath = path.join(__dirname, '../sidebars.js');
  if (!fs.existsSync(sidebarsPath)) {
    errors.push({
      type: 'config',
      error: 'sidebars.js not found',
    });
    return;
  }

  try {
    // Load sidebar config
    delete require.cache[require.resolve(sidebarsPath)];
    const sidebars = require(sidebarsPath);

    // Get all doc IDs from sidebar
    const sidebarDocIds = new Set();

    Object.values(sidebars).forEach(sidebar => {
      if (Array.isArray(sidebar)) {
        extractDocIds(sidebar, sidebarDocIds);
      }
    });

    // Get all actual doc files
    const docsDir = path.join(__dirname, '../docs');
    const actualDocs = glob
      .sync('**/*.md', { cwd: docsDir })
      .map(file => file.replace(/\.md$/, ''))
      .filter(file => !file.startsWith('_')); // Exclude files starting with _

    // Check for docs in sidebar but not in filesystem
    sidebarDocIds.forEach(docId => {
      const docPath = path.join(docsDir, `${docId}.md`);
      if (!fs.existsSync(docPath)) {
        errors.push({
          type: 'sidebar_mismatch',
          docId,
          error: `Sidebar references doc "${docId}" but file does not exist`,
        });
      }
    });

    // Check for orphaned docs (in filesystem but not in sidebar)
    actualDocs.forEach(docFile => {
      if (!sidebarDocIds.has(docFile)) {
        warnings.push({
          type: 'orphaned_doc',
          file: docFile,
          warning: `Doc "${docFile}.md" exists but is not referenced in sidebar`,
        });
      }
    });

    console.log(
      `   ${sidebarDocIds.size} docs in sidebar, ${actualDocs.length} docs in filesystem\n`
    );
  } catch (error) {
    errors.push({
      type: 'config',
      error: `Error parsing sidebars.js: ${error.message}`,
    });
  }
}

/**
 * Validate file naming conventions
 */
function validateFileNaming() {
  console.log('✏️  Checking file naming conventions...');

  const docsPattern = path.join(__dirname, '../docs/**/*.md');
  const docFiles = glob.sync(docsPattern);

  let invalidNames = 0;

  docFiles.forEach(file => {
    const basename = path.basename(file, '.md');

    // Check for spaces in filename
    if (basename.includes(' ')) {
      warnings.push({
        type: 'naming',
        file: path.relative(path.join(__dirname, '..'), file),
        warning: 'Filename contains spaces (use hyphens instead)',
      });
      invalidNames++;
    }

    // Check for uppercase in filename (except _category_.json)
    if (basename !== basename.toLowerCase() && !file.includes('_category_')) {
      warnings.push({
        type: 'naming',
        file: path.relative(path.join(__dirname, '..'), file),
        warning: 'Filename contains uppercase characters (use lowercase)',
      });
      invalidNames++;
    }
  });

  console.log(
    `   ${docFiles.length - invalidNames}/${docFiles.length} files follow naming conventions\n`
  );
}

/**
 * Check for empty directories
 */
function checkEmptyDirectories() {
  console.log('📦 Checking for empty directories...');

  const dirsToCheck = ['docs', 'static/img', 'src/pages', 'src/components'];
  let emptyDirs = 0;

  dirsToCheck.forEach(dir => {
    const dirPath = path.join(__dirname, '..', dir);
    if (fs.existsSync(dirPath)) {
      const files = fs.readdirSync(dirPath);
      if (files.length === 0) {
        warnings.push({
          type: 'empty_directory',
          path: dir,
          warning: `Directory "${dir}" is empty`,
        });
        emptyDirs++;
      }
    }
  });

  console.log(
    `   ${dirsToCheck.length - emptyDirs}/${dirsToCheck.length} directories have content\n`
  );
}

/**
 * Main validation function
 */
function main() {
  console.log('🏗️  Validating project structure...\n');

  validateDirectories();
  validateRequiredFiles();
  validateSidebarConfig();
  validateFileNaming();
  checkEmptyDirectories();

  // Report results
  console.log('─'.repeat(60));

  if (warnings.length > 0) {
    console.log(`\n⚠️  ${warnings.length} warning(s):\n`);
    warnings.forEach((warning, i) => {
      console.log(`${i + 1}. ${warning.warning}`);
      if (warning.file) console.log(`   File: ${warning.file}`);
      if (warning.path) console.log(`   Path: ${warning.path}`);
      console.log();
    });
  }

  if (errors.length === 0) {
    console.log('✅ Project structure is valid!\n');
    process.exit(0);
  } else {
    console.log(`❌ Found ${errors.length} error(s):\n`);
    errors.forEach((error, i) => {
      console.log(`${i + 1}. ${error.error}`);
      if (error.path) console.log(`   Path: ${error.path}`);
      if (error.docId) console.log(`   Doc ID: ${error.docId}`);
      console.log();
    });
    process.exit(1);
  }
}

// Run validation
main();
