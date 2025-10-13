#!/usr/bin/env node

/**
 * Image Reference Validation Script
 *
 * Validates that all image references in documentation exist
 * Checks markdown image syntax: ![alt](path)
 */

const fs = require('fs');
const path = require('path');
const glob = require('glob');

const errors = [];
const warnings = [];

/**
 * Extract image references from markdown content
 */
function extractImageReferences(content) {
  // Match ![alt](path) and ![alt](path "title")
  const imageRegex = /!\[([^\]]*)\]\(([^)]+)\)/g;
  const images = [];
  let match;

  while ((match = imageRegex.exec(content)) !== null) {
    images.push({
      alt: match[1],
      path: match[2].split(' ')[0].replace(/["']/g, ''),
      fullMatch: match[0],
    });
  }

  return images;
}

/**
 * Check if image file exists
 */
function checkImageExists(sourceFile, imagePath) {
  // Skip external URLs
  if (/^https?:\/\//i.test(imagePath)) {
    return { exists: true, external: true };
  }

  const sourceDir = path.dirname(sourceFile);
  let resolvedPath;

  if (imagePath.startsWith('/')) {
    // Absolute path from project root (static files)
    resolvedPath = path.join(__dirname, '..', 'static', imagePath);
  } else {
    // Relative path
    resolvedPath = path.join(sourceDir, imagePath);
  }

  return {
    exists: fs.existsSync(resolvedPath),
    external: false,
    resolvedPath,
  };
}

/**
 * Validate images in a single file
 */
function validateFile(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const images = extractImageReferences(content);
  const relativePath = path.relative(path.join(__dirname, '..'), filePath);

  images.forEach(image => {
    const result = checkImageExists(filePath, image.path);

    if (!result.external && !result.exists) {
      errors.push({
        file: relativePath,
        image: image.path,
        alt: image.alt,
        error: `Image file not found: ${result.resolvedPath}`,
      });
    }

    // Warn if alt text is missing or too short
    if (!image.alt || image.alt.trim().length === 0) {
      warnings.push({
        file: relativePath,
        image: image.path,
        warning: 'Image missing alt text (accessibility issue)',
      });
    } else if (image.alt.length < 5) {
      warnings.push({
        file: relativePath,
        image: image.path,
        alt: image.alt,
        warning: 'Alt text is very short (consider adding more description)',
      });
    }
  });
}

/**
 * Find unused images in static/img
 */
function findUnusedImages(allReferencedImages) {
  const staticImgDir = path.join(__dirname, '../static/img');

  if (!fs.existsSync(staticImgDir)) {
    console.log('⚠️  static/img directory does not exist yet\n');
    return;
  }

  const allImages = glob.sync(path.join(staticImgDir, '**/*.*'));
  const referencedSet = new Set(allReferencedImages.map(img => path.normalize(img)));

  const unused = allImages.filter(imgPath => {
    const relativePath = path.relative(staticImgDir, imgPath);
    const webPath = `/img/${relativePath.replace(/\\/g, '/')}`;
    return !referencedSet.has(path.normalize(imgPath)) && !referencedSet.has(webPath);
  });

  if (unused.length > 0) {
    console.log(`📦 Found ${unused.length} unused image(s) in static/img:\n`);
    unused.forEach(img => {
      const relativePath = path.relative(staticImgDir, img);
      console.log(`   - ${relativePath}`);
    });
    console.log();
  }
}

/**
 * Main validation function
 */
function main() {
  console.log('🖼️  Validating image references...\n');

  // Find all markdown files
  const docsPattern = path.join(__dirname, '../docs/**/*.md');
  const docFiles = glob.sync(docsPattern);

  // Also check README and other root markdown files
  const rootMdFiles = glob.sync(path.join(__dirname, '../*.md'));
  const allFiles = [...docFiles, ...rootMdFiles];

  console.log(`Checking ${allFiles.length} files for image references\n`);

  // Validate all files
  const allReferencedImages = [];
  allFiles.forEach(file => {
    const content = fs.readFileSync(file, 'utf8');
    const images = extractImageReferences(content);
    images.forEach(img => allReferencedImages.push(img.path));
    validateFile(file);
  });

  // Check for unused images
  findUnusedImages(allReferencedImages);

  // Report results
  if (warnings.length > 0) {
    console.log(`⚠️  ${warnings.length} warning(s):\n`);
    warnings.forEach((warning, i) => {
      console.log(`${i + 1}. ${warning.file}`);
      console.log(`   Image: ${warning.image}`);
      console.log(`   ${warning.warning}\n`);
    });
  }

  if (errors.length === 0) {
    console.log('✅ All image references are valid!\n');
    process.exit(0);
  } else {
    console.log(`❌ Found ${errors.length} error(s):\n`);
    errors.forEach((error, i) => {
      console.log(`${i + 1}. ${error.file}`);
      console.log(`   Image: ${error.image}`);
      console.log(`   Alt: "${error.alt}"`);
      console.log(`   Error: ${error.error}\n`);
    });
    process.exit(1);
  }
}

// Run validation
main();
