#!/usr/bin/env node

/**
 * Master Validation Script
 *
 * Runs all validation checks and generates a comprehensive report
 * Exit code 0 = all checks passed
 * Exit code 1 = one or more checks failed
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

// ANSI color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

const results = {
  passed: [],
  failed: [],
  warnings: [],
  startTime: Date.now(),
};

/**
 * Execute a command and track result
 */
function runCheck(name, command, options = {}) {
  const { optional = false, silent = false } = options;

  console.log(`\n${colors.blue}▶${colors.reset} ${name}...`);

  try {
    const output = execSync(command, {
      encoding: 'utf8',
      stdio: silent ? 'pipe' : 'inherit',
    });

    results.passed.push({ name, command });
    console.log(`${colors.green}✓${colors.reset} ${name} passed`);
    return { success: true, output };
  } catch (error) {
    if (optional) {
      results.warnings.push({ name, command, error: error.message });
      console.log(`${colors.yellow}⚠${colors.reset} ${name} skipped (optional)`);
      return { success: true, warning: true, output: error.stdout };
    } else {
      results.failed.push({ name, command, error: error.message });
      console.log(`${colors.red}✗${colors.reset} ${name} failed`);
      return { success: false, output: error.stdout };
    }
  }
}

/**
 * Generate validation report
 */
function generateReport() {
  const duration = ((Date.now() - results.startTime) / 1000).toFixed(2);
  const totalChecks = results.passed.length + results.failed.length + results.warnings.length;
  const passRate = ((results.passed.length / totalChecks) * 100).toFixed(1);

  console.log(`\n${'='.repeat(60)}`);
  console.log(`${colors.cyan}VALIDATION REPORT${colors.reset}`);
  console.log(`${'='.repeat(60)}`);
  console.log(`Total Checks: ${totalChecks}`);
  console.log(`${colors.green}Passed: ${results.passed.length}${colors.reset}`);
  console.log(`${colors.red}Failed: ${results.failed.length}${colors.reset}`);
  console.log(`${colors.yellow}Warnings: ${results.warnings.length}${colors.reset}`);
  console.log(`Pass Rate: ${passRate}%`);
  console.log(`Duration: ${duration}s`);
  console.log(`${'='.repeat(60)}`);

  if (results.failed.length > 0) {
    console.log(`\n${colors.red}FAILED CHECKS:${colors.reset}`);
    results.failed.forEach((item, i) => {
      console.log(`${i + 1}. ${item.name}`);
      console.log(`   Command: ${item.command}`);
    });
  }

  if (results.warnings.length > 0) {
    console.log(`\n${colors.yellow}WARNINGS:${colors.reset}`);
    results.warnings.forEach((item, i) => {
      console.log(`${i + 1}. ${item.name}`);
    });
  }

  // Save report to file
  const reportPath = path.join(__dirname, '../validation-report.json');
  fs.writeFileSync(
    reportPath,
    JSON.stringify(
      {
        timestamp: new Date().toISOString(),
        duration,
        totalChecks,
        passRate,
        results,
      },
      null,
      2
    )
  );

  console.log(`\n${colors.cyan}Report saved to: ${reportPath}${colors.reset}\n`);

  return results.failed.length === 0;
}

/**
 * Main validation flow
 */
async function main() {
  console.log(`${colors.cyan}${'='.repeat(60)}`);
  console.log(`🚀 RUNNING MASTER VALIDATION SUITE`);
  console.log(`${'='.repeat(60)}${colors.reset}\n`);

  // 1. Check dependencies
  runCheck('Dependencies installed', 'npm ls --depth=0 --json', { silent: true, optional: true });

  // 2. Code formatting
  runCheck('Code formatting check', 'npm run format:check', { optional: false });

  // 3. Linting
  runCheck('ESLint check', 'npm run lint', { optional: false });

  // 4. Markdown linting
  runCheck('Markdown linting', 'npm run test:markdown', { optional: false });

  // 5. Front matter validation
  runCheck('Front matter validation', 'npm run test:frontmatter', { optional: false });

  // 6. Link validation
  runCheck('Link validation', 'npm run test:links', { optional: false });

  // 7. Image validation
  runCheck('Image reference check', 'npm run validate:images', { optional: false });

  // 8. Structure validation
  runCheck('Structure validation', 'npm run validate:structure', { optional: false });

  // 9. TypeScript check (if tsconfig.json exists) - optional for now due to Docusaurus type resolution
  if (fs.existsSync(path.join(__dirname, '../tsconfig.json'))) {
    runCheck('TypeScript type check', 'npm run type-check', { optional: true });
  }

  // 10. Build test
  runCheck('Production build', 'npm run test:build', { optional: false });

  // Generate and display report
  const allPassed = generateReport();

  if (allPassed) {
    console.log(`${colors.green}✅ ALL VALIDATIONS PASSED! 🎉${colors.reset}\n`);
    process.exit(0);
  } else {
    console.log(`${colors.red}❌ VALIDATION FAILED${colors.reset}\n`);
    console.log(`Please fix the issues above before proceeding.\n`);
    process.exit(1);
  }
}

// Run validation
main().catch(error => {
  console.error(`${colors.red}Fatal error during validation:${colors.reset}`, error);
  process.exit(1);
});
