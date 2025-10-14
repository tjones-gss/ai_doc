# Playwright Configuration for Claude Code

This guide explains how to use the Playwright configuration files set up in this project with Claude Code for effective web testing and automation.

## 🚀 Quick Start

### 1. Install Dependencies

First, install Playwright and its dependencies:

```bash
npm install
npx playwright install
```

### 2. Run Tests

```bash
# Run all tests
npm run test:e2e

# Run tests in headed mode (see browser)
npm run test:e2e:headed

# Debug tests interactively
npm run test:e2e:debug

# Open Playwright UI for interactive testing
npm run test:e2e:ui
```

## 📁 Project Structure

```
tests/
├── e2e/                    # End-to-end test files
│   ├── homepage.spec.js    # Homepage functionality tests
│   ├── documentation.spec.js # Documentation navigation tests
│   └── performance.spec.js # Performance and accessibility tests
├── utils/
│   └── test-helpers.js     # Reusable utility functions
├── global-setup.js         # Global test setup
└── global-teardown.js      # Global test cleanup
playwright.config.js        # Main Playwright configuration
```

## 🎯 Using Claude Code with Playwright

### Writing Tests with Claude

When working with Claude Code, you can ask for help with:

1. **Creating new test files:**

   ```
   "Create a Playwright test for the user login functionality"
   ```

2. **Debugging existing tests:**

   ```
   "Help me debug this failing Playwright test: [paste test code]"
   ```

3. **Enhancing test coverage:**
   ```
   "Add accessibility checks to my existing Playwright tests"
   ```

### Common Claude Prompts for Playwright

#### Test Creation

```
Create a Playwright test that:
- Navigates to the contact form page
- Fills out all required fields
- Submits the form
- Verifies success message appears
```

#### Test Enhancement

```
Enhance this Playwright test to include:
- Screenshot capture on failure
- Mobile responsiveness checks
- Performance monitoring
- Error handling
```

#### Debugging

```
This Playwright test is failing with a timeout error. Help me:
- Add proper wait conditions
- Implement retry logic
- Improve element selectors
```

## 🛠️ Configuration Features

### Multi-Browser Testing

The configuration supports testing across:

- **Chromium** (Chrome, Edge)
- **Firefox**
- **WebKit** (Safari)
- **Mobile browsers** (Mobile Chrome, Mobile Safari)

### Automatic Screenshots & Videos

- Screenshots taken on test failure
- Videos recorded for failed tests
- Full-page screenshots available via helpers

### Performance Monitoring

- Core Web Vitals measurement
- Page load time tracking
- Network condition simulation

### Accessibility Testing

- Keyboard navigation testing
- Color contrast validation
- Heading hierarchy verification
- Alt text compliance

## 📝 Test Writing Patterns

### Basic Test Structure

```javascript
import { test, expect } from '@playwright/test';
import { waitForPageLoad, takeScreenshot } from '../utils/test-helpers.js';

test.describe('Feature Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await waitForPageLoad(page);
  });

  test('should perform expected behavior', async ({ page }) => {
    // Test implementation
    await expect(page).toHaveTitle(/Expected Title/);
    await takeScreenshot(page, 'feature-test');
  });
});
```

### Using Test Helpers

```javascript
import {
  waitForElement,
  fillWithRetry,
  clickWithRetry,
  checkAccessibility,
  mockApiResponse,
} from '../utils/test-helpers.js';

// Wait for specific elements
const button = await waitForElement(page, '.submit-btn');

// Retry interactions
await fillWithRetry(page, '#email', 'test@example.com');
await clickWithRetry(page, '.submit-btn');

// Check accessibility
await checkAccessibility(page);

// Mock API responses
await mockApiResponse(page, '/api/users', { users: [] });
```

## 🔧 Advanced Usage with Claude

### Custom Test Generation

Ask Claude to generate tests based on your specific needs:

```
Generate a comprehensive Playwright test suite for an e-commerce site that includes:
- Product search and filtering
- Add to cart functionality
- Checkout process
- User account management
- Mobile responsiveness
- Performance benchmarks
```

### Test Data Management

```
Help me create a Playwright test that uses dynamic test data:
- Generate random user data for form testing
- Use different browser configurations
- Test with various screen sizes
- Handle authentication states
```

### CI/CD Integration

```
Create a GitHub Actions workflow that:
- Runs Playwright tests on multiple browsers
- Generates test reports
- Uploads test artifacts
- Runs on pull request and main branch
```

## 📊 Test Reports and Results

### HTML Reports

After running tests, view detailed HTML reports:

```bash
npx playwright show-report
```

### Test Artifacts

Find test results in:

- `test-results/` - Screenshots, videos, traces
- `playwright-report/` - HTML reports
- `test-results/results.json` - JSON test results

## 🎨 Visual Testing with Screenshots

### Automatic Screenshots

```javascript
// Screenshots are taken automatically on failure
// Manual screenshots:
await takeScreenshot(page, 'descriptive-name');
```

### Visual Comparisons

```javascript
// Compare current state with stored screenshot
await expect(page).toHaveScreenshot('homepage.png');
```

## 🔄 Continuous Integration

### Environment Variables

Set these environment variables in your CI/CD:

```bash
CI=true                    # Enables CI-specific settings
BASE_URL=https://your-site.com  # Override base URL
BROWSER=chromium          # Specify browser for CI
```

### GitHub Actions Example

```yaml
name: Playwright Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
      - run: npm ci
      - run: npx playwright install --with-deps
      - run: npm run test:e2e
      - uses: actions/upload-artifact@v3
        if: always()
        with:
          name: playwright-report
          path: playwright-report/
```

## 🚨 Troubleshooting Common Issues

### Test Timeouts

```javascript
// Increase timeout for slow operations
test.setTimeout(60000); // 60 seconds

// Or use specific waits
await page.waitForTimeout(5000);
await page.waitForLoadState('networkidle');
```

### Element Selection Issues

```javascript
// Use multiple selector strategies
const element = page.locator(
  ['data-testid=submit', '#submit-button', 'button:has-text("Submit")'].join(', ')
);
```

### Flaky Tests

```javascript
// Add retry logic
for (let i = 0; i < 3; i++) {
  try {
    await page.click('.button');
    break;
  } catch (error) {
    if (i === 2) throw error;
    await page.waitForTimeout(1000);
  }
}
```

## 📚 Resources and References

- [Playwright Documentation](https://playwright.dev/)
- [Best Practices Guide](https://playwright.dev/docs/best-practices)
- [API Reference](https://playwright.dev/docs/api/class-playwright)
- [Debugging Guide](https://playwright.dev/docs/debug)

## 💡 Tips for Working with Claude

1. **Be Specific:** Provide clear requirements when asking for test creation
2. **Share Context:** Include relevant HTML structure or application details
3. **Iterative Development:** Start with basic tests and enhance incrementally
4. **Error Sharing:** Share complete error messages for accurate debugging help
5. **Configuration Questions:** Ask about specific Playwright configuration options

## 🤝 Getting Help

When asking Claude for Playwright assistance:

1. **Provide context:** Share the relevant test file or configuration
2. **Include error messages:** Full stack traces help with debugging
3. **Specify requirements:** What exactly should the test do?
4. **Mention constraints:** Browser requirements, performance needs, etc.

Example prompt:

```
I have a Playwright test that needs to:
1. Test a multi-step form wizard
2. Handle dynamic content loading
3. Work across desktop and mobile
4. Include accessibility checks

Here's my current test structure: [paste code]
Can you help me enhance it?
```

This setup provides a robust foundation for web testing with Playwright, making it easy to collaborate with Claude Code on test development and maintenance.
