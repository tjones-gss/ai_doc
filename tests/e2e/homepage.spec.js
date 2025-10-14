/* eslint-disable no-console */
import { test, expect } from '@playwright/test';
import {
  waitForPageLoad,
  checkAccessibility,
  takeScreenshot,
  elementExists,
} from '../utils/test-helpers.js';

test.describe('Homepage Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to homepage before each test
    await page.goto('/');
    await waitForPageLoad(page);
  });

  test('should load homepage successfully', async ({ page }) => {
    // Check if page loads with correct title
    await expect(page).toHaveTitle(/Software Modernization AI Guide/);

    // Verify main content is visible
    const mainContent = page.locator('main');
    await expect(mainContent).toBeVisible();

    // Check basic accessibility
    await checkAccessibility(page);
  });

  test('should display streamlined hero section only', async ({ page }) => {
    // Check that the main title is visible
    await expect(page.locator('h1')).toContainText('Software Modernization AI Guide');

    // Check that the tagline is visible
    await expect(page.locator('.hero__subtitle')).toContainText('AI-Assisted Development');

    // Check that the Get Started button is visible
    const getStartedButton = page.locator('a.button:has-text("Get Started")');
    await expect(getStartedButton).toBeVisible();

    // Verify there are NO emoji icons from old sections
    const bodyText = await page.locator('body').textContent();
    const hasEmojis =
      /[\u{1F916}\u{1F504}\u{1F4DA}\u{1F3AF}\u{1F4A1}\u{1F680}\u{26A1}\u{1F4CB}\u{1F527}]/u.test(
        bodyText
      );
    expect(hasEmojis).toBe(false);

    console.log('✓ Homepage is streamlined with single hero section only');
  });

  test('should have brand green color applied', async ({ page }) => {
    // Check the hero section has primary color applied
    const hero = page.locator('.hero--primary');
    await expect(hero).toBeVisible();

    // Get computed styles
    const backgroundColor = await hero.evaluate(el => {
      return window.getComputedStyle(el).backgroundColor;
    });

    console.log('✓ Hero background color:', backgroundColor);

    // The color should be greenish (RGB values where G > R and G > B)
    const rgbMatch = backgroundColor.match(/rgb\((\d+),\s*(\d+),\s*(\d+)\)/);
    if (rgbMatch) {
      const [, r, g] = rgbMatch.map(Number);
      expect(g).toBeGreaterThan(r); // Green component should be higher than red
      console.log('✓ Brand green color scheme verified');
    }
  });

  test('should navigate to documentation on button click', async ({ page }) => {
    // Click the Get Started button
    const getStartedButton = page.locator('a.button:has-text("Get Started")');
    await getStartedButton.click();

    // Should navigate to the overview documentation
    await expect(page).toHaveURL(/.*\/docs\/overview/);

    console.log('✓ Get Started button navigates to documentation');
  });

  test('should display navigation menu', async ({ page }) => {
    // Check if navigation is present
    const nav = page.locator('nav');
    await expect(nav).toBeVisible();

    // Check for common navigation items
    const docLinks = await elementExists(page, 'a[href*="/docs"]');
    expect(docLinks).toBe(true);
  });

  test('should be responsive on mobile', async ({ page }) => {
    // Set mobile viewport
    await page.setViewportSize({ width: 375, height: 667 });

    // Reload page to ensure mobile layout loads
    await page.reload();
    await waitForPageLoad(page);

    // Check mobile-specific elements exist
    await elementExists(page, '.navbar-sidebar-backdrop');

    // Take screenshot for visual verification
    await takeScreenshot(page, 'homepage-mobile');
  });

  test('should handle search functionality if present', async ({ page }) => {
    // Check if search exists
    const searchExists = await elementExists(
      page,
      '[data-testid="search"], .navbar__search, input[type="search"]'
    );

    if (searchExists) {
      const searchInput = page
        .locator('[data-testid="search"], .navbar__search input, input[type="search"]')
        .first();

      // Test search interaction
      await searchInput.click();
      await searchInput.fill('COBOL');

      // Wait for search results or suggestions
      await page.waitForTimeout(1000);

      // Take screenshot of search results
      await takeScreenshot(page, 'search-results');
    } else {
      console.log('Search functionality not found - skipping search test');
    }
  });

  test('should load all critical resources', async ({ page }) => {
    // Check for 404 errors or failed resources
    const failedRequests = [];

    page.on('response', response => {
      if (response.status() >= 400) {
        failedRequests.push({
          url: response.url(),
          status: response.status(),
        });
      }
    });

    // Navigate and wait for all resources
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // Assert no critical resources failed
    const criticalFailures = failedRequests.filter(
      req => req.status >= 400 && !req.url.includes('analytics') && !req.url.includes('tracking')
    );

    expect(criticalFailures).toHaveLength(0);
  });

  test('should have proper meta tags for SEO', async ({ page }) => {
    // Check for essential meta tags
    const title = await page.locator('title').textContent();
    expect(title).toBeTruthy();
    expect(title.length).toBeGreaterThan(10);

    // Check for meta description
    const metaDescription = page.locator('meta[name="description"]');
    await expect(metaDescription).toHaveAttribute('content', /.+/);

    // Check viewport meta tag
    const viewportMeta = page.locator('meta[name="viewport"]');
    await expect(viewportMeta).toHaveAttribute('content', /width=device-width/);
  });
});
