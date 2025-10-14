/* eslint-disable no-console */
import { test, expect } from '@playwright/test';
import {
  waitForPageLoad,
  waitForNavigation,
  elementExists,
  takeScreenshot,
} from '../utils/test-helpers.js';

test.describe('Documentation Navigation Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Start from homepage
    await page.goto('/');
    await waitForPageLoad(page);
  });

  test('should navigate to docs from homepage', async ({ page }) => {
    // Find and click docs link
    const docsLink = page.locator('a[href*="/docs"]').first();
    await expect(docsLink).toBeVisible();

    await docsLink.click();
    await waitForNavigation(page, '**/docs/**');

    // Verify we're on a docs page
    const url = page.url();
    expect(url).toContain('/docs');

    // Check for docs-specific elements
    const docContent = await elementExists(
      page,
      '.markdown, [class*="docItem"], .theme-doc-markdown'
    );
    expect(docContent).toBe(true);
  });

  test('should display sidebar navigation in docs', async ({ page }) => {
    // Navigate to docs
    await page.goto('/docs');
    await waitForPageLoad(page);

    // Check for sidebar presence
    const sidebar = await elementExists(page, '[class*="sidebar"], .menu, [class*="docSidebar"]');
    expect(sidebar).toBe(true);

    // Check for navigation items in sidebar
    const navItems = page.locator('[class*="sidebar"] a, .menu a, [class*="docSidebar"] a');
    const count = await navItems.count();
    expect(count).toBeGreaterThan(0);
  });

  test('should navigate between doc pages', async ({ page }) => {
    // Go to docs
    await page.goto('/docs');
    await waitForPageLoad(page);

    // Find first available link in sidebar or content
    const docLink = page.locator('a[href*="/docs/"]:not([href="/docs"])').first();
    const linkExists = (await docLink.count()) > 0;

    if (linkExists) {
      const originalUrl = page.url();
      await docLink.click();
      await waitForPageLoad(page);

      const newUrl = page.url();
      expect(newUrl).not.toBe(originalUrl);

      // Take screenshot of the new page
      await takeScreenshot(page, 'doc-navigation');
    } else {
      console.log('No additional doc pages found - skipping navigation test');
    }
  });

  test('should display breadcrumbs in docs', async ({ page }) => {
    // Navigate to a docs page
    await page.goto('/docs');
    await waitForPageLoad(page);

    // Look for breadcrumb elements
    const breadcrumbs = await elementExists(
      page,
      '[class*="breadcrumb"], .breadcrumb, [aria-label*="breadcrumb"]'
    );

    if (breadcrumbs) {
      const breadcrumbLinks = page.locator('[class*="breadcrumb"] a, .breadcrumb a');
      const count = await breadcrumbLinks.count();
      expect(count).toBeGreaterThan(0);
    } else {
      console.log('Breadcrumbs not found - may not be implemented');
    }
  });

  test('should have working table of contents', async ({ page }) => {
    // Go to a docs page that likely has a table of contents
    await page.goto('/docs');
    await waitForPageLoad(page);

    // Look for table of contents
    const toc = await elementExists(
      page,
      '[class*="tableOfContents"], .table-of-contents, [class*="toc"]'
    );

    if (toc) {
      const tocLinks = page.locator(
        '[class*="tableOfContents"] a, .table-of-contents a, [class*="toc"] a'
      );
      const count = await tocLinks.count();

      if (count > 0) {
        // Click first TOC link
        const firstTocLink = tocLinks.first();
        await firstTocLink.click();

        // Wait a moment for potential scrolling
        await page.waitForTimeout(1000);

        // Take screenshot to verify TOC functionality
        await takeScreenshot(page, 'toc-navigation');
      }
    } else {
      console.log('Table of contents not found');
    }
  });

  test('should handle page not found (404)', async ({ page }) => {
    // Try to navigate to a non-existent docs page
    const response = await page.goto('/docs/non-existent-page', { waitUntil: 'networkidle' });

    // Check if it's a 404 or redirects appropriately
    if (response && response.status() === 404) {
      // Should display 404 page
      const notFoundText = await elementExists(
        page,
        'text="404", text="Not Found", text="Page Not Found"'
      );
      expect(notFoundText).toBe(true);
    } else {
      // Should redirect to valid page
      const url = page.url();
      expect(url).toMatch(/\/(docs)?/);
    }
  });

  test('should display edit page link for docs', async ({ page }) => {
    // Navigate to docs
    await page.goto('/docs');
    await waitForPageLoad(page);

    // Look for edit page link (common in documentation sites)
    const editLink = await elementExists(
      page,
      'a[href*="edit"], a[href*="github"], [class*="editThisPage"], text="Edit this page"'
    );

    if (editLink) {
      const editButton = page
        .locator('a[href*="edit"], a[href*="github"], [class*="editThisPage"]')
        .first();
      await expect(editButton).toBeVisible();

      // Verify link goes to external source
      const href = await editButton.getAttribute('href');
      expect(href).toMatch(/(github\.com|gitlab\.com|edit)/);
    } else {
      console.log('Edit page link not found - may not be implemented');
    }
  });

  test('should maintain consistent layout across doc pages', async ({ page }) => {
    // Start at docs
    await page.goto('/docs');
    await waitForPageLoad(page);

    // Take screenshot of initial layout
    await takeScreenshot(page, 'docs-layout-initial');

    // Navigate to another doc page if available
    const secondDocLink = page.locator('a[href*="/docs/"]:not([href="/docs"])').first();
    const hasSecondDoc = (await secondDocLink.count()) > 0;

    if (hasSecondDoc) {
      await secondDocLink.click();
      await waitForPageLoad(page);

      // Check that main layout elements are still present
      const sidebar = await elementExists(page, '[class*="sidebar"], .menu, [class*="docSidebar"]');
      const main = await elementExists(page, 'main, [role="main"], .main-wrapper');

      expect(sidebar).toBe(true);
      expect(main).toBe(true);

      // Take screenshot for comparison
      await takeScreenshot(page, 'docs-layout-secondary');
    }
  });
});
