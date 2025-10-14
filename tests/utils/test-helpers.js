/* eslint-disable no-console */
import { expect } from '@playwright/test';

/**
 * Utility functions for Playwright tests
 */

/**
 * Wait for an element to be visible and return it
 * @param {Page} page - Playwright page object
 * @param {string} selector - CSS selector
 * @param {number} timeout - Timeout in milliseconds
 */
export async function waitForElement(page, selector, timeout = 5000) {
  await page.waitForSelector(selector, { state: 'visible', timeout });
  return page.locator(selector);
}

/**
 * Take a screenshot with a descriptive name
 * @param {Page} page - Playwright page object
 * @param {string} name - Screenshot name
 */
export async function takeScreenshot(page, name) {
  await page.screenshot({
    path: `test-results/screenshots/${name}-${Date.now()}.png`,
    fullPage: true,
  });
}

/**
 * Wait for page to load completely
 * @param {Page} page - Playwright page object
 */
export async function waitForPageLoad(page) {
  await page.waitForLoadState('networkidle');
  await page.waitForLoadState('domcontentloaded');
}

/**
 * Check if element exists without throwing
 * @param {Page} page - Playwright page object
 * @param {string} selector - CSS selector
 * @returns {boolean}
 */
export async function elementExists(page, selector) {
  try {
    const element = await page.locator(selector).first();
    return await element.isVisible();
  } catch {
    return false;
  }
}

/**
 * Fill form field with retry logic
 * @param {Page} page - Playwright page object
 * @param {string} selector - CSS selector
 * @param {string} value - Value to fill
 */
export async function fillWithRetry(page, selector, value) {
  for (let i = 0; i < 3; i++) {
    try {
      await page.fill(selector, value);
      const currentValue = await page.inputValue(selector);
      if (currentValue === value) break;
    } catch (error) {
      if (i === 2) throw error;
      await page.waitForTimeout(1000);
    }
  }
}

/**
 * Click element with retry logic
 * @param {Page} page - Playwright page object
 * @param {string} selector - CSS selector
 */
export async function clickWithRetry(page, selector) {
  for (let i = 0; i < 3; i++) {
    try {
      await page.click(selector);
      break;
    } catch (error) {
      if (i === 2) throw error;
      await page.waitForTimeout(1000);
    }
  }
}

/**
 * Wait for navigation and verify URL
 * @param {Page} page - Playwright page object
 * @param {string} expectedUrl - Expected URL pattern
 */
export async function waitForNavigation(page, expectedUrl) {
  await page.waitForURL(expectedUrl);
  await waitForPageLoad(page);
}

/**
 * Check page accessibility
 * @param {Page} page - Playwright page object
 */
export async function checkAccessibility(page) {
  // Basic accessibility checks
  const title = await page.title();
  expect(title).toBeTruthy();
  expect(title.length).toBeGreaterThan(0);

  // Check for main landmarks
  const main = await elementExists(page, 'main');
  const nav = await elementExists(page, 'nav');

  console.log(`Accessibility check - Title: "${title}", Main: ${main}, Nav: ${nav}`);
}

/**
 * Get current viewport size
 * @param {Page} page - Playwright page object
 */
export async function getViewportSize(page) {
  return await page.viewportSize();
}

/**
 * Set mobile viewport
 * @param {Page} page - Playwright page object
 */
export async function setMobileViewport(page) {
  await page.setViewportSize({ width: 375, height: 667 });
}

/**
 * Set desktop viewport
 * @param {Page} page - Playwright page object
 */
export async function setDesktopViewport(page) {
  await page.setViewportSize({ width: 1920, height: 1080 });
}

/**
 * Mock API response
 * @param {Page} page - Playwright page object
 * @param {string} url - API endpoint to mock
 * @param {Object} response - Mock response data
 */
export async function mockApiResponse(page, url, response) {
  await page.route(url, async route => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: JSON.stringify(response),
    });
  });
}

/**
 * Wait for element to be stable (not animating)
 * @param {Page} page - Playwright page object
 * @param {string} selector - CSS selector
 */
export async function waitForElementStable(page, selector) {
  const element = page.locator(selector);
  await element.waitFor({ state: 'visible' });

  // Wait for any animations to complete
  let previousBox = await element.boundingBox();
  await page.waitForTimeout(100);

  for (let i = 0; i < 10; i++) {
    const currentBox = await element.boundingBox();

    if (JSON.stringify(previousBox) === JSON.stringify(currentBox)) {
      break;
    }

    previousBox = currentBox;
    await page.waitForTimeout(100);
  }
}
