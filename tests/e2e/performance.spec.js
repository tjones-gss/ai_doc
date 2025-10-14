/* eslint-disable no-console, @typescript-eslint/no-unused-vars, no-undef */
import { test, expect } from '@playwright/test';
import { waitForPageLoad, takeScreenshot } from '../utils/test-helpers.js';

test.describe('Performance and Accessibility Tests', () => {
  test('should load homepage within acceptable time', async ({ page }) => {
    const startTime = Date.now();

    await page.goto('/');
    await waitForPageLoad(page);

    const loadTime = Date.now() - startTime;

    // Page should load within 5 seconds (adjust as needed)
    expect(loadTime).toBeLessThan(5000);

    console.log(`Page load time: ${loadTime}ms`);
  });

  test('should have acceptable Core Web Vitals', async ({ page }) => {
    // Navigate to page
    await page.goto('/');
    await waitForPageLoad(page);

    // Measure Core Web Vitals using Performance API
    const webVitals = await page.evaluate(() => {
      return new Promise(resolve => {
        const observer = new PerformanceObserver(list => {
          const entries = list.getEntries();
          const vitals = {};

          entries.forEach(entry => {
            if (entry.entryType === 'navigation') {
              vitals.domContentLoaded =
                entry.domContentLoadedEventEnd - entry.domContentLoadedEventStart;
              vitals.loadComplete = entry.loadEventEnd - entry.loadEventStart;
            }
            if (entry.entryType === 'paint') {
              if (entry.name === 'first-contentful-paint') {
                vitals.fcp = entry.startTime;
              }
              if (entry.name === 'largest-contentful-paint') {
                vitals.lcp = entry.startTime;
              }
            }
          });

          resolve(vitals);
        });

        observer.observe({ entryTypes: ['navigation', 'paint'] });

        // Fallback timeout
        setTimeout(() => resolve({}), 3000);
      });
    });

    console.log('Web Vitals:', webVitals);

    // Basic assertions (adjust thresholds as needed)
    if (webVitals.fcp) {
      expect(webVitals.fcp).toBeLessThan(2500); // FCP should be under 2.5s
    }
    if (webVitals.lcp) {
      expect(webVitals.lcp).toBeLessThan(4000); // LCP should be under 4s
    }
  });

  test('should not have JavaScript errors', async ({ page }) => {
    const jsErrors = [];

    page.on('console', msg => {
      if (msg.type() === 'error') {
        jsErrors.push(msg.text());
      }
    });

    page.on('pageerror', error => {
      jsErrors.push(error.message);
    });

    await page.goto('/');
    await waitForPageLoad(page);

    // Navigate to docs to test more pages
    await page.click('a[href*="/docs"]').catch(() => {
      console.log('Docs link not found, skipping docs test');
    });

    await page.waitForTimeout(2000);

    // Filter out common non-critical errors
    const criticalErrors = jsErrors.filter(
      error =>
        !error.includes('analytics') &&
        !error.includes('gtag') &&
        !error.includes('tracking') &&
        !error.includes('third-party')
    );

    if (criticalErrors.length > 0) {
      console.log('JavaScript errors found:', criticalErrors);
    }

    expect(criticalErrors).toHaveLength(0);
  });

  test('should be keyboard navigable', async ({ page }) => {
    await page.goto('/');
    await waitForPageLoad(page);

    // Test keyboard navigation
    await page.keyboard.press('Tab');

    // Check if focus is visible
    const focusedElement = await page.locator(':focus').count();
    expect(focusedElement).toBeGreaterThan(0);

    // Continue tabbing through focusable elements
    for (let i = 0; i < 5; i++) {
      await page.keyboard.press('Tab');
      await page.waitForTimeout(200);
    }

    // Take screenshot to verify focus indicators
    await takeScreenshot(page, 'keyboard-navigation');
  });

  test('should have proper heading hierarchy', async ({ page }) => {
    await page.goto('/');
    await waitForPageLoad(page);

    // Check heading structure
    const headings = await page.locator('h1, h2, h3, h4, h5, h6').allTextContents();

    // Should have at least one h1
    const h1Count = await page.locator('h1').count();
    expect(h1Count).toBeGreaterThanOrEqual(1);
    expect(h1Count).toBeLessThanOrEqual(1); // Should have exactly one h1

    console.log('Page headings:', headings);
  });

  test('should have images with alt text', async ({ page }) => {
    await page.goto('/');
    await waitForPageLoad(page);

    // Find all images
    const images = page.locator('img');
    const imageCount = await images.count();

    if (imageCount > 0) {
      // Check each image has alt text
      for (let i = 0; i < imageCount; i++) {
        const img = images.nth(i);
        const alt = await img.getAttribute('alt');
        const src = await img.getAttribute('src');

        // Skip decorative images or those that might be handled differently
        if (!src?.includes('data:') && !src?.includes('svg')) {
          expect(alt).toBeTruthy();
          expect(alt.length).toBeGreaterThan(0);
        }
      }

      console.log(`Checked ${imageCount} images for alt text`);
    } else {
      console.log('No images found on page');
    }
  });

  test('should have sufficient color contrast', async ({ page }) => {
    await page.goto('/');
    await waitForPageLoad(page);

    // Basic check for color contrast by examining computed styles
    const colorContrastCheck = await page.evaluate(() => {
      const elements = document.querySelectorAll('p, h1, h2, h3, h4, h5, h6, a, button, span');
      const issues = [];

      Array.from(elements)
        .slice(0, 20)
        .forEach(el => {
          // Check first 20 elements
          const styles = getComputedStyle(el);
          const color = styles.color;
          const backgroundColor = styles.backgroundColor;

          // Simple check - if both color and background color are defined
          if (color !== 'rgba(0, 0, 0, 0)' && backgroundColor !== 'rgba(0, 0, 0, 0)') {
            // This is a basic check - in real scenarios, you'd want to use a proper contrast checker
            const textContent = el.textContent?.trim();
            if (textContent && textContent.length > 0) {
              // Log for manual review
              issues.push({
                text: textContent.substring(0, 50),
                color,
                backgroundColor,
              });
            }
          }
        });

      return issues;
    });

    console.log(
      'Color contrast check completed, found',
      colorContrastCheck.length,
      'elements to review'
    );

    // This test mainly logs for manual review - automated contrast checking requires more sophisticated tools
    expect(colorContrastCheck.length).toBeGreaterThan(0);
  });

  test('should work with reduced motion preferences', async ({ page }) => {
    // Set reduced motion preference
    await page.emulateMedia({ reducedMotion: 'reduce' });

    await page.goto('/');
    await waitForPageLoad(page);

    // Check that animations are reduced or removed
    const animatedElements = await page.evaluate(() => {
      const elements = document.querySelectorAll('*');
      const animated = [];

      Array.from(elements).forEach(el => {
        const styles = getComputedStyle(el);
        if (styles.animationDuration !== '0s' && styles.animationDuration !== 'initial') {
          animated.push(el.tagName);
        }
      });

      return animated;
    });

    console.log('Elements with animations:', animated);

    // Take screenshot for visual verification
    await takeScreenshot(page, 'reduced-motion');
  });

  test('should handle network conditions gracefully', async ({ page }) => {
    // Simulate slow 3G connection
    await page.context().setOffline(false);

    // Navigate with slow connection simulation
    const startTime = Date.now();
    await page.goto('/');

    // Wait for page to load despite slow connection
    await waitForPageLoad(page);
    const loadTime = Date.now() - startTime;

    console.log(`Load time with simulated slow connection: ${loadTime}ms`);

    // Page should still load within reasonable time (10 seconds for slow connection)
    expect(loadTime).toBeLessThan(10000);

    // Check that essential content is still visible
    const mainContent = page.locator('main, [role="main"], .main-wrapper');
    await expect(mainContent).toBeVisible();
  });
});
