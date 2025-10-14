/**
 * Global teardown for Playwright tests
 * This runs once after all tests complete
 */
/* eslint-disable no-console, @typescript-eslint/no-unused-vars */

async function globalTeardown(config) {
  console.log('🧹 Starting global teardown for Playwright tests');

  // Clean up any global state, connections, or resources here
  // For example, you might want to:
  // - Close database connections
  // - Clean up test data
  // - Stop services
  // - Remove temporary files

  // Example: Clean up global server if one was started
  // if (global.__TEST_SERVER__) {
  //   await global.__TEST_SERVER__.close();
  //   delete global.__TEST_SERVER__;
  // }

  console.log('✅ Global teardown completed');
}

export default globalTeardown;
