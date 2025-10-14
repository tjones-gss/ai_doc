/**
 * Global setup for Playwright tests
 * This runs once before all tests
 */
/* eslint-disable no-console, @typescript-eslint/no-unused-vars */

async function globalSetup(config) {
  console.log('🚀 Starting global setup for Playwright tests');

  // Set up any global state, authentication, or test data here
  // For example, you might want to:
  // - Set up test databases
  // - Create test users
  // - Download required files
  // - Set environment variables

  // Example: Set up environment variables for testing
  process.env.NODE_ENV = 'test';

  // You can also start services here if needed
  // const server = await startTestServer();
  // global.__TEST_SERVER__ = server;

  console.log('✅ Global setup completed');
}

export default globalSetup;
