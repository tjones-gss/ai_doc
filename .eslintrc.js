module.exports = {
  root: true,
  env: {
    browser: true,
    node: true,
    es2021: true,
  },
  extends: [
    'eslint:recommended',
    'plugin:react/recommended',
    'plugin:react-hooks/recommended',
    'prettier',
  ],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 12,
    sourceType: 'module',
  },
  plugins: ['react', 'react-hooks', '@typescript-eslint'],
  rules: {
    'no-console': 'warn',
    'no-unused-vars': 'off',
    '@typescript-eslint/no-unused-vars': [
      'error',
      {
        argsIgnorePattern: '^_',
        varsIgnorePattern: '^_',
      },
    ],
    'react/prop-types': 'off',
    'react/react-in-jsx-scope': 'off',
  },
  settings: {
    react: {
      version: 'detect',
    },
  },
  overrides: [
    {
      // Allow console.log in scripts (CLI tools need it)
      files: ['scripts/**/*.js'],
      rules: {
        'no-console': 'off',
      },
    },
    {
      // Allow console.log in test files (needed for debugging tests)
      files: ['tests/**/*.js', '**/*.spec.js', '**/*.test.js'],
      rules: {
        'no-console': 'off',
      },
    },
  ],
  ignorePatterns: ['build/', 'build-test/', '.docusaurus/', 'node_modules/', '*.config.js'],
};
