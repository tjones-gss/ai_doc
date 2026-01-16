/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  // By default, Docusaurus generates a sidebar from the docs folder structure
  tutorialSidebar: [
    {
      type: 'doc',
      id: 'overview',
      label: 'Overview',
    },
    {
      type: 'category',
      label: 'AI Tools',
      collapsed: false,
      items: ['cursor', 'chatgpt', 'codex', 'augment-ai', 'claude-code'],
    },
    {
      type: 'category',
      label: 'Methodology',
      collapsed: false,
      items: ['workflow', 'memory-and-change-history', 'ai-workflow-svn'],
    },
    {
      type: 'category',
      label: 'Stack Guides',
      collapsed: false,
      items: ['architecture-overview', 'legacy-modern-handbook', 'cobol-development-guide'],
    },
    {
      type: 'category',
      label: 'Engineering Practices',
      collapsed: false,
      items: ['testing-strategy'],
    },
    {
      type: 'category',
      label: 'Platform Guides',
      collapsed: false,
      items: ['datalayer-guide', 'devexpress-ui-guide'],
    },
    {
      type: 'category',
      label: 'Guides',
      collapsed: false,
      items: ['prompting-guide', 'qa-workflows'],
    },
    {
      type: 'category',
      label: 'Resources',
      collapsed: false,
      items: ['templates', 'prompt-library', 'troubleshooting'],
    },
  ],
};

module.exports = sidebars;
