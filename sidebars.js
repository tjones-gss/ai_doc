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
      items: ['cursor', 'chatgpt', 'codex'],
    },
    {
      type: 'category',
      label: 'Methodology',
      collapsed: false,
      items: ['workflow', 'memory-and-change-history'],
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
