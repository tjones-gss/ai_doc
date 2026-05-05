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
    {
      type: 'category',
      label: 'AI Tools at GSS (Catalog)',
      collapsed: false,
      items: [
        'ai-tools-catalog/overview',
        'ai-tools-catalog/claude-code-at-gss',
        {
          type: 'category',
          label: 'LaunchPad Apps',
          collapsed: true,
          items: [
            'ai-tools-catalog/launchpad/agents',
            'ai-tools-catalog/launchpad/arc-scanner',
            'ai-tools-catalog/launchpad/bom-compare-generator',
            'ai-tools-catalog/launchpad/bug-fixer',
            'ai-tools-catalog/launchpad/bug-traffic-dashboard',
            'ai-tools-catalog/launchpad/bug-triage-pack',
            'ai-tools-catalog/launchpad/career-path-tracker',
            'ai-tools-catalog/launchpad/CausingIssueDashboard',
            'ai-tools-catalog/launchpad/clinic-utilities',
            'ai-tools-catalog/launchpad/cobol-mcp',
            'ai-tools-catalog/launchpad/designerconsolidation',
            'ai-tools-catalog/launchpad/featuretoggles',
            'ai-tools-catalog/launchpad/gab-codeeditor-mcp',
            'ai-tools-catalog/launchpad/gabmcpserver',
            'ai-tools-catalog/launchpad/globe-tv',
            'ai-tools-catalog/launchpad/GSPLinkDecoder',
            'ai-tools-catalog/launchpad/gsshelp-letterpress',
            'ai-tools-catalog/launchpad/gss-stripe-connect',
            'ai-tools-catalog/launchpad/hookmaintenance',
            'ai-tools-catalog/launchpad/ihop-rapid-relay',
            'ai-tools-catalog/launchpad/issue-manager-mcp',
            'ai-tools-catalog/launchpad/log-parser',
            'ai-tools-catalog/launchpad/mcp-artifacts',
            'ai-tools-catalog/launchpad/mobile-crm-status',
            'ai-tools-catalog/launchpad/pervasiveschema',
            'ai-tools-catalog/launchpad/program-issue-tracker',
            'ai-tools-catalog/launchpad/QA-daily-standup-rotation',
            'ai-tools-catalog/launchpad/QA-resolved-issues',
            'ai-tools-catalog/launchpad/qchat',
            'ai-tools-catalog/launchpad/queue-routing',
            'ai-tools-catalog/launchpad/quick-option-import-export-web',
            'ai-tools-catalog/launchpad/rapid-ihop-relay',
            'ai-tools-catalog/launchpad/svn-ops',
            'ai-tools-catalog/launchpad/TABugTracking',
            'ai-tools-catalog/launchpad/TAD-Weekly-Stats',
            'ai-tools-catalog/launchpad/testarchitect-mcp',
            'ai-tools-catalog/launchpad/weekly-performance-dashboard',
            'ai-tools-catalog/launchpad/zen-data-builder',
            'ai-tools-catalog/launchpad/zen-log-parser',
          ],
        },
        {
          type: 'category',
          label: 'MCP Servers (Claude Code)',
          collapsed: true,
          items: [
            'ai-tools-catalog/mcp-servers/mcp-intelligence',
            'ai-tools-catalog/mcp-servers/internal-tools-docs',
            'ai-tools-catalog/mcp-servers/internal-tools-status',
            'ai-tools-catalog/mcp-servers/cobol-codebase',
            'ai-tools-catalog/mcp-servers/microsoft-365',
            'ai-tools-catalog/mcp-servers/book-of-armaments',
            'ai-tools-catalog/mcp-servers/devexpress',
            'ai-tools-catalog/mcp-servers/github',
            'ai-tools-catalog/mcp-servers/notion',
            'ai-tools-catalog/mcp-servers/monday',
          ],
        },
        {
          type: 'category',
          label: 'Claude Code Concepts',
          collapsed: true,
          items: [
            'ai-tools-catalog/concepts/plugins',
            'ai-tools-catalog/concepts/skills',
            'ai-tools-catalog/concepts/hooks',
            'ai-tools-catalog/concepts/subagents',
          ],
        },
      ],
    },
  ],
};

module.exports = sidebars;
