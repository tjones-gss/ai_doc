// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const { themes } = require('prism-react-renderer');
const lightCodeTheme = themes.github;
const darkCodeTheme = themes.dracula;

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Software Modernization AI Guide',
  tagline: 'AI-Assisted Development for Global Shop Solutions',
  favicon: 'img/favicon.svg',

  // Set the production url of your site here
  url: 'https://tjones-gss.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  baseUrl: '/ai_doc/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'tjones-gss', // Usually your GitHub org/user name.
  projectName: 'ai_doc', // Usually your repo name.

  onBrokenLinks: 'warn',

  markdown: {
    mermaid: true,
  },

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      '@docusaurus/preset-classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Edit this page links - update with your actual repo URL
          editUrl: 'https://github.com/tjones-gss/ai_doc/tree/main/',
        },
        blog: false,
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      // SEO Metadata
      metadata: [
        {
          name: 'keywords',
          content:
            'AI, modernization, COBOL, software development, Cursor, ChatGPT, OpenAI, code generation, legacy code',
        },
        {
          name: 'description',
          content:
            'Comprehensive guide for AI-assisted COBOL modernization using Cursor, ChatGPT Enterprise, and OpenAI Codex. Learn workflows, best practices, and proven templates.',
        },
        {
          name: 'og:title',
          content: 'Software Modernization AI Guide - Global Shop Solutions',
        },
        {
          name: 'og:description',
          content:
            'Transform legacy code with AI assistance. Complete guide with workflows, templates, and best practices.',
        },
        {
          name: 'og:type',
          content: 'website',
        },
        {
          name: 'twitter:card',
          content: 'summary_large_image',
        },
      ],

      // Social card
      image: 'img/docusaurus-social-card.svg',

      // Navbar Configuration
      navbar: {
        title: 'AI Modernization Guide',
        logo: {
          alt: 'Global Shop Solutions Logo',
          src: 'img/logo.svg',
        },
        hideOnScroll: false,
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'tutorialSidebar',
            position: 'left',
            label: 'Documentation',
          },
          {
            to: '/docs/cursor',
            label: 'Cursor',
            position: 'left',
          },
          {
            to: '/docs/chatgpt',
            label: 'ChatGPT',
            position: 'left',
          },
          {
            to: '/docs/codex',
            label: 'Codex',
            position: 'left',
          },
          {
            to: '/docs/augment-ai',
            label: 'Augment AI',
            position: 'left',
          },
          {
            to: '/docs/claude-code',
            label: 'Claude Code',
            position: 'left',
          },
          {
            to: '/docs/prompt-library',
            label: 'Templates',
            position: 'left',
          },
          {
            href: 'https://github.com/tjones-gss/ai_doc',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      // Footer Configuration
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Documentation',
            items: [
              {
                label: 'Getting Started',
                to: '/docs/overview',
              },
              {
                label: 'Workflow',
                to: '/docs/workflow',
              },
              {
                label: 'Templates',
                to: '/docs/templates',
              },
            ],
          },
          {
            title: 'AI Tools',
            items: [
              {
                label: 'Cursor IDE',
                to: '/docs/cursor',
              },
              {
                label: 'ChatGPT Pro 5.2',
                to: '/docs/chatgpt',
              },
              {
                label: 'OpenAI Codex',
                to: '/docs/codex',
              },
              {
                label: 'Augment AI',
                to: '/docs/augment-ai',
              },
              {
                label: 'Claude Code',
                to: '/docs/claude-code',
              },
            ],
          },
          {
            title: 'Resources',
            items: [
              {
                label: 'Troubleshooting',
                to: '/docs/troubleshooting',
              },
              {
                label: 'Prompt Library',
                to: '/docs/prompt-library',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'GitHub',
                href: 'https://github.com/tjones-gss/ai_doc',
              },
            ],
          },
        ],
        copyright: `
          <div style="margin-top: 2rem; padding-top: 2rem; border-top: 1px solid rgba(255,255,255,0.1);">
            <p style="margin-bottom: 0.5rem;">Copyright © ${new Date().getFullYear()} Global Shop Solutions. All rights reserved.</p>
            <p style="font-size: 0.9rem; opacity: 0.8;">Built with Docusaurus</p>
          </div>
        `,
      },
      // Syntax Highlighting
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: [
          'bash',
          'diff',
          'json',
          'markdown',
          'powershell',
          'javascript',
          'typescript',
          'python',
          'java',
          'csharp',
          'cpp',
          'sql',
        ],
        magicComments: [
          {
            className: 'theme-code-block-highlighted-line',
            line: 'highlight-next-line',
            block: { start: 'highlight-start', end: 'highlight-end' },
          },
          {
            className: 'code-block-error-line',
            line: 'error-next-line',
          },
        ],
      },

      // Color Mode
      colorMode: {
        defaultMode: 'light',
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },

      // Docs Sidebar
      docs: {
        sidebar: {
          hideable: true,
          autoCollapseCategories: true,
        },
      },

      // Table of Contents
      tableOfContents: {
        minHeadingLevel: 2,
        maxHeadingLevel: 4,
      },
    }),
};

module.exports = config;
