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
      // Replace with your project's social card
      image: 'img/docusaurus-social-card.svg',
      navbar: {
        title: 'AI Modernization Guide',
        logo: {
          alt: 'Global Shop Solutions Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'tutorialSidebar',
            position: 'left',
            label: 'Documentation',
          },
          {
            href: 'https://github.com/tjones-gss/ai_doc',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
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
            title: 'Tools',
            items: [
              {
                label: 'Cursor',
                to: '/docs/cursor',
              },
              {
                label: 'ChatGPT Enterprise',
                to: '/docs/chatgpt',
              },
              {
                label: 'Codex',
                to: '/docs/codex',
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
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Global Shop Solutions. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['bash', 'diff', 'json', 'markdown', 'powershell'],
      },
      colorMode: {
        defaultMode: 'light',
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },
    }),
};

module.exports = config;
