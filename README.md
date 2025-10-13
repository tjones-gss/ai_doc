# Software Modernization AI Guide

Professional-grade living documentation for AI-assisted COBOL development at Global Shop Solutions.

## Overview

This documentation site serves as the evolving source of truth for our AI-assisted development workflow, combining the methodology of **Spec → Plan → Code → Review** with modern documentation practices.

## Features

- **Modern, professional design** - Clean, minimal interface with light/dark theme support
- **Comprehensive guides** - Coverage of Cursor, ChatGPT Enterprise, and Codex
- **Practical templates** - Ready-to-use templates for specs, memory files, and change history
- **Extensive prompt library** - Reusable prompts for common development tasks
- **Troubleshooting guide** - Solutions to common AI development issues
- **Living documentation** - Easy to update and evolve with your team's learnings

## Documentation Structure

```
docs/
├── overview.md                    - Introduction and vision
├── cursor.md                      - Cursor IDE setup and usage
├── chatgpt.md                     - ChatGPT Enterprise features
├── codex.md                       - Codex CLI and web interface
├── workflow.md                    - Spec → Plan → Code → Review methodology
├── memory-and-change-history.md   - Context tracking system
├── templates.md                   - Starter files and examples
├── troubleshooting.md             - Common issues and solutions
└── prompt-library.md              - Reusable prompts
```

## Getting Started

### Prerequisites

- Node.js 18.0 or higher
- npm or yarn

### Installation

1. Install dependencies:

```bash
npm install
```

2. Start the development server:

```bash
npm start
```

This will open the documentation site at `http://localhost:3000` with hot-reload enabled.

### Building for Production

Build the static site:

```bash
npm run build
```

The static files will be generated in the `build/` directory.

### Serve Production Build Locally

Test the production build:

```bash
npm run serve
```

## Deployment Options

### Option 1: GitHub Pages

1. Update `docusaurus.config.js` with your GitHub organization and repo name
2. Run:

```bash
npm run deploy
```

### Option 2: Netlify / Vercel

1. Connect your repository to Netlify or Vercel
2. Set build command: `npm run build`
3. Set publish directory: `build`

### Option 3: Internal Server

1. Build the site: `npm run build`
2. Copy the `build/` directory to your web server
3. Configure your web server to serve the static files

## Customization

### Branding

- **Logo**: Replace `static/img/logo.svg` with your company logo
- **Favicon**: Replace `static/img/favicon.ico`
- **Colors**: Edit theme colors in `src/css/custom.css`

### Configuration

Edit `docusaurus.config.js` to customize:

- Site title and tagline
- Navigation menu items
- Footer links
- GitHub repository links

### Content

All documentation is written in Markdown and located in the `docs/` folder. Simply edit the `.md` files to update content.

## Living Documentation Workflow

This documentation is designed to evolve with your team:

1. **Add new learnings** to the relevant sections
2. **Update templates** based on real-world usage
3. **Expand the prompt library** with effective prompts you discover
4. **Document troubleshooting solutions** when you encounter new issues

### Updating Documentation

1. Edit the relevant `.md` file in the `docs/` folder
2. Update the `last_updated` date in the front matter
3. Commit your changes to version control
4. Deploy the updated site

## Support

For issues or questions:

- Check the [Troubleshooting Guide](docs/troubleshooting.md)
- Review the [Docusaurus Documentation](https://docusaurus.io/)
- Consult with your team

## License

Copyright © 2025 Global Shop Solutions

---

**Built with [Docusaurus 3](https://docusaurus.io/)**
