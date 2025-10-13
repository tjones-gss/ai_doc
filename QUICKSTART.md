# Quick Start Guide

Get your documentation site up and running in minutes.

## Prerequisites

Make sure you have Node.js installed (version 18.0 or higher):

```bash
node --version
```

If you don't have Node.js, download it from [nodejs.org](https://nodejs.org/).

## Installation & Setup

### Step 1: Install Dependencies

```bash
npm install
```

This will install Docusaurus and all required packages.

### Step 2: Start Development Server

```bash
npm start
```

Your documentation site will open at `http://localhost:3000`.

The server includes hot-reload, so any changes you make to the documentation will automatically refresh in your browser.

## Making Your First Edit

### Edit the Overview Page

1. Open `docs/overview.md` in your favorite editor
2. Make a change (e.g., update the "Last Updated" date)
3. Save the file
4. The browser will automatically refresh with your changes

### Add a New Page

1. Create a new `.md` file in the `docs/` folder
2. Add front matter at the top:

```markdown
---
title: My New Page
description: Description of my page
sidebar_position: 10
last_updated: 2025-10-10
tags: [tag1, tag2]
---

# My New Page

Content goes here...
```

3. The page will automatically appear in the sidebar

## Building for Production

When you're ready to deploy:

```bash
npm run build
```

This creates a `build/` directory with static files ready for deployment.

To test the production build locally:

```bash
npm run serve
```

## Customization

### Update Site Title and Logo

1. Edit `docusaurus.config.js`
2. Update the `title` and `tagline` fields
3. Replace logo at `static/img/logo.svg`

### Change Colors

Edit `src/css/custom.css` to customize the color scheme:

```css
:root {
  --ifm-color-primary: #2e8555; /* Your primary color */
}
```

### Update Navigation

Edit `docusaurus.config.js` in the `navbar.items` section to add or modify navigation links.

## Deployment Options

### Deploy to GitHub Pages

1. Update `docusaurus.config.js` with your GitHub info:

```javascript
organizationName: 'your-github-username',
projectName: 'your-repo-name',
```

2. Deploy:

```bash
npm run deploy
```

### Deploy to Netlify

1. Push your code to GitHub
2. Connect your repo at [netlify.com](https://netlify.com)
3. Set build command: `npm run build`
4. Set publish directory: `build`
5. Deploy!

### Deploy to Internal Server

1. Build: `npm run build`
2. Copy the `build/` folder to your web server
3. Configure your server to serve static files

## Next Steps

- Read the full documentation at `/docs/overview`
- Explore the [Templates](docs/templates.md) section
- Check out the [Prompt Library](docs/prompt-library.md)
- Customize the site to match your team's branding

## Troubleshooting

### Port 3000 Already in Use

```bash
npm start -- --port 3001
```

### Clear Cache

```bash
npm run clear
npm start
```

### Build Errors

```bash
rm -rf node_modules package-lock.json
npm install
npm start
```

## Getting Help

- [Docusaurus Documentation](https://docusaurus.io/)
- Check the `README.md` for more details
- Review the [Troubleshooting Guide](docs/troubleshooting.md)

---

**Ready to go?** Run `npm start` and start exploring!
