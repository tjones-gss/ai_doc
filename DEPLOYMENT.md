# Deployment Guide

## Quick Start

Your documentation site is ready to deploy! Choose your preferred hosting platform below.

---

## Option 1: GitHub Pages (Recommended)

### Automatic Deployment with GitHub Actions

1. **Push your code to GitHub** (see GITHUB_SETUP.md)

2. **Create GitHub Actions workflow:**

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches:
      - main

permissions:
  contents: read
  pages: write
  id-token: write

jobs:
  deploy:
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: '18'
          cache: 'npm'

      - name: Install dependencies
        run: npm ci

      - name: Build website
        run: npm run build

      - name: Setup Pages
        uses: actions/configure-pages@v4

      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: ./build

      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
```

3. **Enable GitHub Pages:**
   - Go to repository Settings → Pages
   - Source: GitHub Actions
   - Save

4. **Your site will be live at:**
   ```
   https://tjones-gss.github.io/ai_doc/
   ```

### Manual Deployment

```bash
# Build the site
npm run build

# Deploy using gh-pages branch
npm run deploy
```

---

## Option 2: Netlify

### Quick Deploy

1. **Install Netlify CLI:**

   ```bash
   npm install -g netlify-cli
   ```

2. **Build and deploy:**
   ```bash
   npm run build
   netlify deploy --prod --dir=build
   ```

### Continuous Deployment

1. Go to https://app.netlify.com/
2. Click "Add new site" → "Import an existing project"
3. Connect to GitHub and select your repository
4. Configure build settings:
   - **Build command:** `npm run build`
   - **Publish directory:** `build`
   - **Node version:** 18
5. Click "Deploy site"

**Your site URL:** `https://[random-name].netlify.app`

---

## Option 3: Vercel

### Quick Deploy

1. **Install Vercel CLI:**

   ```bash
   npm install -g vercel
   ```

2. **Deploy:**
   ```bash
   vercel
   ```

### Continuous Deployment

1. Go to https://vercel.com/
2. Click "Add New..." → "Project"
3. Import your GitHub repository
4. Vercel auto-detects Docusaurus settings
5. Click "Deploy"

**Your site URL:** `https://[project-name].vercel.app`

---

## Option 4: Internal Server (IIS/Apache/Nginx)

### Build the static site:

```bash
npm run build
```

The static files will be in the `build/` directory.

### IIS (Windows Server)

1. Copy `build/` contents to `C:\inetpub\wwwroot\ai-docs\`
2. Open IIS Manager
3. Create new site or application pointing to the folder
4. Set default document to `index.html`
5. Enable static content compression (optional)

### Apache

```apache
<VirtualHost *:80>
    ServerName ai-docs.internal
    DocumentRoot /var/www/ai-docs

    <Directory /var/www/ai-docs>
        Options Indexes FollowSymLinks
        AllowOverride All
        Require all granted

        # Enable single-page app routing
        RewriteEngine On
        RewriteBase /
        RewriteRule ^index\.html$ - [L]
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteRule . /index.html [L]
    </Directory>
</VirtualHost>
```

### Nginx

```nginx
server {
    listen 80;
    server_name ai-docs.internal;
    root /var/www/ai-docs;
    index index.html;

    location / {
        try_files $uri $uri/ /index.html;
    }

    # Enable gzip compression
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml;
}
```

---

## Environment Variables

For production deployments, you may want to update:

### `docusaurus.config.js`

```javascript
// Update these values based on your hosting:
url: 'https://tjones-gss.github.io',  // Your domain
baseUrl: '/ai_doc/',                   // Your base path
```

### For custom domain:

```javascript
url: 'https://ai-docs.yourcompany.com',
baseUrl: '/',
```

---

## Post-Deployment Checklist

- [ ] Site loads correctly
- [ ] All pages are accessible
- [ ] Navigation works (sidebar, footer)
- [ ] Images display properly
- [ ] Search works (if enabled)
- [ ] Light/dark theme toggle works
- [ ] Mobile responsive
- [ ] SSL certificate active (HTTPS)
- [ ] Test "Edit this page" links
- [ ] Verify social card displays on sharing

---

## Performance Optimization

After deployment, consider:

1. **Enable CDN** (Cloudflare, AWS CloudFront)
2. **Add Algolia DocSearch** for better search
3. **Enable compression** (gzip/brotli)
4. **Set up caching headers**
5. **Monitor with Google Analytics** (optional)

---

## Monitoring & Updates

### Check build status:

```bash
npm run validate  # Run all checks
npm run build     # Test build locally
```

### Update dependencies:

```bash
npm update        # Update packages
npm run validate  # Verify everything works
```

### Make updates:

```bash
git add .
git commit -m "Update documentation"
git push origin main  # Auto-deploys if CI/CD is set up
```

---

## Troubleshooting

### Build fails on deployment

```bash
# Clear node_modules and rebuild
rm -rf node_modules package-lock.json
npm install
npm run build
```

### 404 errors on page refresh

Add proper routing configuration for your server (see IIS/Apache/Nginx configs above).

### Images not loading

Check that `baseUrl` in `docusaurus.config.js` matches your deployment path.

### Assets have wrong URLs

Verify the `url` and `baseUrl` settings in `docusaurus.config.js`.

---

## Need Help?

- **Docusaurus Docs:** https://docusaurus.io/docs/deployment
- **GitHub Pages Guide:** https://docs.github.com/en/pages
- **Your README:** See README.md for local development

---

**Happy Deploying! 🚀**

Your professional AI documentation site is ready to share with your team!
