# Deployment Guide

This application can be deployed to several platforms. Below are instructions for the most common options.

## Heroku

Heroku is recommended for Node.js applications with file uploads.

```bash
# Install Heroku CLI
brew install heroku/brew/heroku

# Login
heroku login

# Create app and deploy
heroku create your-app-name
git push heroku main

# Open your app
heroku open
```

## Vercel

Vercel works well for serverless deployments.

```bash
# Install Vercel CLI
npm install -g vercel

# Deploy
vercel --prod
```

Note: The project includes `vercel.json` and `api/index.js` for Vercel compatibility.

## Railway

Railway offers simple deployment with automatic HTTPS.

```bash
# Install Railway CLI
npm install -g @railway/cli

# Login and deploy
railway login
railway init
railway up
```

## DigitalOcean App Platform

1. Go to https://cloud.digitalocean.com/apps
2. Click "Create App"
3. Connect your GitHub repository
4. Configure build and run commands
5. Deploy

## Environment Variables

For production deployments, set:
- `PORT` - Server port (usually set automatically)
- `NODE_ENV=production` - Production mode

## Post-Deployment

After deploying:
1. Test all features (individual prediction, batch upload, downloads)
2. Monitor logs for errors
3. Set up custom domain if needed
4. Enable HTTPS (usually automatic on most platforms)

## Troubleshooting

**File uploads not working**: Some serverless platforms have limitations. Heroku is recommended for full file system support.

**Build failures**: Check that all dependencies are in `package.json` and node version is compatible (v14+).

**404 errors**: Ensure `vercel.json` and `api/index.js` are committed to your repository.
