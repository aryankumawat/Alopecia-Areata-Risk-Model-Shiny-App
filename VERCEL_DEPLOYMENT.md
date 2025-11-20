# Vercel Deployment Guide

## Quick Fix for Your 404 Error

The 404 error happens because Vercel needs specific configuration. I've created the necessary files:

### Files Created:
1. `vercel.json` - Vercel configuration
2. `api/index.js` - Serverless function entry point

## Deploy to Vercel (3 Steps)

### Option 1: Using Vercel CLI (Recommended)

1. **Install Vercel CLI:**
   ```bash
   npm install -g vercel
   ```

2. **Deploy:**
   ```bash
   vercel
   ```

3. **Follow the prompts:**
   - Link to existing project or create new
   - Confirm settings
   - Deploy!

### Option 2: Using Vercel Dashboard

1. **Go to:** https://vercel.com/new

2. **Import your Git repository:**
   - Connect GitHub/GitLab/Bitbucket
   - Select your repository

3. **Configure:**
   - Framework Preset: Other
   - Build Command: (leave empty)
   - Output Directory: (leave empty)
   - Install Command: `npm install`

4. **Deploy:**
   - Click "Deploy"
   - Wait for deployment to complete

## Troubleshooting

### Still Getting 404?

1. **Check the build logs:**
   - Go to your Vercel dashboard
   - Click on your deployment
   - Check the "Build Logs" tab

2. **Verify files are committed:**
   ```bash
   git add vercel.json api/index.js
   git commit -m "Add Vercel configuration"
   git push
   ```

3. **Redeploy:**
   ```bash
   vercel --prod
   ```

### File Upload Issues

Vercel has a read-only filesystem except for `/tmp`. The configuration already handles this, but if you have issues:

- File uploads work but files are temporary
- They're deleted after the function completes
- This is normal for serverless environments

### Environment Variables

If you need environment variables:

1. Go to Vercel Dashboard → Your Project → Settings → Environment Variables
2. Add any needed variables
3. Redeploy

## Alternative: Use Heroku Instead

If Vercel continues to have issues, Heroku is easier for Express apps:

```bash
# Install Heroku CLI
brew install heroku/brew/heroku

# Login
heroku login

# Create app
heroku create your-app-name

# Deploy
git push heroku main

# Open
heroku open
```

## Vercel vs Heroku

| Feature | Vercel | Heroku |
|---------|--------|--------|
| Setup | Medium | Easy |
| Express Support | Serverless | Native |
| File Uploads | Limited | Full |
| Free Tier | Yes | Yes |
| Best For | Static + API | Full apps |

## Recommended: Deploy to Heroku

For this Express app, Heroku is actually easier:

```bash
heroku create alopecia-calculator
git push heroku main
```

That's it! Your app will be live.

## Current Status

✅ Vercel configuration created
✅ Serverless function ready
✅ Static files configured

**Next:** Commit and push these changes, then redeploy to Vercel.

## Need More Help?

1. Check Vercel build logs
2. Try Heroku instead (easier for Express)
3. Review the main DEPLOYMENT_GUIDE.md for other options
