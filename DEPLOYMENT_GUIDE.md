# Deployment Guide - Alopecia Areata Risk Calculator

This guide covers multiple deployment options for your Node.js application.

## Quick Start (Local Development)

1. **Install dependencies:**
   ```bash
   npm install
   ```

2. **Start the server:**
   ```bash
   npm start
   ```
   
   Or use the startup script:
   ```bash
   ./start.sh
   ```

3. **Access the application:**
   Open your browser to http://localhost:3000

## Deployment Options

### 1. Heroku (Easiest)

Heroku is a cloud platform that makes deployment simple.

**Steps:**

1. Install Heroku CLI:
   ```bash
   brew install heroku/brew/heroku  # macOS
   ```

2. Login to Heroku:
   ```bash
   heroku login
   ```

3. Create a new Heroku app:
   ```bash
   heroku create alopecia-risk-calculator
   ```

4. Deploy:
   ```bash
   git add .
   git commit -m "Deploy to Heroku"
   git push heroku main
   ```

5. Open your app:
   ```bash
   heroku open
   ```

**Cost:** Free tier available (with limitations)

### 2. Vercel (Recommended for Static + Serverless)

Vercel is great for modern web applications.

**Steps:**

1. Install Vercel CLI:
   ```bash
   npm install -g vercel
   ```

2. Deploy:
   ```bash
   vercel
   ```

3. Follow the prompts and your app will be live!

**Cost:** Free tier available

### 3. Railway (Modern Alternative)

Railway is a modern deployment platform.

**Steps:**

1. Visit https://railway.app
2. Sign up with GitHub
3. Click "New Project" → "Deploy from GitHub repo"
4. Select your repository
5. Railway will auto-detect Node.js and deploy

**Cost:** Free tier with $5 credit/month

### 4. DigitalOcean App Platform

**Steps:**

1. Visit https://cloud.digitalocean.com/apps
2. Click "Create App"
3. Connect your GitHub repository
4. Configure:
   - Build Command: `npm install`
   - Run Command: `npm start`
5. Deploy

**Cost:** Starting at $5/month

### 5. AWS (Most Flexible)

#### Option A: AWS Elastic Beanstalk

1. Install AWS CLI and EB CLI
2. Initialize:
   ```bash
   eb init
   ```
3. Create environment:
   ```bash
   eb create production
   ```
4. Deploy:
   ```bash
   eb deploy
   ```

#### Option B: AWS EC2

1. Launch an EC2 instance (Ubuntu recommended)
2. SSH into the instance
3. Install Node.js:
   ```bash
   curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
   sudo apt-get install -y nodejs
   ```
4. Clone your repository
5. Install dependencies and start:
   ```bash
   npm install
   npm start
   ```
6. Use PM2 for process management:
   ```bash
   npm install -g pm2
   pm2 start server.js
   pm2 startup
   pm2 save
   ```

**Cost:** Starting at ~$5/month for t2.micro

### 6. Google Cloud Platform

#### Cloud Run (Containerized)

1. Create a Dockerfile:
   ```dockerfile
   FROM node:18
   WORKDIR /app
   COPY package*.json ./
   RUN npm install
   COPY . .
   EXPOSE 3000
   CMD ["npm", "start"]
   ```

2. Build and deploy:
   ```bash
   gcloud run deploy alopecia-calculator --source .
   ```

**Cost:** Pay per use, free tier available

### 7. Azure

#### Azure App Service

1. Install Azure CLI
2. Login:
   ```bash
   az login
   ```
3. Create and deploy:
   ```bash
   az webapp up --name alopecia-calculator --runtime "NODE:18-lts"
   ```

**Cost:** Starting at ~$13/month

## Environment Configuration

For production, create environment variables:

```bash
# .env file
PORT=3000
NODE_ENV=production
```

## Domain Setup

After deployment, you can add a custom domain:

1. **Heroku:**
   ```bash
   heroku domains:add www.yourdomain.com
   ```

2. **Vercel:**
   - Go to project settings → Domains
   - Add your custom domain

3. **Others:**
   - Update DNS records to point to your server IP
   - Configure SSL certificate (Let's Encrypt is free)

## SSL/HTTPS

Most platforms (Heroku, Vercel, Railway) provide free SSL automatically.

For manual setup:
```bash
# Using Let's Encrypt with Certbot
sudo certbot --nginx -d yourdomain.com
```

## Monitoring & Logs

### Heroku:
```bash
heroku logs --tail
```

### PM2 (for VPS):
```bash
pm2 logs
pm2 monit
```

### Cloud Platforms:
Use their built-in monitoring dashboards

## Performance Optimization

1. **Enable compression:**
   ```javascript
   const compression = require('compression');
   app.use(compression());
   ```

2. **Add caching:**
   ```javascript
   app.use(express.static('public', { maxAge: '1d' }));
   ```

3. **Use a CDN** for static assets (Cloudflare is free)

## Security Checklist

- [ ] Use HTTPS in production
- [ ] Set secure headers (use helmet.js)
- [ ] Add rate limiting
- [ ] Validate all inputs
- [ ] Keep dependencies updated
- [ ] Use environment variables for secrets
- [ ] Enable CORS properly
- [ ] Add authentication if needed

## Recommended: Heroku Deployment

For the easiest deployment, I recommend Heroku:

```bash
# One-time setup
heroku login
heroku create alopecia-risk-calculator

# Deploy
git add .
git commit -m "Initial deployment"
git push heroku main

# View logs
heroku logs --tail

# Open app
heroku open
```

Your app will be live at: https://alopecia-risk-calculator.herokuapp.com

## Troubleshooting

**Port Issues:**
Make sure your server uses `process.env.PORT`:
```javascript
const PORT = process.env.PORT || 3000;
```

**Build Failures:**
Check that all dependencies are in `package.json`, not `devDependencies`.

**File Upload Issues:**
Ensure the `uploads` directory exists and has write permissions.

## Next Steps

1. Choose a deployment platform
2. Follow the specific steps above
3. Test your deployed application
4. Set up monitoring
5. Configure a custom domain (optional)

Need help? Check the platform-specific documentation or open an issue!
