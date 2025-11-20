#!/bin/bash

echo "🚀 Complete Setup and Deployment Script"
echo "========================================"
echo ""

# Check if we're in the right directory
if [ ! -f "server.js" ]; then
    echo "❌ Error: server.js not found!"
    echo "Please run this script from your project directory."
    exit 1
fi

echo "✅ Found project files"
echo ""

# Check if git is initialized
if [ ! -d ".git" ]; then
    echo "📦 Initializing git repository..."
    git init
    echo "✅ Git initialized"
else
    echo "✅ Git already initialized"
fi

echo ""

# Add all files
echo "📝 Adding all files to git..."
git add .

# Commit
echo "💾 Committing files..."
git commit -m "Initial commit with all configurations" || echo "No changes to commit"

echo ""
echo "✅ Git setup complete!"
echo ""

# Ask user which platform
echo "Choose deployment platform:"
echo "1) Heroku (Recommended - easier, full features)"
echo "2) Vercel (Serverless, may have limitations)"
echo ""
read -p "Enter choice (1 or 2): " choice

if [ "$choice" = "1" ]; then
    echo ""
    echo "🚀 Deploying to Heroku..."
    echo ""
    
    # Check if Heroku CLI is installed
    if ! command -v heroku &> /dev/null; then
        echo "❌ Heroku CLI not found."
        echo ""
        echo "Install it with:"
        echo "  brew install heroku/brew/heroku"
        echo ""
        echo "Then run this script again."
        exit 1
    fi
    
    echo "✅ Heroku CLI found"
    echo ""
    
    # Login
    echo "🔐 Logging in to Heroku..."
    heroku login
    
    # Create app
    echo ""
    echo "📦 Creating Heroku app..."
    APP_NAME="alopecia-calculator-$(date +%s)"
    heroku create $APP_NAME
    
    # Deploy
    echo ""
    echo "🚀 Deploying to Heroku..."
    git push heroku main || git push heroku master
    
    # Open
    echo ""
    echo "🌐 Opening your app..."
    heroku open
    
    echo ""
    echo "🎉 Success! Your app is live on Heroku!"
    echo ""
    echo "App URL: https://$APP_NAME.herokuapp.com"
    
elif [ "$choice" = "2" ]; then
    echo ""
    echo "🚀 Deploying to Vercel..."
    echo ""
    
    # Check if Vercel CLI is installed
    if ! command -v vercel &> /dev/null; then
        echo "📦 Installing Vercel CLI..."
        npm install -g vercel
    fi
    
    echo "✅ Vercel CLI ready"
    echo ""
    
    # Deploy
    echo "🚀 Deploying to Vercel..."
    vercel --prod
    
    echo ""
    echo "🎉 Deployment complete!"
    echo "Check the URL provided above."
    
else
    echo ""
    echo "❌ Invalid choice. Please run the script again and choose 1 or 2."
    exit 1
fi

echo ""
echo "📚 Useful commands:"
if [ "$choice" = "1" ]; then
    echo "  heroku logs --tail    (view logs)"
    echo "  heroku open           (open app)"
    echo "  heroku restart        (restart app)"
else
    echo "  vercel --prod         (redeploy)"
    echo "  vercel logs           (view logs)"
fi

echo ""
echo "✅ All done!"
