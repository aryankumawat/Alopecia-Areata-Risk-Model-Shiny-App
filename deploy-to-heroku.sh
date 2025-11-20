#!/bin/bash

echo "🚀 Deploying to Heroku - The Easy Way!"
echo "========================================"
echo ""

# Check if Heroku CLI is installed
if ! command -v heroku &> /dev/null; then
    echo "❌ Heroku CLI not found."
    echo ""
    echo "Install it with:"
    echo "  macOS: brew install heroku/brew/heroku"
    echo "  Or visit: https://devcenter.heroku.com/articles/heroku-cli"
    echo ""
    exit 1
fi

echo "✅ Heroku CLI found"
echo ""

# Check if logged in
if ! heroku auth:whoami &> /dev/null; then
    echo "🔐 Please login to Heroku..."
    heroku login
fi

echo "✅ Logged in to Heroku"
echo ""

# Create app
echo "📦 Creating Heroku app..."
APP_NAME="alopecia-calculator-$(date +%s)"
heroku create $APP_NAME

echo ""
echo "✅ App created: $APP_NAME"
echo ""

# Add, commit, and push
echo "📤 Deploying your code..."
git add .
git commit -m "Deploy to Heroku" || echo "No changes to commit"
git push heroku main || git push heroku master

echo ""
echo "✅ Deployment complete!"
echo ""

# Open the app
echo "🌐 Opening your app..."
heroku open

echo ""
echo "🎉 Success! Your app is live!"
echo ""
echo "Useful commands:"
echo "  heroku logs --tail    (view logs)"
echo "  heroku open           (open app)"
echo "  heroku restart        (restart app)"
