#!/bin/bash

echo "🚀 Deploying to Vercel"
echo "===================="
echo ""

# Check if vercel CLI is installed
if ! command -v vercel &> /dev/null; then
    echo "❌ Vercel CLI not found. Installing..."
    npm install -g vercel
fi

echo "✅ Vercel CLI ready"
echo ""

# Check if required files exist
if [ ! -f "vercel.json" ]; then
    echo "❌ vercel.json not found!"
    exit 1
fi

if [ ! -f "api/index.js" ]; then
    echo "❌ api/index.js not found!"
    exit 1
fi

echo "✅ Configuration files found"
echo ""

# Deploy to Vercel
echo "📦 Deploying to Vercel..."
echo ""

vercel --prod

echo ""
echo "✅ Deployment complete!"
echo ""
echo "Your app should now be live on Vercel."
echo "Check the URL provided above."
