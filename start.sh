#!/bin/bash

echo "🧬 Alopecia Areata Risk Calculator - Node.js Setup"
echo "=================================================="
echo ""

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "📦 Installing dependencies..."
    npm install
    echo ""
fi

# Create uploads directory if it doesn't exist
if [ ! -d "uploads" ]; then
    echo "📁 Creating uploads directory..."
    mkdir uploads
    echo ""
fi

echo "✅ Setup complete!"
echo ""
echo "🚀 Starting server..."
echo "   Access the application at: http://localhost:3000"
echo ""
echo "   Press Ctrl+C to stop the server"
echo ""

npm start
