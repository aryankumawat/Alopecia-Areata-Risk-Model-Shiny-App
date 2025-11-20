# Getting Started - Alopecia Areata Risk Calculator

Welcome! Your R Shiny application has been successfully converted to a Node.js web application.

## 📋 What Was Created

Your new Node.js application includes:

### Backend (Node.js/Express)
- `server.js` - Express server with API endpoints
- `package.json` - Node.js dependencies and scripts

### Frontend (HTML/CSS/JavaScript)
- `public/index.html` - Main application interface
- `public/styles.css` - Modern, responsive styling
- `public/app.js` - Client-side JavaScript logic

### Configuration & Documentation
- `README_NODEJS.md` - Complete project documentation
- `DEPLOYMENT_GUIDE.md` - Step-by-step deployment instructions
- `MODEL_INTEGRATION.md` - Guide for integrating the actual R model
- `GETTING_STARTED.md` - This file
- `.gitignore` - Git ignore rules
- `start.sh` - Quick start script
- `test.js` - Setup verification script

## 🚀 Quick Start (3 Steps)

### Step 1: Install Dependencies
```bash
npm install
```

### Step 2: Start the Server
```bash
npm start
```

Or use the startup script:
```bash
./start.sh
```

### Step 3: Open Your Browser
Navigate to: http://localhost:3000

That's it! Your application is now running.

## 📱 Features

Your converted application includes all the features from the original Shiny app:

### ✅ Step 1: Demographic Information
- Enter age and gender
- Upload CSV files with sample data
- Select specific rows from uploaded data

### ✅ Step 2: Gene Expression & Prediction
- Input gene expression levels for 21 genes
- Get instant risk predictions
- View prediction history
- Download results as CSV

### ✅ Batch Prediction
- Upload CSV files with multiple samples
- Process all samples at once
- Download batch results

### ✅ Educational Content
- GSE68801 dataset overview
- Information about Alopecia Areata
- Links to research and resources

## 🎨 User Interface

The new interface features:
- Modern, responsive design
- Tab-based navigation
- Color-coded risk levels
- Mobile-friendly layout
- Professional styling with gradient themes

## 📊 Sample Data

Use the sample CSV file from the original project:
```
Alopecia-Areata-Risk-Model-Shiny-App/Shiny App Sample Input Data.csv
```

This file contains 100 sample records with:
- 21 gene expression values
- Age and gender information
- Ready to test both individual and batch predictions

## ⚠️ Important Note: Prediction Model

The current implementation uses a **simplified placeholder prediction function**. 

To use your actual trained R model (`final_lasso_model.rds`), see:
- `MODEL_INTEGRATION.md` - Detailed integration options

**Recommended approach:** Use R Plumber API (easiest) or Python microservice.

## 🌐 Deployment

Ready to deploy? See `DEPLOYMENT_GUIDE.md` for detailed instructions.

**Quick recommendations:**
- **Easiest:** Heroku (free tier available)
- **Modern:** Vercel or Railway
- **Flexible:** AWS, Google Cloud, or Azure

### Deploy to Heroku (5 minutes):
```bash
heroku login
heroku create your-app-name
git add .
git commit -m "Deploy to Heroku"
git push heroku main
heroku open
```

## 🔧 Development

### File Structure
```
├── server.js              # Backend API server
├── package.json           # Dependencies
├── public/                # Frontend files
│   ├── index.html        # Main page
│   ├── styles.css        # Styling
│   └── app.js            # Client logic
├── uploads/              # Temporary file storage
└── docs/                 # Documentation
```

### API Endpoints

**POST /api/predict**
- Predict risk for single sample
- Input: JSON with age, gender, and gene expression
- Output: Prediction, probability, risk level

**POST /api/batch-predict**
- Predict risk for multiple samples
- Input: CSV file
- Output: Array of predictions

**GET /api/genes**
- Get list of genes and symbols
- Output: Gene IDs and mappings

### Development Mode

For auto-reload during development:
```bash
npm install -g nodemon
npm run dev
```

## 🧪 Testing

Verify your setup:
```bash
node test.js
```

Test the API:
```bash
# Start server in one terminal
npm start

# In another terminal, test the API
curl -X POST http://localhost:3000/api/predict \
  -H "Content-Type: application/json" \
  -d '{"age":30,"gender":1,"205758_at":5.5,...}'
```

## 📚 Next Steps

1. **Install dependencies:** `npm install`
2. **Test locally:** `npm start`
3. **Integrate actual model:** See `MODEL_INTEGRATION.md`
4. **Deploy:** See `DEPLOYMENT_GUIDE.md`
5. **Customize:** Modify styling, add features, etc.

## 🆚 Comparison: R Shiny vs Node.js

| Feature | R Shiny | Node.js |
|---------|---------|---------|
| Language | R | JavaScript |
| Deployment | Shiny Server, shinyapps.io | Any Node.js host |
| Scalability | Limited | Excellent |
| Cost | Can be expensive | Many free options |
| Performance | Good | Excellent |
| Ecosystem | R packages | npm packages |

## 🔐 Security Considerations

Before deploying to production:

1. Add rate limiting
2. Implement input validation
3. Use HTTPS
4. Add authentication if needed
5. Set up monitoring
6. Keep dependencies updated

## 💡 Tips

- **CSV Format:** Ensure your CSV has the correct column names
- **Gene Values:** Typical range is 0-15 (normalized expression)
- **Browser Support:** Works on all modern browsers
- **Mobile:** Fully responsive design

## 🐛 Troubleshooting

**Port already in use:**
```bash
# Change port in server.js or use environment variable
PORT=3001 npm start
```

**Dependencies not installing:**
```bash
# Clear cache and reinstall
rm -rf node_modules package-lock.json
npm install
```

**File upload not working:**
```bash
# Ensure uploads directory exists
mkdir uploads
```

## 📞 Support

- Check `README_NODEJS.md` for detailed documentation
- See `DEPLOYMENT_GUIDE.md` for deployment help
- Review `MODEL_INTEGRATION.md` for model integration

## 🎉 Success!

Your application is ready to use! Start the server and begin making predictions.

```bash
npm start
```

Then visit: http://localhost:3000

Happy predicting! 🧬
