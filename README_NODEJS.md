# Alopecia Areata Risk Calculator - Node.js Web Application

A modern web application for predicting Alopecia Areata risk using gene expression data. Converted from R Shiny to Node.js/Express.

## Features

- **Individual Prediction**: Enter demographic info and gene expression levels for single predictions
- **Batch Prediction**: Upload CSV files for multiple predictions at once
- **Prediction History**: Track and download all predictions
- **Educational Content**: Learn about Alopecia Areata and the GSE68801 dataset
- **Responsive Design**: Works on desktop and mobile devices

## Prerequisites

- Node.js (v14 or higher)
- npm (comes with Node.js)

## Installation

1. Install dependencies:
```bash
npm install
```

2. Create the uploads directory:
```bash
mkdir uploads
```

## Running the Application

### Development Mode (with auto-reload):
```bash
npm run dev
```

### Production Mode:
```bash
npm start
```

The application will be available at: http://localhost:3000

## Project Structure

```
├── server.js              # Express server and API endpoints
├── package.json           # Node.js dependencies
├── public/                # Frontend files
│   ├── index.html        # Main HTML page
│   ├── styles.css        # Styling
│   └── app.js            # Frontend JavaScript
├── uploads/              # Temporary CSV upload directory
└── README_NODEJS.md      # This file
```

## API Endpoints

### POST /api/predict
Predict risk for a single sample.

**Request Body:**
```json
{
  "age": 30,
  "gender": 1,
  "205758_at": 5.5,
  "241014_at": 8.2,
  ...
}
```

**Response:**
```json
{
  "prediction": "Patient",
  "probability": "0.7234",
  "riskLevel": "High"
}
```

### POST /api/batch-predict
Predict risk for multiple samples from CSV file.

**Request:** Multipart form data with CSV file

**Response:** Array of predictions with all input features

### GET /api/genes
Get list of genes and their symbols.

**Response:**
```json
{
  "genes": ["205758_at", "241014_at", ...],
  "symbols": {"205758_at": "CD8A", ...}
}
```

## CSV File Format

Your CSV file should include the following columns:
- 21 gene expression columns (probe IDs)
- `age` (numeric)
- `gender` (0 = Female, 1 = Male)

Example: See `Alopecia-Areata-Risk-Model-Shiny-App/Shiny App Sample Input Data.csv`

## Deployment

### Deploy to Heroku:
```bash
heroku create your-app-name
git push heroku main
```

### Deploy to Vercel:
```bash
vercel
```

### Deploy to AWS/Azure/GCP:
Use their respective Node.js deployment guides.

## Important Notes

⚠️ **Model Implementation**: The current prediction function is a simplified placeholder. For production use, you need to:

1. Convert the R model (`final_lasso_model.rds`) to a format usable in Node.js
2. Options include:
   - Use ONNX format and onnxruntime-node
   - Create a Python microservice with the R model and call it from Node.js
   - Reimplement the model in JavaScript using TensorFlow.js
   - Use a REST API wrapper around the R model

## Converting the R Model

To use the actual trained model, you have several options:

### Option 1: Python Microservice
Create a separate Python service that loads the R model and expose it via REST API.

### Option 2: ONNX
Convert the R model to ONNX format and use it in Node.js:
```bash
npm install onnxruntime-node
```

### Option 3: Model Reimplementation
Retrain the model using a JavaScript-compatible library like TensorFlow.js.

## Environment Variables

Create a `.env` file for configuration:
```
PORT=3000
NODE_ENV=production
```

## Security Considerations

- Add rate limiting for API endpoints
- Implement file size limits for CSV uploads
- Add input validation and sanitization
- Use HTTPS in production
- Add authentication if needed

## License

MIT

## Support

For issues or questions, please open an issue on the repository.
