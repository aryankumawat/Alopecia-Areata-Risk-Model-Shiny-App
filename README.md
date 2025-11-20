# 🧬 Alopecia Areata Smart Risk Calculator

A web application that predicts Alopecia Areata risk using gene expression data and machine learning. This project was originally built in R Shiny and has been converted to Node.js/Express for improved performance and easier deployment.

## Getting Started

To run this application locally:

```bash
npm install
npm start
```

Then open your browser and navigate to `http://localhost:3000`

## Features

- Individual risk predictions based on demographics and gene expression data
- Batch processing for multiple samples via CSV upload
- Prediction history tracking with download capability
- Educational information about Alopecia Areata and the GSE68801 dataset
- Responsive design that works on desktop and mobile devices

## About

This application predicts Alopecia Areata risk using:
- Patient demographics (age and gender)
- Expression levels of 21 genes identified through LASSO feature selection
- A machine learning model trained on the GSE68801 dataset (122 samples)

## Use Cases

This tool can be used for:
- Research into gene expression patterns in Alopecia Areata
- Clinical assessment of patient risk based on biomarkers
- Educational purposes to understand the molecular basis of the disease
- As a template for similar medical prediction applications

## Project Structure

```
├── server.js              # Express server with API endpoints
├── package.json           # Node.js dependencies
├── public/                # Frontend files
│   ├── index.html        # Main application interface
│   ├── styles.css        # Responsive styling
│   └── app.js            # Client-side logic
├── uploads/              # Temporary file storage
└── docs/                 # Comprehensive documentation
```

## Technology Stack

**Backend:**
- Node.js + Express.js
- Multer (file uploads)
- csv-parser (CSV processing)

**Frontend:**
- HTML5 + CSS3
- Vanilla JavaScript
- Fetch API

## Documentation

Additional documentation:
- [DEPLOYMENT.md](DEPLOYMENT.md) - Deployment instructions
- [MODEL_INTEGRATION.md](MODEL_INTEGRATION.md) - How to integrate the actual R model

## Deployment

This application can be deployed to various platforms including Heroku, Vercel, Railway, and others. See [DEPLOYMENT.md](DEPLOYMENT.md) for detailed instructions.

## Testing

You can verify your setup by running `node test.js`. Sample data is available in `Alopecia-Areata-Risk-Model-Shiny-App/Shiny App Sample Input Data.csv` with 100 test records.

## Data Format

CSV files should contain 21 gene expression columns (probe IDs), plus age (0-100) and gender (0=Female, 1=Male) columns.

## Model Details

The prediction model was trained on the GSE68801 dataset from NCBI GEO, which contains 122 samples (36 controls, 86 patients) from Affymetrix Human Genome U133 Plus 2.0 Array. LASSO regression was used to select 21 predictive genes, and a Support Vector Machine (SVM) was trained using the caret package in R with cross-validation.

## Note on Model Implementation

The current version uses a simplified prediction function for demonstration purposes. To integrate the actual trained R model, see [MODEL_INTEGRATION.md](MODEL_INTEGRATION.md). The original model files are located in `Alopecia-Areata-Risk-Model-Shiny-App/shiny_app_coding/`.



## Security Considerations

For production use, consider enabling HTTPS, adding rate limiting, implementing input validation, using environment variables for sensitive data, and adding authentication as needed.





## Contributing

Contributions are welcome. Potential areas for improvement include model integration, additional visualizations, database integration, user authentication, and mobile app development.

## License

This project is licensed under the MIT License.

## Acknowledgments

Thanks to the contributors of the GSE68801 dataset (Ali Jabbari et al.), NCBI GEO database, and the Alopecia Areata Foundation.







## References

- [GSE68801 Dataset](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE68801)
- [Alopecia Areata Information](https://aaaf.org.au/about-alopecia-areata/)
