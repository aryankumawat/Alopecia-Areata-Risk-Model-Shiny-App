# Alopecia Areata Risk Model & Shiny App

A comprehensive machine learning project for predicting Alopecia Areata risk using gene expression data from the GSE68801 dataset.

## 📊 Project Overview

This project analyzes gene expression profiles from scalp skin punch biopsies of patients with Alopecia Areata (AA) and healthy controls to develop a predictive risk model. The analysis includes differential expression analysis, LASSO feature selection, and multiple machine learning algorithms.

## 🧬 Dataset Information

- **Dataset**: GSE68801 (NCBI GEO)
- **Platform**: Affymetrix Human Genome U133 Plus 2.0 Array
- **Samples**: 122 total (36 controls, 86 patients)
- **Genes**: 54,675 gene features analyzed
- **Source**: Ali Jabbari et al. - Molecular signatures define alopecia areata subtypes

## 📁 Repository Structure

```
├── Git Final.Rmd                    # Complete R Markdown analysis
├── Shiny App Sample Input Data.csv  # Sample data for testing the app
├── shiny_app_coding/                # Shiny web application
│   ├── app.R                        # Main Shiny app file
│   ├── final_lasso_model.rds        # Trained SVM model
│   └── X_with_gender_age.rds        # Feature matrix with demographics
├── .gitignore                       # Git ignore file
└── README.md                        # This file
```

## 🔬 Analysis Components

### 1. Exploratory Data Analysis (EDA)
- Data quality assessment
- Sample distribution analysis
- Expression level visualization

### 2. Differential Expression Analysis
- Linear modeling using limma
- Empirical Bayes moderation
- FDR correction for multiple testing

### 3. Feature Selection
- LASSO regression for gene selection
- Cross-validation for optimal lambda
- 21 most predictive genes selected

### 4. Machine Learning Models
- **Random Forest**: Ensemble learning
- **GLMNET**: Elastic Net regularization
- **SVM**: Support Vector Machine with radial kernel
- **kNN**: k-Nearest Neighbors

### 5. Model Evaluation
- Cross-validation performance
- ROC analysis and AUC calculation
- Accuracy, F1-score, and other metrics
- Overfitting verification

## 🚀 Shiny Web Application

### Features
- **Individual Prediction**: Enter age, gender, and gene expression levels
- **Batch Prediction**: Upload CSV files for multiple predictions
- **Prediction History**: Track and download results
- **Educational Content**: Learn about Alopecia Areata
- **Data Overview**: GSE68801 dataset information

### How to Run
```r
# Navigate to the shiny app directory
cd shiny_app_coding

# Run the app
R -e "library(shiny); runApp('app.R', host='127.0.0.1', port=3838)"
```

Then open your browser to: http://127.0.0.1:3838

### Sample Input Data
A sample CSV file (`Shiny App Sample Input Data.csv`) is provided to help users understand the required data format and test the application:

**File Structure:**
- **21 Gene Expression Columns**: Probe IDs for the selected genes (e.g., `205758_at`, `241014_at`, etc.)
- **Gender Column**: `gender` (0 = Female, 1 = Male)
- **Age Column**: `age` (numeric values)

**How to Use the Sample Data:**
1. **Individual Testing**: Use any row from the sample file to populate the Shiny app inputs
2. **Batch Prediction**: Upload the entire sample file to test batch prediction functionality
3. **Format Reference**: Use this file as a template for your own data

**Sample Data Details:**
- Contains 100 sample records with realistic gene expression values
- Values are normalized and scaled appropriately for the model
- Includes diverse age ranges (26-48 years) and both genders
- Ready to use for immediate testing of the application

## 📈 Key Results

- **Best Model**: Random Forest (AUC: 0.92)
- **Selected Features**: 21 genes + age + gender
- **Cross-validation**: 10-fold CV with consistent performance
- **Gene Mapping**: Probe IDs mapped to gene symbols

## 🛠️ Required R Packages

```r
# Core analysis packages
library(GEOquery)
library(Biobase)
library(limma)
library(ggplot2)
library(dplyr)
library(caret)

# Machine learning packages
library(randomForest)
library(glmnet)
library(e1071)

# Shiny app packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

# Additional packages
library(pROC)
library(AnnotationDbi)
library(hgu133plus2.db)
```

## 📋 Usage Instructions

### Running the Analysis
1. Open `Git Final.Rmd` in RStudio
2. Install required packages if needed
3. Run all code chunks sequentially
4. View results and visualizations

### Using the Shiny App
1. Start the Shiny app (see instructions above)
2. Enter demographic information (age, gender)
3. Input gene expression levels (defaults provided)
4. Click "Predict Risk" for results
5. Use "Batch Prediction" for CSV uploads

## 🔧 Technical Notes

- **Data Processing**: GCRMA normalization
- **Feature Selection**: LASSO with 10-fold CV
- **Model Training**: Caret framework with cross-validation
- **Gene Annotation**: hgu133plus2.db for probe-to-gene mapping
- **Visualization**: ggplot2 with professional themes

## 📚 References

- Jabbari, A., et al. "Molecular signatures define alopecia areata subtypes and transcriptional biomarkers." *eLife* (2016)
- GSE68801 dataset: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE68801
- Alopecia Areata Foundation: https://aaaf.org.au/

