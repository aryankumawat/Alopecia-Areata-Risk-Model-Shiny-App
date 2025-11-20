# Model Integration Guide

The current implementation uses a simplified prediction function. To integrate the actual trained R model, you have several options.

## Option 1: R Plumber API (Recommended)

Create an R API that serves the model predictions.

**api.R:**
```r
library(plumber)
library(caret)

model <- readRDS("final_lasso_model.rds")

#* @post /predict
function(req) {
    data <- jsonlite::fromJSON(req$postBody)
    df <- as.data.frame(data)
    
    pred_class <- predict(model, newdata = df, type = "raw")
    pred_prob <- predict(model, newdata = df, type = "prob")[, "Patient"]
    
    list(
        prediction = as.character(pred_class),
        probability = as.numeric(pred_prob)
    )
}
```

Run the API:
```r
pr <- plumb("api.R")
pr$run(port=5000)
```

Update `server.js` to call this API instead of the placeholder function.

## Option 2: Python Microservice

Use rpy2 to load the R model in Python and create a Flask API.

**requirements.txt:**
```
flask
rpy2
pandas
```

**model_service.py:**
```python
from flask import Flask, request, jsonify
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri

app = Flask(__name__)
pandas2ri.activate()

ro.r('library(caret)')
model = ro.r('readRDS("final_lasso_model.rds")')

@app.route('/predict', methods=['POST'])
def predict():
    data = request.json
    df = pd.DataFrame([data])
    r_df = pandas2ri.py2rpy(df)
    
    pred_class = ro.r.predict(model, newdata=r_df, type='raw')[0]
    pred_prob = ro.r.predict(model, newdata=r_df, type='prob')
    
    return jsonify({
        'prediction': str(pred_class),
        'probability': float(pred_prob[1])
    })

if __name__ == '__main__':
    app.run(port=5000)
```

## Option 3: TensorFlow.js

Retrain the model using TensorFlow.js for native JavaScript support. This requires retraining with your data but eliminates the need for a separate service.

## Deployment

When deploying with a separate model service:

1. Deploy the model service (R Plumber or Python Flask)
2. Update the Node.js app to call the model service API
3. Deploy the Node.js app
4. Ensure both services can communicate (same network or public endpoints)

## Model Files

The original R model files are located in:
- `Alopecia-Areata-Risk-Model-Shiny-App/shiny_app_coding/final_lasso_model.rds`
- `Alopecia-Areata-Risk-Model-Shiny-App/shiny_app_coding/X_with_gender_age.rds`
