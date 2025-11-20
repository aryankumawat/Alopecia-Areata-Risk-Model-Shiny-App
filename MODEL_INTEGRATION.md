# Model Integration Guide

The current implementation uses a simplified prediction function. To use the actual trained R model, you have several options:

## Option 1: Python Microservice (Recommended)

Create a separate Python service that loads the R model and exposes it via REST API.

### Step 1: Create Python Service

**requirements.txt:**
```
flask==3.0.0
rpy2==3.5.14
pandas==2.1.0
numpy==1.24.3
```

**model_service.py:**
```python
from flask import Flask, request, jsonify
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
import pandas as pd

app = Flask(__name__)
pandas2ri.activate()

# Load R model
ro.r('library(caret)')
model = ro.r('readRDS("final_lasso_model.rds")')

@app.route('/predict', methods=['POST'])
def predict():
    data = request.json
    df = pd.DataFrame([data])
    
    # Convert to R dataframe
    r_df = pandas2ri.py2rpy(df)
    
    # Make prediction
    pred_class = ro.r.predict(model, newdata=r_df, type='raw')[0]
    pred_prob = ro.r.predict(model, newdata=r_df, type='prob')
    
    return jsonify({
        'prediction': str(pred_class),
        'probability': float(pred_prob[1])
    })

if __name__ == '__main__':
    app.run(port=5000)
```

### Step 2: Update Node.js Server

```javascript
// In server.js
async function predictRisk(features) {
    const response = await fetch('http://localhost:5000/predict', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(features)
    });
    return await response.json();
}
```

### Step 3: Deploy Both Services

**Docker Compose:**
```yaml
version: '3.8'
services:
  web:
    build: .
    ports:
      - "3000:3000"
    depends_on:
      - model-service
  
  model-service:
    build: ./model-service
    ports:
      - "5000:5000"
```

## Option 2: ONNX Format

Convert the R model to ONNX and use it directly in Node.js.

### Step 1: Convert Model in R

```r
library(caret)
library(onnx)

# Load model
model <- readRDS("final_lasso_model.rds")

# Convert to ONNX (requires additional packages)
# This is complex and may require custom implementation
```

### Step 2: Use in Node.js

```javascript
const onnx = require('onnxruntime-node');

async function loadModel() {
    const session = await onnx.InferenceSession.create('model.onnx');
    return session;
}

async function predict(session, features) {
    const tensor = new onnx.Tensor('float32', features, [1, features.length]);
    const results = await session.run({ input: tensor });
    return results.output.data;
}
```

## Option 3: TensorFlow.js

Retrain the model using TensorFlow.js for native JavaScript support.

### Step 1: Prepare Data

```javascript
const tf = require('@tensorflow/tfjs-node');

// Load and prepare your data
const data = loadData();
const xs = tf.tensor2d(data.features);
const ys = tf.tensor2d(data.labels);
```

### Step 2: Train Model

```javascript
const model = tf.sequential({
    layers: [
        tf.layers.dense({ inputShape: [23], units: 64, activation: 'relu' }),
        tf.layers.dropout({ rate: 0.5 }),
        tf.layers.dense({ units: 32, activation: 'relu' }),
        tf.layers.dense({ units: 1, activation: 'sigmoid' })
    ]
});

model.compile({
    optimizer: 'adam',
    loss: 'binaryCrossentropy',
    metrics: ['accuracy']
});

await model.fit(xs, ys, {
    epochs: 100,
    validationSplit: 0.2
});

await model.save('file://./model');
```

### Step 3: Use Model

```javascript
const model = await tf.loadLayersModel('file://./model/model.json');

function predict(features) {
    const input = tf.tensor2d([features]);
    const prediction = model.predict(input);
    return prediction.dataSync()[0];
}
```

## Option 4: R Plumber API

Create an R API using the plumber package.

### Step 1: Create R API

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

### Step 2: Run R API

```r
pr <- plumb("api.R")
pr$run(port=5000)
```

### Step 3: Call from Node.js

```javascript
async function predictRisk(features) {
    const response = await fetch('http://localhost:5000/predict', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(features)
    });
    return await response.json();
}
```

## Option 5: AWS Lambda with R

Deploy the R model as an AWS Lambda function.

### Step 1: Create Lambda Layer with R

Use the `r-lambda` project or create a custom layer.

### Step 2: Lambda Function

```r
handler <- function(event, context) {
    model <- readRDS("/opt/final_lasso_model.rds")
    data <- jsonlite::fromJSON(event$body)
    
    pred <- predict(model, newdata = as.data.frame(data), type = "raw")
    
    list(
        statusCode = 200,
        body = jsonlite::toJSON(list(prediction = as.character(pred)))
    )
}
```

### Step 3: Call from Node.js

```javascript
const AWS = require('aws-sdk');
const lambda = new AWS.Lambda();

async function predictRisk(features) {
    const params = {
        FunctionName: 'alopecia-predictor',
        Payload: JSON.stringify({ body: JSON.stringify(features) })
    };
    
    const result = await lambda.invoke(params).promise();
    return JSON.parse(result.Payload);
}
```

## Comparison

| Option | Pros | Cons | Difficulty |
|--------|------|------|------------|
| Python Microservice | Easy to implement, keeps R model | Requires two services | Medium |
| ONNX | Single service, fast | Complex conversion | Hard |
| TensorFlow.js | Native JS, no dependencies | Need to retrain | Medium |
| R Plumber | Direct R usage | Requires R runtime | Easy |
| AWS Lambda | Serverless, scalable | AWS-specific | Medium |

## Recommended Approach

For production, I recommend **Option 1 (Python Microservice)** or **Option 4 (R Plumber API)** because:

1. You can use the existing trained model
2. No need to retrain or convert
3. Relatively simple to implement
4. Can be containerized easily

## Quick Start with R Plumber

This is the fastest way to get your actual model working:

1. Install plumber in R:
```r
install.packages("plumber")
```

2. Create `api.R` (see Option 4 above)

3. Run the API:
```r
library(plumber)
pr <- plumb("api.R")
pr$run(port=5000)
```

4. Update `server.js` to call the R API instead of the placeholder function

5. Deploy both services together using Docker Compose

## Need Help?

If you need assistance implementing any of these options, please let me know which approach you'd like to use!
