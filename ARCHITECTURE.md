# Application Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                         Browser                              │
│  ┌───────────────────────────────────────────────────────┐  │
│  │              Frontend (HTML/CSS/JS)                    │  │
│  │  • Tab Navigation                                      │  │
│  │  • Form Inputs (Demographics + Genes)                 │  │
│  │  • CSV Upload Interface                               │  │
│  │  • Results Display                                    │  │
│  │  • History Table                                      │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ HTTP/HTTPS
                            │ (REST API)
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Node.js Server                            │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                 Express.js                             │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │  API Endpoints                                   │  │  │
│  │  │  • POST /api/predict                            │  │  │
│  │  │  • POST /api/batch-predict                      │  │  │
│  │  │  • GET  /api/genes                              │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │  Middleware                                      │  │  │
│  │  │  • Body Parser                                   │  │  │
│  │  │  • Multer (File Upload)                         │  │  │
│  │  │  • Static File Server                           │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │  Business Logic                                  │  │  │
│  │  │  • Prediction Function                          │  │  │
│  │  │  • CSV Parser                                   │  │  │
│  │  │  • Data Validation                              │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ (Future Integration)
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              Machine Learning Model Service                  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  Options:                                              │  │
│  │  • R Plumber API                                      │  │
│  │  • Python Flask/FastAPI                               │  │
│  │  • TensorFlow.js                                      │  │
│  │  • ONNX Runtime                                       │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Data Flow

### Individual Prediction Flow

```
User Input → Frontend Form → API Request → Server Processing → 
Prediction Function → Response → Display Results → Update History
```

**Detailed Steps:**

1. **User enters data:**
   - Age, gender
   - 21 gene expression values

2. **Frontend validation:**
   - Check required fields
   - Validate data types

3. **API call:**
   ```javascript
   POST /api/predict
   {
     "age": 30,
     "gender": 1,
     "205758_at": 5.5,
     ...
   }
   ```

4. **Server processing:**
   - Parse request body
   - Validate input
   - Call prediction function

5. **Prediction:**
   - Calculate risk score
   - Determine classification
   - Calculate probability

6. **Response:**
   ```json
   {
     "prediction": "Patient",
     "probability": "0.7234",
     "riskLevel": "High"
   }
   ```

7. **Display:**
   - Show prediction result
   - Update risk indicator
   - Add to history table

### Batch Prediction Flow

```
CSV Upload → File Processing → Parse Rows → 
Predict Each Row → Aggregate Results → Display Table → 
Download Option
```

**Detailed Steps:**

1. **User uploads CSV:**
   - Select file from computer
   - File sent to server

2. **Server receives file:**
   - Multer middleware handles upload
   - File saved to `uploads/` directory

3. **CSV parsing:**
   - Read file line by line
   - Parse each row into object

4. **Batch prediction:**
   - Loop through all rows
   - Call prediction function for each
   - Collect results

5. **Response:**
   ```json
   [
     { ...row1Data, "prediction": "Patient", "probability": 0.72 },
     { ...row2Data, "prediction": "Control", "probability": 0.31 },
     ...
   ]
   ```

6. **Display:**
   - Render results table
   - Enable download button

7. **Cleanup:**
   - Delete uploaded file from server

## Component Architecture

### Frontend Components

```
index.html
├── Header
│   └── Title with icon
├── Tab Navigation
│   ├── Demographic Info Tab
│   ├── Gene Expression Tab
│   ├── Batch Prediction Tab
│   ├── Data Overview Tab
│   └── About Alopecia Tab
└── Tab Content Areas
    ├── Forms
    ├── Input Fields
    ├── Results Display
    └── Data Tables
```

### Backend Components

```
server.js
├── Configuration
│   ├── Express setup
│   ├── Middleware
│   └── Constants (genes, symbols)
├── Routes
│   ├── GET  /
│   ├── POST /api/predict
│   ├── POST /api/batch-predict
│   └── GET  /api/genes
├── Business Logic
│   ├── predictRisk()
│   └── CSV processing
└── Server Initialization
```

## Technology Stack

### Frontend
- **HTML5** - Structure
- **CSS3** - Styling (with gradients, flexbox, grid)
- **Vanilla JavaScript** - Logic (no frameworks)
- **Fetch API** - HTTP requests

### Backend
- **Node.js** - Runtime environment
- **Express.js** - Web framework
- **Multer** - File upload handling
- **csv-parser** - CSV processing

### Future Integration
- **R/Python** - Machine learning model
- **Docker** - Containerization
- **PostgreSQL** - Database (optional)

## File Structure

```
project-root/
├── server.js                 # Main server file
├── package.json             # Dependencies
├── .gitignore              # Git ignore rules
├── start.sh                # Startup script
├── test.js                 # Setup verification
│
├── public/                 # Frontend files
│   ├── index.html         # Main HTML
│   ├── styles.css         # Styling
│   └── app.js             # Client JavaScript
│
├── uploads/               # Temporary file storage
│   └── .gitkeep          # Keep directory in git
│
└── docs/                  # Documentation
    ├── README_NODEJS.md
    ├── DEPLOYMENT_GUIDE.md
    ├── MODEL_INTEGRATION.md
    ├── GETTING_STARTED.md
    └── ARCHITECTURE.md
```

## API Specification

### POST /api/predict

**Request:**
```json
{
  "age": 30,
  "gender": 1,
  "205758_at": 5.459,
  "241014_at": 8.209,
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

**Status Codes:**
- 200: Success
- 400: Invalid input
- 500: Server error

### POST /api/batch-predict

**Request:**
- Content-Type: multipart/form-data
- Body: CSV file

**Response:**
```json
[
  {
    "age": 30,
    "gender": 1,
    "205758_at": 5.459,
    ...
    "prediction": "Patient",
    "probability": "0.7234",
    "riskLevel": "High"
  },
  ...
]
```

### GET /api/genes

**Response:**
```json
{
  "genes": [
    "205758_at",
    "241014_at",
    ...
  ],
  "symbols": {
    "205758_at": "CD8A",
    "241014_at": "CXCL9",
    ...
  }
}
```

## Security Architecture

### Current Implementation
- Input validation on client and server
- File type restrictions (.csv only)
- Temporary file cleanup
- No authentication (public access)

### Production Recommendations
```
┌─────────────────────────────────────────┐
│  Security Layer                          │
│  ├── HTTPS/SSL                          │
│  ├── Rate Limiting                      │
│  ├── Input Sanitization                 │
│  ├── CORS Configuration                 │
│  ├── Helmet.js (Security Headers)       │
│  └── Authentication (Optional)          │
└─────────────────────────────────────────┘
```

## Scalability Considerations

### Current Setup
- Single server instance
- In-memory data storage
- Synchronous processing

### Production Scaling
```
┌─────────────────────────────────────────┐
│  Load Balancer                           │
└─────────────────────────────────────────┘
         │         │         │
         ▼         ▼         ▼
    ┌─────┐   ┌─────┐   ┌─────┐
    │ App │   │ App │   │ App │
    │  1  │   │  2  │   │  3  │
    └─────┘   └─────┘   └─────┘
         │         │         │
         └─────────┴─────────┘
                   │
                   ▼
         ┌─────────────────┐
         │   Database      │
         │   (Optional)    │
         └─────────────────┘
```

## Deployment Architecture

### Development
```
Local Machine → Node.js Server → Browser
```

### Production (Heroku Example)
```
User → Heroku Load Balancer → Dyno (Container) → 
Node.js App → Model Service (if separate)
```

### Production (AWS Example)
```
User → CloudFront (CDN) → ALB (Load Balancer) → 
EC2/ECS (App Server) → S3 (Static Files) → 
Lambda/ECS (Model Service)
```

## Performance Optimization

### Current
- Static file serving
- Basic caching headers
- Synchronous processing

### Optimizations
1. **Caching:**
   - Redis for prediction cache
   - CDN for static assets

2. **Async Processing:**
   - Queue system for batch predictions
   - Worker processes

3. **Database:**
   - Store prediction history
   - Query optimization

4. **Compression:**
   - Gzip compression
   - Minified assets

## Monitoring & Logging

### Recommended Setup
```
Application → Logging Service → Dashboard
              (Winston/Bunyan)   (Grafana/Datadog)
              
Application → Metrics → Monitoring
              (Prometheus) (Alerting)
```

## Future Enhancements

1. **User Accounts:**
   - Authentication
   - Personal history
   - Saved predictions

2. **Database Integration:**
   - PostgreSQL/MongoDB
   - Persistent storage
   - Query capabilities

3. **Advanced Features:**
   - Visualization charts
   - Comparison tools
   - Export formats (PDF, Excel)

4. **API Improvements:**
   - GraphQL endpoint
   - WebSocket for real-time updates
   - API versioning

5. **Mobile App:**
   - React Native
   - Progressive Web App (PWA)

This architecture provides a solid foundation for a production-ready application while maintaining simplicity and ease of deployment.
