const express = require('express');
const multer = require('multer');
const csv = require('csv-parser');
const fs = require('fs');
const path = require('path');

const app = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(express.json());
app.use(express.urlencoded({ extended: true }));
app.use(express.static('public'));

// Configure multer for file uploads
// Use /tmp for Vercel serverless environment
const upload = multer({ 
  dest: process.env.VERCEL ? '/tmp/uploads/' : 'uploads/' 
});

// Gene list (21 genes from the model)
const GENES = [
  '205758_at', '241014_at', '1559131_a_at', '224555_x_at', '210176_at',
  '204440_at', '1562576_at', '240343_at', '241154_x_at', '223720_at',
  '213537_at', '206749_at', '220507_s_at', '209728_at', '242218_at',
  '207651_at', '1552870_s_at', '1558687_a_at', '203915_at', '209924_at',
  '210311_at'
];

// Gene symbol mapping (simplified - in production, load from annotation database)
const GENE_SYMBOLS = {
  '205758_at': 'CD8A',
  '241014_at': 'CXCL9',
  '1559131_a_at': 'STAT1',
  '224555_x_at': 'IFNG',
  '210176_at': 'GZMB'
  // Add more mappings as needed
};

// Simple prediction model (placeholder - replace with actual model)
function predictRisk(features) {
  // This is a simplified prediction function
  // In production, you would load the actual trained model
  
  const { age, gender, ...geneExpression } = features;
  
  // Calculate a simple risk score based on gene expression levels
  let riskScore = 0;
  let geneCount = 0;
  
  for (const gene of GENES) {
    if (geneExpression[gene] !== undefined) {
      riskScore += parseFloat(geneExpression[gene]);
      geneCount++;
    }
  }
  
  // Normalize and add demographic factors
  riskScore = riskScore / geneCount;
  riskScore += (age / 100) * 0.1;
  riskScore += gender * 0.05;
  
  // Convert to probability (0-1)
  const probability = Math.min(Math.max((riskScore - 4) / 6, 0), 1);
  const prediction = probability > 0.5 ? 'Patient' : 'Control';
  
  return {
    prediction,
    probability: probability.toFixed(4),
    riskLevel: probability > 0.7 ? 'High' : probability > 0.4 ? 'Medium' : 'Low'
  };
}

// Routes
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

app.post('/api/predict', (req, res) => {
  try {
    const features = req.body;
    const result = predictRisk(features);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: 'Prediction failed', message: error.message });
  }
});

app.post('/api/batch-predict', upload.single('csvFile'), (req, res) => {
  if (!req.file) {
    return res.status(400).json({ error: 'No file uploaded' });
  }

  const results = [];
  const filePath = req.file.path;

  fs.createReadStream(filePath)
    .pipe(csv())
    .on('data', (row) => {
      const prediction = predictRisk(row);
      results.push({ ...row, ...prediction });
    })
    .on('end', () => {
      fs.unlinkSync(filePath); // Clean up uploaded file
      res.json(results);
    })
    .on('error', (error) => {
      fs.unlinkSync(filePath);
      res.status(500).json({ error: 'CSV processing failed', message: error.message });
    });
});

app.get('/api/genes', (req, res) => {
  res.json({
    genes: GENES,
    symbols: GENE_SYMBOLS
  });
});

// For local development
if (process.env.NODE_ENV !== 'production') {
  app.listen(PORT, () => {
    console.log(`Server running on http://localhost:${PORT}`);
  });
}

// Export for Vercel serverless
module.exports = app;
