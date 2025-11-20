// Global variables
let genes = [];
let geneSymbols = {};
let uploadedData = null;
let predictionHistory = [];
let batchResults = null;

// Initialize app
document.addEventListener('DOMContentLoaded', async () => {
    await loadGenes();
    setupTabs();
    setupEventListeners();
    renderGeneInputs();
});

// Load genes from server
async function loadGenes() {
    try {
        const response = await fetch('/api/genes');
        const data = await response.json();
        genes = data.genes;
        geneSymbols = data.symbols;
    } catch (error) {
        console.error('Error loading genes:', error);
    }
}

// Setup tab navigation
function setupTabs() {
    const tabButtons = document.querySelectorAll('.tab-button');
    tabButtons.forEach(button => {
        button.addEventListener('click', () => {
            const tabName = button.dataset.tab;
            switchTab(tabName);
        });
    });
}

function switchTab(tabName) {
    // Hide all tabs
    document.querySelectorAll('.tab-content').forEach(tab => {
        tab.classList.remove('active');
    });
    document.querySelectorAll('.tab-button').forEach(btn => {
        btn.classList.remove('active');
    });
    
    // Show selected tab
    document.getElementById(tabName).classList.add('active');
    document.querySelector(`[data-tab="${tabName}"]`).classList.add('active');
}

// Setup event listeners
function setupEventListeners() {
    const csvUpload = document.getElementById('csv-upload');
    csvUpload.addEventListener('change', handleCSVUpload);
}

// Render gene input fields
function renderGeneInputs() {
    const container = document.getElementById('gene-inputs');
    container.innerHTML = '';
    
    genes.forEach(gene => {
        const label = geneSymbols[gene] || gene;
        const div = document.createElement('div');
        div.className = 'gene-input-group';
        div.innerHTML = `
            <label for="${gene}">${label}</label>
            <input type="number" id="${gene}" value="1" min="0" max="15" step="0.01">
        `;
        container.appendChild(div);
    });
}

// Handle CSV upload
function handleCSVUpload(event) {
    const file = event.target.files[0];
    if (!file) return;
    
    const reader = new FileReader();
    reader.onload = (e) => {
        const text = e.target.result;
        parseCSV(text);
    };
    reader.readAsText(file);
}

// Parse CSV data
function parseCSV(text) {
    const lines = text.trim().split('\n');
    const headers = lines[0].split(',');
    const data = [];
    
    for (let i = 1; i < lines.length; i++) {
        const values = lines[i].split(',');
        const row = {};
        headers.forEach((header, index) => {
            row[header.trim()] = values[index];
        });
        data.push(row);
    }
    
    uploadedData = data;
    
    // Show status
    const statusDiv = document.getElementById('csv-status');
    statusDiv.textContent = `CSV uploaded successfully. Rows available: ${data.length}`;
    statusDiv.className = 'status-success';
    
    // Create row selector
    const selectorContainer = document.getElementById('row-selector-container');
    selectorContainer.innerHTML = `
        <div class="form-group">
            <label for="row-selector">Select Row for Input:</label>
            <select id="row-selector" onchange="loadRowData()">
                ${data.map((_, i) => `<option value="${i}">Row ${i + 1}</option>`).join('')}
            </select>
        </div>
    `;
    
    // Load first row
    loadRowData();
}

// Load data from selected row
function loadRowData() {
    const selector = document.getElementById('row-selector');
    if (!selector || !uploadedData) return;
    
    const rowIndex = parseInt(selector.value);
    const row = uploadedData[rowIndex];
    
    // Update demographic fields
    if (row.age) document.getElementById('age').value = row.age;
    if (row.gender !== undefined) document.getElementById('gender').value = row.gender;
    
    // Update gene expression fields
    genes.forEach(gene => {
        if (row[gene] !== undefined) {
            const input = document.getElementById(gene);
            if (input) input.value = row[gene];
        }
    });
}

// Navigate to genes tab
function goToGenes() {
    switchTab('genes');
}

// Predict risk
async function predictRisk() {
    const features = {
        age: parseFloat(document.getElementById('age').value),
        gender: parseInt(document.getElementById('gender').value)
    };
    
    // Get gene expression values
    genes.forEach(gene => {
        const input = document.getElementById(gene);
        if (input) {
            features[gene] = parseFloat(input.value);
        }
    });
    
    try {
        const response = await fetch('/api/predict', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(features)
        });
        
        const result = await response.json();
        displayPrediction(result);
        
        // Add to history
        predictionHistory.push({ ...features, ...result, timestamp: new Date().toISOString() });
        updateHistoryTable();
    } catch (error) {
        console.error('Prediction error:', error);
        alert('Prediction failed. Please try again.');
    }
}

// Display prediction result
function displayPrediction(result) {
    const resultDiv = document.getElementById('prediction-result');
    resultDiv.innerHTML = `
        <h4>Prediction: ${result.prediction}</h4>
        <p>Probability: ${(result.probability * 100).toFixed(2)}%</p>
        <p>Risk Level: ${result.riskLevel}</p>
    `;
    
    const riskDiv = document.getElementById('risk-level');
    if (result.prediction === 'Patient') {
        riskDiv.innerHTML = '🔴 YES';
        riskDiv.className = 'risk-level risk-high';
    } else {
        riskDiv.innerHTML = '✅ NO';
        riskDiv.className = 'risk-level risk-low';
    }
}

// Update history table
function updateHistoryTable() {
    const container = document.getElementById('history-table');
    if (predictionHistory.length === 0) {
        container.innerHTML = '<p>No predictions yet.</p>';
        return;
    }
    
    const table = document.createElement('table');
    table.innerHTML = `
        <thead>
            <tr>
                <th>Age</th>
                <th>Gender</th>
                <th>Prediction</th>
                <th>Probability</th>
                <th>Risk Level</th>
            </tr>
        </thead>
        <tbody>
            ${predictionHistory.map(pred => `
                <tr>
                    <td>${pred.age}</td>
                    <td>${pred.gender === 1 ? 'Male' : 'Female'}</td>
                    <td>${pred.prediction}</td>
                    <td>${(pred.probability * 100).toFixed(2)}%</td>
                    <td>${pred.riskLevel}</td>
                </tr>
            `).join('')}
        </tbody>
    `;
    container.innerHTML = '';
    container.appendChild(table);
}

// Download prediction history
function downloadHistory() {
    if (predictionHistory.length === 0) {
        alert('No predictions to download.');
        return;
    }
    
    const csv = convertToCSV(predictionHistory);
    downloadCSV(csv, `predictions_${new Date().toISOString().split('T')[0]}.csv`);
}

// Run batch prediction
async function runBatchPrediction() {
    if (!uploadedData) {
        alert('Please upload a CSV file first.');
        return;
    }
    
    const formData = new FormData();
    const csvUpload = document.getElementById('csv-upload');
    formData.append('csvFile', csvUpload.files[0]);
    
    try {
        const response = await fetch('/api/batch-predict', {
            method: 'POST',
            body: formData
        });
        
        batchResults = await response.json();
        displayBatchResults(batchResults);
        document.getElementById('download-batch-btn').style.display = 'inline-block';
    } catch (error) {
        console.error('Batch prediction error:', error);
        alert('Batch prediction failed. Please try again.');
    }
}

// Display batch results
function displayBatchResults(results) {
    const container = document.getElementById('batch-result');
    
    const table = document.createElement('table');
    const headers = Object.keys(results[0]);
    
    table.innerHTML = `
        <thead>
            <tr>${headers.map(h => `<th>${h}</th>`).join('')}</tr>
        </thead>
        <tbody>
            ${results.map(row => `
                <tr>${headers.map(h => `<td>${row[h]}</td>`).join('')}</tr>
            `).join('')}
        </tbody>
    `;
    
    container.innerHTML = '';
    container.appendChild(table);
}

// Download batch results
function downloadBatch() {
    if (!batchResults) {
        alert('No batch results to download.');
        return;
    }
    
    const csv = convertToCSV(batchResults);
    downloadCSV(csv, `batch_predictions_${new Date().toISOString().split('T')[0]}.csv`);
}

// Convert array to CSV
function convertToCSV(data) {
    const headers = Object.keys(data[0]);
    const rows = data.map(row => headers.map(h => row[h]).join(','));
    return [headers.join(','), ...rows].join('\n');
}

// Download CSV file
function downloadCSV(csv, filename) {
    const blob = new Blob([csv], { type: 'text/csv' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    window.URL.revokeObjectURL(url);
}

// Reset all inputs
function resetAll() {
    document.getElementById('age').value = 30;
    document.getElementById('gender').value = 1;
    
    genes.forEach(gene => {
        const input = document.getElementById(gene);
        if (input) input.value = 1;
    });
    
    document.getElementById('prediction-result').innerHTML = '';
    document.getElementById('risk-level').innerHTML = '';
    
    switchTab('demographic');
}
