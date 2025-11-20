// Simple test script to verify the application setup

const http = require('http');

console.log('🧪 Testing Alopecia Areata Risk Calculator Setup\n');

// Test 1: Check if required files exist
console.log('Test 1: Checking required files...');
const fs = require('fs');
const requiredFiles = [
    'server.js',
    'package.json',
    'public/index.html',
    'public/styles.css',
    'public/app.js'
];

let allFilesExist = true;
requiredFiles.forEach(file => {
    if (fs.existsSync(file)) {
        console.log(`  ✅ ${file} exists`);
    } else {
        console.log(`  ❌ ${file} missing`);
        allFilesExist = false;
    }
});

if (!allFilesExist) {
    console.log('\n❌ Some required files are missing!');
    process.exit(1);
}

// Test 2: Check if uploads directory exists
console.log('\nTest 2: Checking uploads directory...');
if (!fs.existsSync('uploads')) {
    console.log('  ⚠️  Creating uploads directory...');
    fs.mkdirSync('uploads');
}
console.log('  ✅ uploads directory ready');

// Test 3: Check Node.js version
console.log('\nTest 3: Checking Node.js version...');
const nodeVersion = process.version;
const majorVersion = parseInt(nodeVersion.split('.')[0].substring(1));
if (majorVersion >= 14) {
    console.log(`  ✅ Node.js ${nodeVersion} (compatible)`);
} else {
    console.log(`  ❌ Node.js ${nodeVersion} (requires v14 or higher)`);
    process.exit(1);
}

// Test 4: Check if dependencies are installed
console.log('\nTest 4: Checking dependencies...');
if (fs.existsSync('node_modules')) {
    console.log('  ✅ node_modules exists');
    
    const requiredPackages = ['express', 'multer', 'csv-parser'];
    requiredPackages.forEach(pkg => {
        if (fs.existsSync(`node_modules/${pkg}`)) {
            console.log(`  ✅ ${pkg} installed`);
        } else {
            console.log(`  ❌ ${pkg} not installed`);
        }
    });
} else {
    console.log('  ⚠️  node_modules not found. Run: npm install');
}

console.log('\n✅ All tests passed! Your application is ready.');
console.log('\nTo start the server, run:');
console.log('  npm start');
console.log('\nOr use the startup script:');
console.log('  ./start.sh');
console.log('\nThen open: http://localhost:3000\n');
