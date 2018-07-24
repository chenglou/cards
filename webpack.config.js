const path = require('path');
const outputDir = path.join(__dirname, "build/");

module.exports = {
  entry: './src/Index.bs.js',
  mode: 'production',
  output: {
    path: outputDir,
    publicPath: outputDir,
    filename: 'Index.js',
  },
};
