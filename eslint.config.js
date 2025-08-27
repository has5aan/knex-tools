const eslint = require('@eslint/js')
const globals = require('globals')

module.exports = [
  eslint.configs.recommended,
  {
    files: ['**/*.js'],
    languageOptions: {
      globals: {
        ...globals.node,
        ...globals.jest
      }
    },
    rules: {
      curly: ['error', 'all'],
      'brace-style': ['error', '1tbs']
    }
  }
]
