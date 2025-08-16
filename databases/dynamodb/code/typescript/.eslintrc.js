/** @type {import('eslint').Linter.Config} */
const config = {
  root: true,
  parser: '@typescript-eslint/parser',
  parserOptions: {
    project: 'tsconfig.json',
    ecmaVersion: 2020,
    sourceType: 'module'
  },
  env: { node: true },
  extends: ['ackama', 'ackama/@typescript-eslint'],
  ignorePatterns: ['lib/', '.serverless/', '.tmp/'],
  overrides: [
    {
      files: ['test/**'],
      extends: ['ackama/jest'],
      rules: { 'jest/prefer-expect-assertions': 'off' }
    }
  ],
  rules: {}
};

module.exports = config;
