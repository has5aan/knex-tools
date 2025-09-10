# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Testing

- `npm test` - Run all Jest tests
- `npm run test:watch` - Run tests in watch mode
- `npm run test:coverage` - Run tests with coverage report

### Code Quality

- `npm run lint` - Run ESLint linter
- `npm run lint:fix` - Run ESLint with auto-fix
- `npm run format` - Format code with Prettier
- `npm run format:check` - Check code formatting
- `npm run prepublishOnly` - Run tests and linting before publish

### Publishing

- `npm run preversion` - Run tests and linting before version bump
- `npm run postversion` - Push changes and tags after version bump

## Architecture

This is a Knex.js utility library that provides query building enhancements. The main export is from `src/knex-tools.js` with six core functions:

### Core Functions

- `buildQuery(knexInstance, modelObject, queryConfig)` - GraphQL-style data fetching with nested relations and filtering. Returns structured results with optional metadata counts
- `applyWhereClauses(query, table, criteria, relations)` - Filtering with logical operators (AND, OR, NOT) and rich comparison operators (equals, not, gt, gte, lt, lte, contains, startsWith, endsWith, in, notIn, isNull, isNotNull, hasAll)
- `applyPagingClauses(query, criteria)` - Pagination using skip/take parameters
- `applySortingClauses(query, table, criteria, defaultSortOptions)` - Multi-field sorting with defaults
- `processJoins(query, rootModel, joins, relations)` - JOIN operations with conditions and logical operators
- `buildMakeTransaction(knexInstance)` - Transaction factory with automatic rollback

### Key Design Patterns

- All functions return the modified Knex query for chaining
- Conditional filtering via `_condition` flags that can dynamically include/exclude filters
- Database-agnostic with PostgreSQL optimizations (ILIKE vs LIKE for string operations)
- Prisma-style null handling (direct null assignment)
- Logical operator support with nested condition arrays

### Testing Setup

Tests use in-memory SQLite database created in `tests/setup.js`. Each test gets a fresh database instance that's cleaned up automatically.

### Code Style

- ESLint config enforces curly braces and 1tbs brace style
- Prettier config: no semicolons, single quotes, no arrow parens, 2-space tabs, no trailing commas
- Node.js >= 14 and Knex.js >= 3.0.0 peer dependency

## buildQuery Result Structure

The `buildQuery` function returns results in a consistent structure:

```javascript
{
  data: [...], // Array of query results
  metadata: {  // Optional, included when queryConfig.includeCounts = true
    total: number,    // Total records without filters
    filtered: number  // Records matching filter criteria
  }
}
```

### Basic Usage

```javascript
// Simple query without counts
const result = await buildQuery(knex, userModel, {
  projection: 'details'
})
// Returns: { data: [{ id: 1, name: 'John', email: 'john@example.com', ... }] }

// Query with metadata counts
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  metadata: { counts: { total: true, filtered: true } }
})
// Returns: {
//   data: [{ id: 1, name: 'John', ... }],
//   metadata: { counts: { total: 100, filtered: 25 } }
// }
```

### Relation Metadata

Relations also follow the same structure when populated:

```javascript
// User with folders relation
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  each: {
    folders: {
      projection: 'short',
      metadata: { counts: { total: true, filtered: true } }
    }
  }
})
// Returns:
// {
//   data: [{
//     id: 1, name: 'John', email: 'john@example.com',
//     folders: {
//       data: [{ id: 1, name: 'Work', user_id: 1 }],
//       metadata: { counts: { total: 5, filtered: 3 } }
//     }
//   }]
// }
```
