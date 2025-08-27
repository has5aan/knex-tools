# knex-tools

A powerful utility library that extends Knex.js with advanced query building capabilities, sophisticated filtering, pagination, sorting, and transaction management.

## Features

- 🔍 **Advanced Filtering**: Sophisticated where clauses with logical operators (AND, OR, NOT)
- 🎯 **Rich Operators**: Support for equals, not, gt, gte, lt, lte, contains, startsWith, endsWith, in, notIn, isNull, isNotNull
- 📄 **Pagination**: Built-in skip/take pagination support
- 🔄 **Sorting**: Flexible multi-field sorting with default options
- 🔒 **Transactions**: Simplified transaction management with automatic rollback
- 🎛️ **Conditional Filtering**: Dynamic query building with `_condition` flags
- 🗄️ **Database Agnostic**: Works with all Knex.js supported databases

## Installation

```bash
npm install knex-tools
# or
yarn add knex-tools
```

## Requirements

- Node.js >= 14
- Knex.js >= 3.0.0

## Quick Start

```javascript
const knex = require('knex')(config)
const {
  applyWhereClauses,
  applyPagingClauses,
  applySortingClauses,
  buildMakeTransaction
} = require('knex-tools')

// Basic usage
const query = knex('users').select('*')
applyWhereClauses(query, 'users', {
  where: {
    age: { gte: 18 },
    name: { contains: 'John' }
  }
})
```

## API Reference

### applyWhereClauses(query, table, criteria, relations)

Applies advanced filtering conditions to a Knex query with intuitive object-based syntax.

**Parameters:**

- `query` (Object): Knex query builder instance
- `table` (String): Table name for prefixing columns
- `criteria` (Object): Filter criteria containing `where` conditions
- `relations` (Object): Optional relations configuration

**Example:**

```javascript
const criteria = {
  where: {
    // Simple equality
    status: 'active',

    // Null checks
    deleted_at: null,

    // Comparison operators
    age: { gte: 18, lt: 65 },

    // String operations
    name: { contains: 'John' },
    email: { endsWith: '@company.com' },

    // Array operations
    role: { in: ['admin', 'editor'] },
    department: { notIn: ['temp', 'intern'] },

    // Logical operators
    AND: [{ age: { gte: 18 } }, { status: 'active' }],
    OR: [{ role: 'admin' }, { department: 'management' }],
    NOT: {
      status: 'banned'
    }
  }
}

applyWhereClauses(query, 'users', criteria)
```

#### Supported Operators

| Operator     | Description                        | Example                               |
| ------------ | ---------------------------------- | ------------------------------------- |
| `equals`     | Exact match                        | `{ id: { equals: 1 } }`               |
| `not`        | Not equal                          | `{ status: { not: 'inactive' } }`     |
| `gt`         | Greater than                       | `{ age: { gt: 18 } }`                 |
| `gte`        | Greater than or equal              | `{ age: { gte: 21 } }`                |
| `lt`         | Less than                          | `{ score: { lt: 100 } }`              |
| `lte`        | Less than or equal                 | `{ score: { lte: 95 } }`              |
| `contains`   | String contains (case-insensitive) | `{ name: { contains: 'john' } }`      |
| `startsWith` | String starts with                 | `{ email: { startsWith: 'admin' } }`  |
| `endsWith`   | String ends with                   | `{ email: { endsWith: '.com' } }`     |
| `in`         | Value in array                     | `{ role: { in: ['admin', 'user'] } }` |
| `notIn`      | Value not in array                 | `{ status: { notIn: ['banned'] } }`   |
| `isNull`     | Is null                            | `{ deleted_at: { isNull: true } }`    |
| `isNotNull`  | Is not null                        | `{ email: { isNotNull: true } }`      |

#### Conditional Filtering

Use the `_condition` flag to dynamically include/exclude filters:

```javascript
const includeAgeFilter = user.hasPermission('view_age')

const criteria = {
  where: {
    name: { contains: 'John' },
    age: {
      gte: 18,
      _condition: includeAgeFilter // Only applied if true
    }
  }
}
```

### applyPagingClauses(query, criteria)

Adds pagination to a Knex query using `skip` and `take` parameters.

**Parameters:**

- `query` (Object): Knex query builder instance
- `criteria` (Object): Pagination criteria

**Example:**

```javascript
const criteria = {
  skip: 20, // Offset
  take: 10 // Limit
}

applyPagingClauses(query, criteria)
// Generates: LIMIT 10 OFFSET 20
```

### applySortingClauses(query, table, criteria, defaultSortOptions)

Applies sorting to a Knex query with support for multiple fields and default sorting.

**Parameters:**

- `query` (Object): Knex query builder instance
- `table` (String): Table name for prefixing columns
- `criteria` (Object): Sort criteria with field-direction pairs
- `defaultSortOptions` (Object): Default sort configuration

**Example:**

```javascript
// Multi-field sorting
const sortCriteria = {
  created_at: 'desc',
  name: 'asc'
}

// Default sorting when no criteria provided
const defaultSort = {
  field: 'created_at',
  direction: 'desc'
}

applySortingClauses(query, 'users', sortCriteria, defaultSort)
```

### buildMakeTransaction(knexInstance)

Creates a transaction factory function with automatic error handling and rollback.

**Parameters:**

- `knexInstance` (Object): Knex instance

**Returns:**

- `Function`: Transaction factory function

**Example:**

```javascript
const makeTransaction = buildMakeTransaction(knex)

// Use in async functions
const result = await makeTransaction(async trx => {
  const user = await trx('users').insert({ name: 'John' }).returning('*')
  await trx('profiles').insert({ user_id: user[0].id, bio: 'Developer' })

  // If any operation fails, transaction is automatically rolled back
  return user[0]
})
```

## Complete Example

```javascript
const knex = require('knex')({
  client: 'postgresql',
  connection: process.env.DATABASE_URL
})

const {
  applyWhereClauses,
  applyPagingClauses,
  applySortingClauses,
  buildMakeTransaction
} = require('knex-tools')

async function getUsersWithFilters(filters) {
  const query = knex('users')
    .select('users.*')
    .leftJoin('profiles', 'users.id', 'profiles.user_id')

  // Apply complex filtering
  if (filters.where) {
    applyWhereClauses(query, 'users', { where: filters.where })
  }

  // Apply pagination
  if (filters.skip || filters.take) {
    applyPagingClauses(query, {
      skip: filters.skip || 0,
      take: filters.take || 50
    })
  }

  // Apply sorting with default
  applySortingClauses(query, 'users', filters.orderBy, {
    field: 'created_at',
    direction: 'desc'
  })

  return await query
}

// Usage examples
const activeAdults = await getUsersWithFilters({
  where: {
    AND: [{ age: { gte: 18 } }, { status: 'active' }],
    email: { isNotNull: true }
  },
  orderBy: {
    last_login: 'desc',
    name: 'asc'
  },
  skip: 0,
  take: 20
})

// Transaction example
const makeTransaction = buildMakeTransaction(knex)

const newUser = await makeTransaction(async trx => {
  const [user] = await trx('users')
    .insert({
      name: 'Jane Doe',
      email: 'jane@example.com'
    })
    .returning('*')

  await trx('user_settings').insert({
    user_id: user.id,
    theme: 'dark'
  })

  return user
})
```

## Database Compatibility

knex-tools works with all databases supported by Knex.js:

- PostgreSQL (recommended for `contains`, `startsWith`, `endsWith` with case-insensitive search)
- MySQL/MariaDB
- SQLite3
- Oracle Database
- Amazon Redshift
- Microsoft SQL Server

**Note:** String operations (`contains`, `startsWith`, `endsWith`) use `ILIKE` on PostgreSQL for case-insensitive matching, and `LIKE` on other databases.

## Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make your changes and add tests
4. Run tests: `npm test`
5. Run linting: `npm run lint`
6. Submit a pull request

## Testing

```bash
# Run all tests
npm test

# Run tests in watch mode
npm run test:watch

# Run linting
npm run lint

# Format code
npm run format
```

## License

MIT © [has5aan](https://github.com/has5aan)

## Changelog

### v1.0.0

- Initial release
- Advanced where clause building with intuitive object-based syntax
- Pagination and sorting utilities
- Transaction management
- Comprehensive test coverage

---

For more examples and advanced usage patterns, check out the [examples](./examples/) directory.
