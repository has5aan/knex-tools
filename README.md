# knex-tools

**Query builder extension for Node.js** - Extends Knex.js with filtering, GraphQL-style data fetching, exists clause filtering, and JOIN capabilities.

[![npm version](https://badge.fury.io/js/knex-tools.svg)](https://www.npmjs.com/package/knex-tools)
[![Node.js Version](https://img.shields.io/node/v/knex-tools.svg)](https://nodejs.org)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Coverage](https://img.shields.io/badge/coverage-94%25-brightgreen.svg)](https://github.com/has5aan/knex-tools)

## 📦 Installation

```bash
npm install knex-tools
```

## 🔥 Why knex-tools?

### **🎯 Filtering**

Rich filtering with the `applyWhereClauses` method. Model definition is **not required** for standard operators (see [Operators Reference](docs/API_REFERENCE.md#operators-reference)).

```javascript
const { applyWhereClauses } = require('knex-tools')

const query = knex('users as u').select('*')

// Apply filtering - no model definition needed for standard operators
applyWhereClauses(query, 'u', {
  where: {
    age: { gte: 21, lte: 65 }, // Range queries
    name: { contains: 'John' }, // Text search
    email: { endsWith: '@company.com' }, // Pattern matching
    role: { in: ['admin', 'manager'] }, // List queries
    deletedAt: { isNull: true }, // Null checks
    department: {
      in: ['IT', 'Sales'],
      _condition: user.canViewAll // Dynamic conditions
    }
  }
})

const users = await query
```

### **🔒 Exists Clause Filtering**

The `_exists` operator in `applyWhereClauses` enables row-level security. **Model definition with relations is required** for this operator.

```javascript
const { applyWhereClauses } = require('knex-tools')

const query = knex('posts as p').select('*')

// _exists requires model definition with relations
applyWhereClauses(
  query,
  'p',
  {
    where: {
      _exists: {
        // Only show posts user can access
        user: {
          id: currentUser.id,
          role: { in: ['admin', 'owner'] }
        }
      }
    }
  },
  postModel.relations // Model relations required for _exists
)

const posts = await query
```

### **📊 GraphQL-Style Data Fetching**

The `buildQuery` method enables efficient nested data loading without N+1 queries.

```javascript
const { buildQuery } = require('knex-tools')

// Nested data fetching with metadata
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  where: {
    role: 'admin',
    active: true,
    lastLogin: { gte: '2024-01-01' }
  },
  each: {
    posts: {
      projection: 'summary',
      where: { published: true }
    }
  },
  orderBy: { created_at: 'desc' },
  take: 10,
  metadata: {
    counts: { total: true, filtered: true }
  }
})

// Result:
// {
//   data: [
//     {
//       id: 1,
//       name: 'John Doe',
//       email: 'john@example.com',
//       role: 'admin',
//       posts: {
//         data: [{ id: 10, title: 'Post Title', user_id: 1 }]
//       }
//     }
//   ],
//   metadata: { counts: { total: 500, filtered: 12 } }
// }

// Efficient nested loading (no N+1 queries)
const posts = await buildQuery(knex, postModel, {
  each: {
    author: { projection: 'profile' },
    comments: {
      projection: 'details',
      orderBy: { created_at: 'desc' },
      take: 5
    },
    tags: { projection: 'name' }
  }
})
```

### **📈 Metadata & Count Tracking**

```javascript
const { buildQuery } = require('knex-tools')

// Get counts alongside your data
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  where: { active: true },
  metadata: {
    counts: {
      total: true, // Total users in database
      filtered: true // Users matching where clause
    }
  },
  each: {
    posts: {
      projection: 'summary',
      metadata: { counts: { total: true } } // Also count user's posts
    }
  }
})

// Result structure:
// {
//   data: [{
//     id: 1, name: 'John',
//     posts: {
//       data: [...],
//       metadata: { counts: { total: 5 } }
//     }
//   }],
//   metadata: { counts: { total: 1000, filtered: 25 } }
// }
```

### **✅ Existence Checks**

When you only need to know if records exist (without fetching them), use `exists` for optimal performance. It uses `SELECT 1 LIMIT 1` internally.

```javascript
const { exists } = require('knex-tools')

// Check if any users exist
const hasUsers = await exists(knex, userModel, {})
// Returns: true or false

// Check if specific records exist
const hasAdmins = await exists(knex, userModel, {
  where: { role: 'admin' }
})
// Returns: true or false

// Combine filters and modifiers
const hasActiveAdmins = await exists(knex, userModel, {
  where: {
    role: 'admin',
    lastLogin: { gte: '2024-01-01' }
  },
  modifiers: {
    activeOnly: {}
  }
})
// Returns: true or false
```

### **🔢 Count-Only Queries**

When you only need counts without fetching data, use `getCounts` for better performance.

```javascript
const { getCounts } = require('knex-tools')

// Get total and filtered counts
const counts = await getCounts(knex, userModel, {
  where: { active: true },
  counts: {
    total: true, // Total records in table
    filtered: true // Records matching where clause
  }
})
// Returns: { total: 1000, filtered: 250 }

// Use with modifiers for custom counts
const counts = await getCounts(knex, userModel, {
  where: { role: 'admin' },
  counts: {
    total: true,
    filtered: true,
    modifiers: {
      inactive: { active: false } // Count inactive users
    }
  }
})
// Returns: { total: 1000, filtered: 50, inactive: 100 }
```

### **🔢 Related Counts**

Get counts of related records per parent record efficiently with `withRelatedCounts`. This performs a single optimized GROUP BY query per relation instead of N+1 queries.

```javascript
const { buildQuery } = require('knex-tools')

// Get counts of related items for each record
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  withRelatedCounts: {
    posts: true, // Count all posts per user
    folders: true // Count all folders per user
  }
})
// Result:
// {
//   data: [
//     { id: 1, name: 'John', _counts: { posts: 15, folders: 3 } },
//     { id: 2, name: 'Jane', _counts: { posts: 8, folders: 5 } }
//   ]
// }

// Filter related counts with where conditions
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  withRelatedCounts: {
    posts: { where: { published: true } } // Only count published posts
  }
})
// Result:
// {
//   data: [
//     { id: 1, name: 'John', _counts: { posts: 10 } }, // Only published posts counted
//     { id: 2, name: 'Jane', _counts: { posts: 0 } }   // No published posts
//   ]
// }

// Combine with nested relations and their counts
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  withRelatedCounts: {
    folders: true
  },
  each: {
    posts: {
      projection: 'summary',
      withRelatedCounts: {
        comments: true,
        tags: { where: { active: true } }
      }
    }
  }
})
// Result:
// {
//   data: [
//     {
//       id: 1,
//       name: 'John',
//       _counts: { folders: 3 },
//       posts: {
//         data: [
//           { id: 10, title: 'Post 1', _counts: { comments: 5, tags: 2 } },
//           { id: 11, title: 'Post 2', _counts: { comments: 3, tags: 1 } }
//         ]
//       }
//     }
//   ]
// }
```

**Supported Relations:**

- `hasMany` - Counts related records (e.g., user → posts)
- `manyToMany` - Counts through junction table (e.g., post → tags)

### **🏗️ Horizontal Table Partitioning**

Modifiers are reusable query functions defined in your model. The special `default` modifier is **automatically applied** by `buildQuery` to every query, making it perfect for creating logical data partitions (e.g., active-only views, tenant isolation).

```javascript
const { buildQuery } = require('knex-tools')

// Create logical views with default modifiers
const activeUserModel = {
  ...userModel,
  modifiers: {
    // Special 'default' modifier - automatically applied to all queries
    default: (query, alias) => {
      query.where(`${alias}.active`, true).whereNull(`${alias}.deleted_at`)
    }
  }
}

// Automatically filtered - no parameters needed
const activeUsers = await buildQuery(knex, activeUserModel, {
  projection: 'details'
})
// Only returns active, non-deleted users automatically
```

## 📖 Core Concepts

### **Query Building**

```javascript
const {
  applyWhereClauses,
  applySortingClauses,
  applyPagingClauses
} = require('knex-tools')

const query = knex('users as u').select('*')

// Rich filtering
applyWhereClauses(
  query,
  'u',
  {
    where: {
      age: { gte: 18 },
      OR: [{ name: { startsWith: 'John' } }, { email: { contains: '@admin' } }]
    }
  },
  userModel.relations
)

// Multi-field sorting
applySortingClauses(query, 'u', {
  role: 'asc',
  created_at: 'desc'
})

// Pagination
applyPagingClauses(query, {
  skip: 20,
  take: 10
})

const users = await query
```

### **Model Definition Structure**

```javascript
const userModel = {
  tableName: 'users',
  alias: 'u',

  // Function-based projections with alias support (REQUIRED for buildQuery)
  projections: {
    details: (knexInstanceOrQuery, alias, relationName = null) => [
      `${alias}.id`,
      `${alias}.name`,
      `${alias}.email`,
      `${alias}.role`
    ],
    summary: (knexInstanceOrQuery, alias, relationName = null) => [
      `${alias}.id`,
      `${alias}.name`
    ]
  },

  // Relations for data fetching and security
  relations: {
    posts: {
      type: 'hasMany',
      model: 'post',
      table: 'posts',
      foreignKey: 'user_id',
      primaryKey: 'id',
      modelDefinition: () => require('./post.model')
    },
    tags: {
      type: 'manyToMany',
      model: 'tag',
      table: 'tags',
      primaryKey: 'id',
      through: {
        table: 'user_tags',
        alias: 'ut', // Junction table alias (required)
        foreignKey: 'user_id',
        otherKey: 'tag_id'
      },
      modelDefinition: () => require('./tag.model')
    }
  },

  // Modifiers for reusable query logic
  modifiers: {
    forRole: (query, knex, alias, { role }) => {
      query.where(`${alias}.role`, role)
    },
    withMinPosts: (query, knex, alias, { minCount }) => {
      query
        .join('posts', `${alias}.id`, 'posts.user_id')
        .groupBy(`${alias}.id`)
        .having(knex.raw('COUNT(posts.id)'), '>=', minCount)
    }
  }
}
```

### **Using Named Modifiers**

```javascript
// Apply named modifiers with parameters
const adminUsers = await buildQuery(knex, userModel, {
  projection: 'details',
  modifiers: {
    forRole: { role: 'admin' }
  }
})

// Combine multiple modifiers
const prolificAdmins = await buildQuery(knex, userModel, {
  projection: 'details',
  modifiers: {
    forRole: { role: 'admin' },
    withMinPosts: { minCount: 10 }
  }
})
```

## 🏢 Advanced Features

### **🔗 JOINs**

```javascript
// Complex data fetching with JOINs
const result = await processJoins(
  knex('users as u').select(['u.*', 'p.title', 't.name as tag_name']),
  userModel,
  {
    posts: {
      type: 'enforce', // INNER JOIN - only users with posts
      on: { published: true, status: 'approved' }
    },
    tags: {
      type: 'include', // LEFT JOIN - all users, with/without tags
      on: { active: true, category: { not: 'private' } }
    },
    profile: {
      on: { verified: true },
      where: { updated_at: { gte: '2024-01-01' } } // Post-JOIN filtering
    }
  },
  userModel.relations
)

// Advanced: Nested JOINs for hierarchical data
const nestedResult = await processJoins(
  knex('posts as p').select('*'),
  postModel,
  {
    author: {
      join: {
        department: {
          // author.department relation
          type: 'enforce',
          on: { active: true }
        }
      }
    }
  },
  postModel.relations
)
// SQL: INNER JOIN users as u ON p.author_id = u.id
//      INNER JOIN departments as d ON u.department_id = d.id AND d.active = true
```

### **Performance Optimization**

- **Smart batching** - Eliminates N+1 queries
- **Efficient projections** - Only fetch needed columns
- **Optimized JOINs** - Minimal database round trips
- **Alias management** - Prevents SQL conflicts

## 🔧 API

| Function               | Purpose                         |
| ---------------------- | ------------------------------- |
| `exists`               | Lightweight existence checks    |
| `getCounts`            | Lightweight count-only queries  |
| `buildQuery`           | GraphQL-style data fetching     |
| `applyWhereClauses`    | Rich filtering with operators   |
| `applySortingClauses`  | Multi-field sorting             |
| `applyPagingClauses`   | Pagination with skip/take       |
| `processJoins`         | JOIN operations with conditions |
| `buildMakeTransaction` | Transaction management          |

## 📚 Documentation

- 📖 **[API Reference](docs/API_REFERENCE.md)** - Function documentation
- 🏗️ **[Models Guide](docs/MODELS_GUIDE.md)** - Model definitions and patterns

## 🧪 Test Coverage

knex-tools maintains high test coverage to ensure reliability and stability.

| Metric     | Coverage |
| ---------- | -------- |
| Statements | 94.38%   |
| Branches   | 85.56%   |
| Functions  | 91.91%   |
| Lines      | 94.35%   |

**Test Suite**: 128 tests passing

Run coverage report:

```bash
npm run test:coverage
```

## 📄 License

Apache 2.0 © [Hassaan](mailto:has5aan@outlook.com)
