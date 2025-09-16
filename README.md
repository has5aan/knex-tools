# knex-tools

**Query builder extension for Node.js** - Extends Knex.js with filtering, GraphQL-style data fetching, exists clause filtering, and JOIN capabilities.

[![npm version](https://badge.fury.io/js/knex-tools.svg)](https://www.npmjs.com/package/knex-tools)
[![Node.js Version](https://img.shields.io/node/v/knex-tools.svg)](https://nodejs.org)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

## 🚀 Quick Start

```bash
npm install knex-tools
```

```javascript
const knex = require('knex')(config)
const { buildQuery } = require('knex-tools')

// GraphQL-style nested data fetching
const users = await buildQuery(knex, userModel, {
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
  take: 10
})
```

## 🔥 Why knex-tools?

### **🎯 Filtering**

```javascript
// Rich operator system
where: {
  age: { gte: 21, lte: 65 },           // Range queries
  name: { contains: 'John' },          // Text search
  email: { endsWith: '@company.com' }, // Pattern matching
  role: { in: ['admin', 'manager'] },  // List queries
  deletedAt: { isNull: true },         // Null checks
  _condition: user.canViewAll          // Dynamic conditions
}
```

### **🔒 Exists Clause Filtering**

```javascript
// Security with _exists operator
where: {
  _exists: {
    // Only show posts user can access
    user: {
      id: currentUser.id,
      role: { in: ['admin', 'owner'] }
    }
  }
}
```

### **📊 GraphQL-Style Data Fetching**

```javascript
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

### **🏗️ Horizontal Table Partitioning**

```javascript
// Create logical views with default modifiers
const activeUserModel = {
  ...userModel,
  modifiers: {
    default: (query, knex, alias) => {
      query.where(`${alias}.active`, true).where(`${alias}.deleted_at`, null)
    }
  }
}

// Automatically filtered - no parameters needed
const activeUsers = await buildQuery(knex, activeUserModel, {
  projection: 'details'
})
```

## 📖 Core Concepts

### **Models Define Structure**

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

### **Query Building**

```javascript
const { applyWhereClauses, applySortingClauses } = require('knex-tools')

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
```

## 🔧 API

| Function               | Purpose                     | Use Case                  |
| ---------------------- | --------------------------- | ------------------------- |
| `buildQuery`           | GraphQL-style data fetching | Nested queries            |
| `applyWhereClauses`    | Filtering                   | Rich search functionality |
| `applySortingClauses`  | Multi-field sorting         | Ordered results           |
| `applyPagingClauses`   | Pagination                  | Large dataset handling    |
| `applyJoinConditions`  | JOIN conditions             | Complex JOIN logic        |
| `processJoins`         | JOINs                       | Report generation         |
| `buildMakeTransaction` | Transaction management      | Data consistency          |

## 📚 Documentation

- 📖 **[API Reference](docs/API_REFERENCE.md)** - Function documentation
- 🏗️ **[Models Guide](docs/MODELS_GUIDE.md)** - Model definitions and patterns

## 🏢 Features

### **Multi-tenant Architecture**

```javascript
// Tenant isolation with exists clause
where: {
  _exists: {
    tenant: {
      id: user.tenantId,
      active: true
    }
  }
}
```

### **Reporting**

```javascript
// JOIN scenarios with proper aliasing
const report = await processJoins(
  query,
  userModel,
  {
    posts: {
      on: { published: true },
      where: { created_at: { gte: startDate } }
    },
    tags: {
      joinType: 'leftJoin',
      on: { active: true }
    }
  },
  relations
)
// Generated SQL: LEFT JOIN user_tags as ut ON u.id = ut.user_id
//                LEFT JOIN tags as t ON ut.tag_id = t.id
```

### **Performance Optimization**

- **Smart batching** - Eliminates N+1 queries
- **Efficient projections** - Only fetch needed columns
- **Optimized JOINs** - Minimal database round trips
- **Alias management** - Prevents SQL conflicts

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

## 📄 License

Apache 2.0 © [Hassaan](mailto:has5aan@outlook.com)

---

**Built for applications that need flexible data access patterns.**
