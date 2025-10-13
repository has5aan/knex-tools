# API Reference

Documentation for all knex-tools functions and their parameters.

## Table of Contents

- [Core Functions](#core-functions)
  - [exists](#exists)
  - [counts](#counts)
  - [buildQuery](#buildquery)
  - [applyWhereClauses](#applywhereclauses)
  - [applySortingClauses](#applysortingclauses)
  - [applyPagingClauses](#applypagingclauses)
  - [processJoins](#processjoins)
  - [buildMakeTransaction](#buildmaketransaction)
- [Fluent API](#fluent-api)
  - [WhereBuilder](#wherebuilder)
  - [QueryBuilder](#querybuilder)
  - [CountsBuilder](#countsbuilder)
  - [ExistsBuilder](#existsbuilder)
- [Operators Reference](#operators-reference)
- [Model Structure](#model-structure)

---

## Core Functions

### buildQuery

**GraphQL-style data fetching with nested relations and filtering.**

```javascript
buildQuery(knexInstance, modelObject, queryConfig)
```

#### Parameters

| Parameter      | Type     | Description                                   |
| -------------- | -------- | --------------------------------------------- |
| `knexInstance` | `Knex`   | Knex.js database instance                     |
| `modelObject`  | `Object` | Model definition with structure and relations |
| `queryConfig`  | `Object` | Query configuration object                    |

#### Query Configuration

| Property            | Type     | Description                      | Example                                       |
| ------------------- | -------- | -------------------------------- | --------------------------------------------- |
| `projection`        | `string` | Projection key from model        | `'details'`                                   |
| `where`             | `Object` | Filtering conditions             | `{ age: { gte: 18 } }`                        |
| `orderBy`           | `Object` | Sorting configuration            | `{ name: 'asc', created_at: 'desc' }`         |
| `take`              | `number` | Limit results                    | `10`                                          |
| `skip`              | `number` | Offset results                   | `20`                                          |
| `each`              | `Object` | Nested relation loading          | `{ posts: { projection: 'summary' } }`        |
| `modifiers`         | `Object` | Apply named modifiers            | `{ forRole: { role: 'admin' } }`              |
| `metadata`          | `Object` | Include metadata counts          | `{ counts: { total: true, filtered: true } }` |
| `withRelatedCounts` | `Object` | Count related records per parent | `{ posts: true, comments: { where: {...} } }` |

#### Examples

**Basic Query**

```javascript
const users = await buildQuery(knex, userModel, {
  projection: 'details',
  where: { active: true },
  orderBy: { created_at: 'desc' },
  take: 10
})
```

**Nested Data Fetching**

```javascript
const posts = await buildQuery(knex, postModel, {
  projection: 'details',
  each: {
    author: {
      projection: 'profile',
      where: { active: true }
    },
    comments: {
      projection: 'summary',
      orderBy: { created_at: 'desc' },
      take: 5
    }
  }
})
```

**With Modifiers**

```javascript
const adminUsers = await buildQuery(knex, userModel, {
  projection: 'details',
  modifiers: {
    forRole: { role: 'admin' },
    active: {}
  }
})
```

**With Metadata Counts**

```javascript
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  where: { active: true },
  metadata: {
    counts: {
      total: true, // Include total count without filters
      filtered: true // Include count with filters applied
    }
  }
})
// Returns: {
//   data: [{ id: 1, name: 'John', ... }],
//   metadata: { counts: { total: 100, filtered: 25 } }
// }
```

#### Metadata Configuration

The `metadata.counts` parameter must be an **object** (not array) specifying which counts to include:

```javascript
metadata: {
  counts: {
    total: true,     // Include total count without filters
    filtered: true   // Include count with filters applied
  }
}
```

**Important**: Only object format is supported. Array formats like `['total']` are not supported.

#### Related Counts Configuration

The `withRelatedCounts` parameter allows you to efficiently count related records for each parent record using a single GROUP BY query per relation instead of N+1 queries.

**Basic Usage:**

```javascript
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  withRelatedCounts: {
    posts: true, // Count all posts per user
    folders: true // Count all folders per user
  }
})
// Returns: {
//   data: [
//     { id: 1, name: 'John', _counts: { posts: 15, folders: 3 } },
//     { id: 2, name: 'Jane', _counts: { posts: 8, folders: 5 } }
//   ]
// }
```

**With Filtered Counts:**

```javascript
const result = await buildQuery(knex, userModel, {
  projection: 'details',
  withRelatedCounts: {
    posts: { where: { published: true } } // Only count published posts
  }
})
// Returns: {
//   data: [
//     { id: 1, name: 'John', _counts: { posts: 10 } },
//     { id: 2, name: 'Jane', _counts: { posts: 0 } }
//   ]
// }
```

**Nested with Relations:**

```javascript
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
// Each user has _counts.folders
// Each post within user.posts has _counts.comments and _counts.tags
```

**Supported Relations:**

- `hasMany` - Counts related records (e.g., user → posts)
- `manyToMany` - Counts through junction table (e.g., post → tags)

**Configuration Options:**

| Format             | Description                    | Example                                     |
| ------------------ | ------------------------------ | ------------------------------------------- |
| `true`             | Count all related records      | `{ posts: true }`                           |
| `{ where: {...} }` | Count filtered related records | `{ posts: { where: { published: true } } }` |

#### Return Value

buildQuery returns a structured result object:

```javascript
{
  data: Array,      // Query results
  metadata?: {      // Optional, included when metadata.counts specified
    counts: {
      total?: number,    // Total records without filters
      filtered?: number  // Records matching filter criteria
    }
  }
}
```

Relations also follow the same structure when populated with metadata:

```javascript
// User with folders relation and metadata
{
  data: [
    {
      id: 1,
      name: 'John',
      email: 'john@example.com',
      folders: {
        data: [{ id: 1, name: 'Work', user_id: 1 }],
        metadata: { counts: { total: 5, filtered: 3 } }
      }
    }
  ]
}
```

---

### exists

**✅ Lightweight Existence Checks**

Check if records exist without fetching them. Uses `SELECT 1 LIMIT 1` internally for optimal performance.

```javascript
exists(knexInstance, modelObject, queryConfig)
```

#### Parameters

| Parameter      | Type     | Description                  |
| -------------- | -------- | ---------------------------- |
| `knexInstance` | `Knex`   | Knex.js database instance    |
| `modelObject`  | `Object` | Model definition             |
| `queryConfig`  | `Object` | Optional query configuration |

#### Query Configuration

| Property    | Type     | Description           | Example              |
| ----------- | -------- | --------------------- | -------------------- |
| `where`     | `Object` | Filtering conditions  | `{ role: 'admin' }`  |
| `modifiers` | `Object` | Apply named modifiers | `{ activeOnly: {} }` |

#### Return Value

Returns a **boolean**: `true` if at least one record exists matching the criteria, `false` otherwise.

#### Examples

**Basic Existence Check**

```javascript
const { exists } = require('knex-tools')

// Check if any users exist
const hasUsers = await exists(knex, userModel, {})
// Returns: true or false
```

**With Filters**

```javascript
// Check if admin users exist
const hasAdmins = await exists(knex, userModel, {
  where: { role: 'admin' }
})
// Returns: true or false

// Complex filters with operators
const hasRecentActiveUsers = await exists(knex, userModel, {
  where: {
    active: true,
    lastLogin: { gte: '2024-01-01' },
    OR: [{ role: 'admin' }, { permissions: { contains: 'write' } }]
  }
})
// Returns: true or false
```

**With Modifiers**

```javascript
// Check using model modifiers
const hasActiveAdmins = await exists(knex, userModel, {
  where: { role: 'admin' },
  modifiers: {
    activeOnly: {}
  }
})
// Returns: true or false

// Multiple modifiers are combined
const result = await exists(knex, postModel, {
  modifiers: {
    published: {},
    recent: { days: 7 }
  }
})
// Returns: true or false
```

#### Performance

The `exists` function is optimized for performance:

- Uses `SELECT 1 LIMIT 1` instead of counting or fetching full records
- Returns immediately after finding the first matching record
- Ideal for conditional logic and authorization checks

---

### counts

**🔢 Count-Only Queries**

When you only need counts without fetching data, use `counts` for better performance.

```javascript
const { counts } = require('knex-tools')

// Get total and filtered counts
const counts = await counts(knex, userModel, {
  where: { active: true },
  counts: {
    total: true, // Total records in table
    filtered: true // Records matching where clause
  }
})
// Returns: { total: 1000, filtered: 250 }

// Use with modifiers for custom counts
const counts = await counts(knex, userModel, {
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

### applyWhereClauses

**Filtering with rich operators and logical conditions.**

```javascript
applyWhereClauses(query, table, criteria, relations)
```

#### Parameters

| Parameter   | Type                | Description                            |
| ----------- | ------------------- | -------------------------------------- |
| `query`     | `Knex.QueryBuilder` | Knex query to modify                   |
| `table`     | `string`            | Table alias for field prefixing        |
| `criteria`  | `Object`            | Filter criteria with `where` property  |
| `relations` | `Object`            | Model relations for `_exists` operator |

#### Examples

**Basic Filtering**

```javascript
const query = knex('users as u').select('*')
applyWhereClauses(query, 'u', {
  where: {
    age: { gte: 18, lte: 65 },
    name: { contains: 'John' },
    active: true
  }
})
```

**Logical Operators**

```javascript
applyWhereClauses(query, 'u', {
  where: {
    OR: [
      { name: { startsWith: 'John' } },
      { email: { endsWith: '@admin.com' } }
    ],
    AND: [{ age: { gte: 18 } }, { verified: true }]
  }
})
```

**Exists Clause Filtering**

```javascript
applyWhereClauses(
  query,
  'u',
  {
    where: {
      _exists: {
        posts: {
          published: true,
          created_at: { gte: '2024-01-01' }
        }
      }
    }
  },
  userModel.relations
)
```

**Conditional Filtering**

```javascript
applyWhereClauses(query, 'u', {
  where: {
    role: {
      equals: 'admin',
      _condition: user.isAdmin // Only apply if true
    },
    department: {
      in: ['IT', 'Engineering'],
      _condition: user.canViewAllDepts
    }
  }
})
```

---

### applySortingClauses

**Multi-field sorting with default fallbacks.**

```javascript
applySortingClauses(query, table, criteria, defaultSortOptions)
```

#### Parameters

| Parameter            | Type                | Description                            |
| -------------------- | ------------------- | -------------------------------------- |
| `query`              | `Knex.QueryBuilder` | Knex query to modify                   |
| `table`              | `string`            | Table alias for field prefixing        |
| `criteria`           | `Object`            | Object of field-direction pairs        |
| `defaultSortOptions` | `Object`            | Default sort when no criteria provided |

#### Criteria Format

Object where keys are field names and values are sort directions:

```javascript
{
  fieldName: 'asc' | 'desc',
  // Multiple fields supported
  role: 'asc',
  created_at: 'desc'
}
```

#### Examples

**Basic Sorting**

```javascript
applySortingClauses(query, 'u', {
  role: 'asc',
  created_at: 'desc'
})
```

**With Default Fallback**

```javascript
applySortingClauses(query, 'u', sortCriteria, {
  field: 'id',
  direction: 'asc'
})
```

---

### applyPagingClauses

**Skip/take pagination for large datasets.**

```javascript
applyPagingClauses(query, criteria)
```

#### Parameters

| Parameter  | Type                | Description              |
| ---------- | ------------------- | ------------------------ |
| `query`    | `Knex.QueryBuilder` | Knex query to modify     |
| `criteria` | `Object`            | Pagination configuration |

#### Pagination Configuration

| Property | Type     | Description                         |
| -------- | -------- | ----------------------------------- |
| `skip`   | `number` | Number of records to skip (offset)  |
| `take`   | `number` | Number of records to return (limit) |

#### Examples

**Basic Pagination**

```javascript
applyPagingClauses(query, {
  skip: 20, // Skip first 20 records
  take: 10 // Return next 10 records
})
```

**Page-based Calculation**

```javascript
const page = 3
const pageSize = 10
applyPagingClauses(query, {
  skip: (page - 1) * pageSize,
  take: pageSize
})
```

---

### processJoins

**JOIN operations with conditions and logical operators for complex data fetching.**

```javascript
processJoins(query, rootModel, joins, relations)
```

#### Parameters

| Parameter   | Type                | Description                |
| ----------- | ------------------- | -------------------------- |
| `query`     | `Knex.QueryBuilder` | Knex query to modify       |
| `rootModel` | `Object`            | Root model definition      |
| `joins`     | `Object`            | JOIN configurations        |
| `relations` | `Object`            | Model relations definition |

#### JOIN Configuration

| Property | Type     | Description                                              | Default     |
| -------- | -------- | -------------------------------------------------------- | ----------- |
| `type`   | `string` | JOIN behavior: `'enforce'` (INNER) or `'include'` (LEFT) | `'include'` |
| `on`     | `Object` | JOIN-time filtering conditions                           | `{}`        |
| `where`  | `Object` | Post-JOIN WHERE conditions                               | `{}`        |
| `join`   | `Object` | Nested JOINs on this relation                            | `{}`        |

#### JOIN Types

| Type        | SQL JOIN   | Use Case                               |
| ----------- | ---------- | -------------------------------------- |
| `'include'` | LEFT JOIN  | Include rows even if relation is null  |
| `'enforce'` | INNER JOIN | Only include rows with valid relations |

#### Supported Operators

JOIN conditions support all the same operators as WHERE clauses:

- **Comparison**: `equals`, `not`, `gt`, `gte`, `lt`, `lte`, `in`, `notIn`, `isNull`, `isNotNull`
- **String**: `contains`, `startsWith`, `endsWith`
- **Logical**: `AND`, `OR`
- **Conditional**: `_condition` flags

#### Examples

**Basic JOINs**

```javascript
// Simple LEFT JOINs
processJoins(
  query,
  userModel,
  {
    posts: true, // Simple LEFT JOIN
    profile: {
      // LEFT JOIN with conditions
      on: { active: true }
    }
  },
  userModel.relations
)
// Generated SQL:
// LEFT JOIN posts as p ON u.id = p.user_id
// LEFT JOIN profiles as pr ON u.id = pr.user_id AND pr.active = true
```

**INNER vs LEFT JOINs**

```javascript
processJoins(
  query,
  userModel,
  {
    posts: {
      type: 'enforce', // INNER JOIN - only users with posts
      on: { published: true }
    },
    profile: {
      type: 'include', // LEFT JOIN - all users (default)
      on: { verified: true }
    }
  },
  userModel.relations
)
// Generated SQL:
// INNER JOIN posts as p ON u.id = p.user_id AND p.published = true
// LEFT JOIN profiles as pr ON u.id = pr.user_id AND pr.verified = true
```

**Advanced JOIN Conditions**

```javascript
processJoins(
  query,
  postModel,
  {
    author: {
      on: {
        active: true,
        role: { in: ['author', 'editor'] },
        created_at: { gte: '2024-01-01' }
      }
    },
    tags: {
      type: 'include',
      on: {
        AND: [{ active: true }, { category: { not: 'hidden' } }]
      }
    }
  },
  postModel.relations
)
```

**ManyToMany JOINs**

```javascript
processJoins(
  query,
  userModel,
  {
    tags: {
      type: 'enforce', // Only users with tags
      on: {
        active: true, // Tag must be active
        type: { in: ['skill', 'interest'] }
      }
    }
  },
  userModel.relations
)
// Generated SQL for manyToMany:
// INNER JOIN user_tags as ut ON u.id = ut.user_id
// INNER JOIN tags as t ON ut.tag_id = t.id AND t.active = true AND t.type IN ('skill', 'interest')
```

**JOIN vs WHERE Conditions**

```javascript
processJoins(
  query,
  postModel,
  {
    author: {
      on: {
        active: true // JOIN condition - affects JOIN behavior
      },
      where: {
        last_login: { gte: '2024-01-01' } // WHERE condition - filters after JOIN
      }
    }
  },
  postModel.relations
)
// Generated SQL:
// LEFT JOIN users as u ON p.user_id = u.id AND u.active = true
// WHERE u.last_login >= '2024-01-01'
```

**Nested JOINs**

```javascript
processJoins(
  query,
  memoModel,
  {
    folder: {
      join: {
        parent: {
          // folder.parent relation
          on: { active: true }
        },
        owner: {
          // folder.owner relation
          type: 'enforce'
        }
      }
    }
  },
  memoModel.relations
)
// Generated SQL:
// LEFT JOIN folders as f ON m.folder_id = f.id
// LEFT JOIN folders as fp ON f.parent_id = fp.id AND fp.active = true
// INNER JOIN users as u ON f.user_id = u.id
```

**Conditional JOINs**

```javascript
const includeArchived = user.hasRole('admin')

processJoins(
  query,
  postModel,
  {
    author: {
      on: {
        active: true,
        archived: { equals: true, _condition: includeArchived }
      }
    }
  },
  postModel.relations
)
// If user is admin: JOIN includes archived authors
// If user is not admin: archived condition is ignored
```

**Self-Referencing JOINs**

```javascript
processJoins(
  query,
  folderModel,
  {
    parent: {
      // Self-reference: folder -> parent folder
      on: { active: true }
    },
    children: {
      // Self-reference: folder -> child folders
      type: 'include',
      on: { deleted_at: { isNull: true } }
    }
  },
  folderModel.relations
)
// Generated SQL with proper aliasing:
// LEFT JOIN folders as parent ON f.parent_id = parent.id AND parent.active = true
// LEFT JOIN folders as children ON f.id = children.parent_id AND children.deleted_at IS NULL
```

#### Performance Considerations

- **INNER JOINs** (`type: 'enforce'`) are typically faster than LEFT JOINs
- **JOIN conditions** (`on`) are applied during the JOIN and can use indexes
- **WHERE conditions** are applied after the JOIN and may be less efficient
- **Nested JOINs** should be used carefully to avoid cartesian products

#### Error Handling

```javascript
// Relation not found
processJoins(query, userModel, { nonexistent: true }, relations)
// Throws: "Relation 'nonexistent' not found in relations config"

// Invalid projection in nested join
processJoins(
  query,
  userModel,
  {
    posts: {
      join: { author: { projection: 'invalid' } }
    }
  },
  relations
)
// Throws: "Projection 'invalid' not found in model"
```

---

### buildMakeTransaction

**Transaction factory with automatic rollback on errors.**

```javascript
buildMakeTransaction(knexInstance)
```

#### Parameters

| Parameter      | Type   | Description               |
| -------------- | ------ | ------------------------- |
| `knexInstance` | `Knex` | Knex.js database instance |

#### Returns

Function that accepts a callback for transaction operations.

#### Examples

**Basic Transaction**

```javascript
const makeTransaction = buildMakeTransaction(knex)

const result = await makeTransaction(async trx => {
  const user = await trx('users').insert({ name: 'John' }).returning('*')
  await trx('posts').insert({ user_id: user[0].id, title: 'Hello' })
  return user[0]
})
```

**Error Handling**

```javascript
try {
  await makeTransaction(async trx => {
    await trx('users').insert({ name: 'John' })
    throw new Error('Something went wrong')
    await trx('posts').insert({ title: 'Never executed' })
  })
} catch (error) {
  // Transaction automatically rolled back
  console.log('Transaction failed:', error.message)
}
```

---

## Operators Reference

### Comparison Operators

| Operator    | SQL Equivalent | Example                                        |
| ----------- | -------------- | ---------------------------------------------- |
| `equals`    | `=`            | `{ age: { equals: 25 } }`                      |
| `not`       | `!=`           | `{ status: { not: 'deleted' } }`               |
| `gt`        | `>`            | `{ age: { gt: 18 } }`                          |
| `gte`       | `>=`           | `{ age: { gte: 21 } }`                         |
| `lt`        | `<`            | `{ age: { lt: 65 } }`                          |
| `lte`       | `<=`           | `{ score: { lte: 100 } }`                      |
| `in`        | `IN`           | `{ role: { in: ['admin', 'user'] } }`          |
| `notIn`     | `NOT IN`       | `{ status: { notIn: ['deleted', 'banned'] } }` |
| `isNull`    | `IS NULL`      | `{ deletedAt: { isNull: true } }`              |
| `isNotNull` | `IS NOT NULL`  | `{ email: { isNotNull: true } }`               |

### String Operators

| Operator     | SQL Equivalent | Example                                   |
| ------------ | -------------- | ----------------------------------------- |
| `contains`   | `LIKE %value%` | `{ name: { contains: 'John' } }`          |
| `startsWith` | `LIKE value%`  | `{ email: { startsWith: 'admin' } }`      |
| `endsWith`   | `LIKE %value`  | `{ email: { endsWith: '@company.com' } }` |

### Logical Operators

| Operator | Description                 | Example                                             |
| -------- | --------------------------- | --------------------------------------------------- |
| `AND`    | All conditions must be true | `{ AND: [{ age: { gte: 18 } }, { active: true }] }` |
| `OR`     | Any condition must be true  | `{ OR: [{ role: 'admin' }, { role: 'owner' }] }`    |

### Special Operators

| Operator     | Description              | Example                                                   |
| ------------ | ------------------------ | --------------------------------------------------------- |
| `_exists`    | Subquery existence check | `{ _exists: { posts: { published: true } } }`             |
| `_condition` | Conditional application  | `{ role: { equals: 'admin', _condition: user.isOwner } }` |

### Default Behavior

- **No operator specified**: Defaults to `equals`
- **Null values**: Automatically use `isNull`
- **Arrays**: Automatically use `in`

```javascript
// These are equivalent
{ age: 25 }
{ age: { equals: 25 } }

// These are equivalent
{ deletedAt: null }
{ deletedAt: { isNull: true } }

// These are equivalent
{ role: ['admin', 'owner'] }
{ role: { in: ['admin', 'owner'] } }
```

---

## Model Structure

### Model Definition

```javascript
const userModel = {
  // Required properties
  tableName: 'users',
  alias: 'u',

  // Projections - functions that return column arrays (REQUIRED for buildQuery)
  projections: {
    details: (_, alias, relationName = null) => [
      `${alias}.id`,
      `${alias}.name`,
      `${alias}.email`,
      `${alias}.role`,
      `${alias}.created_at`
    ],
    summary: (_, alias, relationName = null) => [
      `${alias}.id`,
      `${alias}.name`
    ],
    withStats: (knexInstanceOrQuery, alias, relationName = null) => [
      `${alias}.id`,
      `${alias}.name`,
      knexInstanceOrQuery.raw(`COUNT(posts.id) as post_count`)
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
    profile: {
      type: 'belongsTo',
      model: 'profile',
      table: 'profiles',
      foreignKey: 'profile_id',
      primaryKey: 'id',
      modelDefinition: () => require('./profile.model')
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
    // Automatic application (horizontal partitioning)
    default: (query, knexInstance, alias) => {
      query.where(`${alias}.active`, true).whereNull(`${alias}.deleted_at`)
    },

    // Parameterized modifiers
    forRole: (query, knexInstance, alias, { role }) => {
      query.where(`${alias}.role`, role)
    },

    createdAfter: (query, knexInstance, alias, { date }) => {
      query.where(`${alias}.created_at`, '>=', date)
    },

    withMinPosts: (query, knexInstance, alias, { minCount }) => {
      query
        .join('posts', `${alias}.id`, 'posts.user_id')
        .groupBy(`${alias}.id`)
        .having(knexInstance.raw('COUNT(posts.id)'), '>=', minCount)
    }
  }
}
```

### Relation Types

#### belongsTo

```javascript
profile: {
  type: 'belongsTo',
  model: 'profile',
  table: 'profiles',
  foreignKey: 'profile_id',  // This table's foreign key
  primaryKey: 'id',          // Related table's primary key
  modelDefinition: () => require('./profile.model')
}
```

#### hasMany

```javascript
posts: {
  type: 'hasMany',
  model: 'post',
  table: 'posts',
  foreignKey: 'user_id',     // Related table's foreign key
  primaryKey: 'id',          // This table's primary key
  modelDefinition: () => require('./post.model')
}
```

#### manyToMany

```javascript
tags: {
  type: 'manyToMany',
  model: 'tag',
  table: 'tags',
  through: {
    table: 'user_tags',      // Junction table
    alias: 'ut',             // Junction table alias (required)
    foreignKey: 'user_id',   // This table's key in junction
    otherKey: 'tag_id'       // Other table's key in junction
  },
  primaryKey: 'id',          // Optional, defaults to 'id'
  modelDefinition: () => require('./tag.model')
}
```

**Generated SQL:**

```sql
LEFT JOIN user_tags as ut ON u.id = ut.user_id
LEFT JOIN tags as t ON ut.tag_id = t.id
```

### Self-Referencing Relations

```javascript
// Parent-child relationships
const categoryModel = {
  tableName: 'categories',
  alias: 'c',
  relations: {
    parent: {
      type: 'belongsTo',
      model: 'category',
      table: 'categories',
      foreignKey: 'parent_id',
      primaryKey: 'id',
      modelDefinition: () => categoryModel
    },
    children: {
      type: 'hasMany',
      model: 'category',
      table: 'categories',
      foreignKey: 'parent_id',
      primaryKey: 'id',
      modelDefinition: () => categoryModel
    }
  }
}
```

---

## Error Handling

### Common Errors

**Invalid Projection**

```javascript
// Error: Projection 'invalid' not found in model
await buildQuery(knex, userModel, {
  projection: 'invalid'
})
```

**Invalid Modifier**

```javascript
// Error: Modifier 'nonexistent' not found in model
await buildQuery(knex, userModel, {
  modifiers: {
    nonexistent: { param: 'value' }
  }
})
```

**Missing Relations for \_exists**

```javascript
// Error: Relations must be provided to use _exists functionality
applyWhereClauses(query, 'users', {
  where: {
    _exists: { posts: { published: true } }
  }
  // Missing relations parameter
})
```

**Invalid Relation**

```javascript
// Error: Relation 'invalid' not found in relations config
applyWhereClauses(
  query,
  'users',
  {
    where: {
      _exists: { invalid: { active: true } }
    }
  },
  userModel.relations
)
```

### Best Practices

1. **Always validate projections** exist before using
2. **Include relations parameter** when using `_exists` operator
3. **Use meaningful aliases** to avoid SQL conflicts
4. **Test modifiers thoroughly** with edge cases
5. **Handle async operations** with proper error catching

```javascript
try {
  const results = await buildQuery(knex, userModel, queryConfig)
  return results
} catch (error) {
  console.error('Query failed:', error.message)
  throw new Error('Failed to fetch users')
}
```

---

## Fluent API

The Fluent API provides a chainable, type-safe interface for building queries. It offers an alternative to plain object configuration with improved developer experience through method chaining and IDE autocomplete support.

### WhereBuilder

**Fluent API for building where conditions with method chaining.**

```javascript
const { where } = require('knex-tools')
```

The `WhereBuilder` class provides a fluent interface for constructing complex where conditions that can be used with `query()`, `countQuery()`, and `existsQuery()` builders.

#### Factory Function

```javascript
where()
```

Creates a new `WhereBuilder` instance.

**Returns:** `WhereBuilder` instance

**Example:**

```javascript
const condition = where().equals('role', 'admin').gte('age', 18).build()

// Result: { role: 'admin', age: { gte: 18 } }
```

---

#### Comparison Operators

##### `.equals(field, value)`

Adds an equality condition.

**Parameters:**

- `field` (string): Field name
- `value` (any): Value to match

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().equals('role', 'admin').build()
// { role: 'admin' }
```

##### `.not(field, value)`

Adds a not-equal condition.

**Parameters:**

- `field` (string): Field name
- `value` (any): Value to exclude

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().not('status', 'deleted').build()
// { status: { not: 'deleted' } }
```

##### `.gt(field, value)`

Adds a greater-than condition.

**Parameters:**

- `field` (string): Field name
- `value` (number): Minimum value (exclusive)

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().gt('age', 18).build()
// { age: { gt: 18 } }
```

##### `.gte(field, value)`

Adds a greater-than-or-equal condition.

**Parameters:**

- `field` (string): Field name
- `value` (number): Minimum value (inclusive)

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().gte('age', 18).build()
// { age: { gte: 18 } }
```

##### `.lt(field, value)`

Adds a less-than condition.

**Parameters:**

- `field` (string): Field name
- `value` (number): Maximum value (exclusive)

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().lt('age', 65).build()
// { age: { lt: 65 } }
```

##### `.lte(field, value)`

Adds a less-than-or-equal condition.

**Parameters:**

- `field` (string): Field name
- `value` (number): Maximum value (inclusive)

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().lte('age', 65).build()
// { age: { lte: 65 } }
```

##### `.contains(field, value)`

Adds a string contains condition (case-insensitive on PostgreSQL).

**Parameters:**

- `field` (string): Field name
- `value` (string): Substring to search for

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().contains('email', 'example').build()
// { email: { contains: 'example' } }
```

##### `.startsWith(field, value)`

Adds a string starts-with condition (case-insensitive on PostgreSQL).

**Parameters:**

- `field` (string): Field name
- `value` (string): Prefix to match

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().startsWith('name', 'John').build()
// { name: { startsWith: 'John' } }
```

##### `.endsWith(field, value)`

Adds a string ends-with condition (case-insensitive on PostgreSQL).

**Parameters:**

- `field` (string): Field name
- `value` (string): Suffix to match

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().endsWith('email', '@example.com').build()
// { email: { endsWith: '@example.com' } }
```

##### `.in(field, values)`

Adds an IN condition.

**Parameters:**

- `field` (string): Field name
- `values` (Array): Array of values to match

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().in('role', ['admin', 'manager']).build()
// { role: { in: ['admin', 'manager'] } }
```

##### `.notIn(field, values)`

Adds a NOT IN condition.

**Parameters:**

- `field` (string): Field name
- `values` (Array): Array of values to exclude

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().notIn('status', ['deleted', 'banned']).build()
// { status: { notIn: ['deleted', 'banned'] } }
```

##### `.isNull(field)`

Adds an IS NULL condition.

**Parameters:**

- `field` (string): Field name

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().isNull('deleted_at').build()
// { deleted_at: { isNull: true } }
```

##### `.isNotNull(field)`

Adds an IS NOT NULL condition.

**Parameters:**

- `field` (string): Field name

**Returns:** `this` (for chaining)

**Example:**

```javascript
where().isNotNull('email').build()
// { email: { isNotNull: true } }
```

---

#### Chaining Multiple Conditions

Multiple conditions on different fields are combined with AND logic:

```javascript
where().equals('role', 'admin').gte('age', 18).isNotNull('email').build()
// Result: { role: 'admin', age: { gte: 18 }, email: { isNotNull: true } }
```

Multiple operators on the same field are merged:

```javascript
where().gte('age', 18).lte('age', 65).build()
// Result: { age: { gte: 18, lte: 65 } }
```

---

#### Logical Operators

##### `.and(callback)`

Adds an AND block with nested conditions.

**Parameters:**

- `callback` (Function): Function that receives a `WhereBuilder` instance

**Returns:** `this` (for chaining)

**Example:**

```javascript
where()
  .equals('active', true)
  .and(and => and.equals('verified', true).gte('age', 18))
  .build()
// Result: { active: true, AND: [{ verified: true, age: { gte: 18 } }] }
```

**Multiple AND blocks:**

```javascript
where()
  .and(a1 => a1.equals('verified', true))
  .and(a2 => a2.gte('age', 18))
  .build()
// Result: { AND: [{ verified: true }, { age: { gte: 18 } }] }
```

##### `.or(callback)`

Adds an OR block with nested conditions.

**Parameters:**

- `callback` (Function): Function that receives a `WhereBuilder` instance

**Returns:** `this` (for chaining)

**Example:**

```javascript
where()
  .equals('active', true)
  .or(or => or.equals('role', 'admin').contains('email', '@admin.com'))
  .build()
// Result: { active: true, OR: [{ role: 'admin', email: { contains: '@admin.com' } }] }
```

**Multiple OR blocks:**

```javascript
where()
  .or(or1 => or1.equals('role', 'admin'))
  .or(or2 => or2.equals('role', 'manager'))
  .build()
// Result: { OR: [{ role: 'admin' }, { role: 'manager' }] }
```

**Combining AND and OR:**

```javascript
where()
  .equals('active', true)
  .or(or => or.equals('role', 'admin'))
  .and(and => and.gte('age', 18))
  .build()
// Result: {
//   active: true,
//   OR: [{ role: 'admin' }],
//   AND: [{ age: { gte: 18 } }]
// }
```

---

#### Exists Operator

##### `.exists(relationName, callback)`

Adds an exists condition for row-level security filtering. Requires model with relations.

**Parameters:**

- `relationName` (string): Name of the relation
- `callback` (Function): Function that receives a `WhereBuilder` instance

**Returns:** `this` (for chaining)

**Example:**

```javascript
where()
  .exists('posts', posts => posts.equals('published', true))
  .build()
// Result: { _exists: { posts: { published: true } } }
```

**Multiple exists blocks:**

```javascript
where()
  .exists('posts', p => p.equals('published', true))
  .exists('comments', c => c.equals('approved', true))
  .build()
// Result: {
//   _exists: {
//     posts: { published: true },
//     comments: { approved: true }
//   }
// }
```

**With complex conditions:**

```javascript
where()
  .exists('memberships', m =>
    m.equals('organization_id', 1).in('role', ['admin', 'owner'])
  )
  .build()
// Result: {
//   _exists: {
//     memberships: {
//       organization_id: 1,
//       role: { in: ['admin', 'owner'] }
//     }
//   }
// }
```

---

#### Conditional Building

##### `.when(condition, callback)`

Conditionally applies conditions based on a boolean predicate.

**Parameters:**

- `condition` (boolean): If true, applies the callback
- `callback` (Function): Function that receives `this` WhereBuilder instance

**Returns:** `this` (for chaining)

**Example:**

```javascript
const userRole = 'admin'

where()
  .equals('active', true)
  .when(userRole === 'admin', w => w.isNotNull('admin_at'))
  .when(userRole === 'guest', w => w.isNull('verified_at'))
  .build()
// Result: { active: true, admin_at: { isNotNull: true } }
```

**With multiple conditions:**

```javascript
const filters = { showDeleted: false, minAge: 18 }

where()
  .equals('active', true)
  .when(!filters.showDeleted, w => w.isNull('deleted_at'))
  .when(filters.minAge, w => w.gte('age', filters.minAge))
  .build()
// Result: {
//   active: true,
//   deleted_at: { isNull: true },
//   age: { gte: 18 }
// }
```

---

#### Raw Where Object

##### `.raw(whereObject)`

Merges a raw where object into the conditions.

**Parameters:**

- `whereObject` (Object): Plain where object to merge

**Returns:** `this` (for chaining)

**Example:**

```javascript
where()
  .equals('active', true)
  .raw({
    role: 'admin',
    age: { gte: 18 },
    OR: [{ verified: true }, { trusted: true }]
  })
  .build()
// Result: {
//   active: true,
//   role: 'admin',
//   age: { gte: 18 },
//   OR: [{ verified: true }, { trusted: true }]
// }
```

---

#### Build Method

##### `.build()`

Returns the constructed where object.

**Returns:** `Object` - Where conditions object

**Example:**

```javascript
const conditions = where().equals('role', 'admin').gte('age', 18).build()

console.log(conditions)
// { role: 'admin', age: { gte: 18 } }
```

---

#### Complete Examples

**Complex filtering:**

```javascript
const { where } = require('knex-tools')

const filters = where()
  .equals('active', true)
  .in('role', ['admin', 'manager'])
  .gte('created_at', '2024-01-01')
  .or(or =>
    or.contains('email', '@company.com').contains('email', '@partner.com')
  )
  .and(and => and.gte('age', 18).lte('age', 65))
  .build()
```

**Row-level security:**

```javascript
const userFilters = where()
  .exists('memberships', m =>
    m.equals('user_id', currentUserId).equals('active', true)
  )
  .exists('permissions', p => p.in('action', ['read', 'write']))
  .build()
```

**Dynamic filters:**

```javascript
function buildUserFilters(options) {
  const w = where()

  if (options.role) {
    w.equals('role', options.role)
  }

  if (options.minAge) {
    w.gte('age', options.minAge)
  }

  if (options.search) {
    w.or(or =>
      or.contains('name', options.search).contains('email', options.search)
    )
  }

  return w.build()
}

const filters = buildUserFilters({
  role: 'admin',
  minAge: 18,
  search: 'john'
})
```

---

### QueryBuilder

**Fluent API for building buildQuery configurations with method chaining.**

```javascript
const { query } = require('knex-tools')
```

The `QueryBuilder` class provides a fluent interface for constructing `buildQuery` configurations with support for projections, filtering, sorting, pagination, relations, and metadata.

#### Factory Function

```javascript
query(modelObject)
```

Creates a new `QueryBuilder` instance for the given model.

**Parameters:**

- `modelObject` (Object): Model definition with projections and relations

**Returns:** `QueryBuilder` instance

**Example:**

```javascript
const userQuery = query(userModel)
  .projection('details')
  .where(w => w.equals('active', true))
  .orderBy('created_at', 'desc')
  .take(10)

const config = userQuery.build()
// Or execute directly
const result = await userQuery.execute(knex)
```

---

#### Basic Methods

##### `.projection(name)`

Sets the projection to use for the query.

**Parameters:**

- `name` (string): Projection name from model.projections

**Returns:** `this` (for chaining)

**Example:**

```javascript
query(userModel).projection('details').build()
// { projection: 'details' }
```

##### `.where(callbackOrObject)`

Adds where conditions using either a fluent callback or plain object.

**Parameters:**

- `callbackOrObject` (Function | Object): WhereBuilder callback or plain where object

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// With fluent callback
query(userModel)
  .where(w => w.equals('role', 'admin').gte('age', 18))
  .build()
// { where: { role: 'admin', age: { gte: 18 } } }

// With plain object
query(userModel).where({ role: 'admin', active: true }).build()
// { where: { role: 'admin', active: true } }

// Multiple where calls are merged
query(userModel).where({ role: 'admin' }).where({ active: true }).build()
// { where: { role: 'admin', active: true } }
```

---

#### Sorting and Pagination

##### `.orderBy(fieldOrObject, direction)`

Adds sorting configuration.

**Parameters:**

- `fieldOrObject` (string | Object): Field name or object with field-direction pairs
- `direction` (string): Sort direction ('asc' or 'desc'), defaults to 'asc'

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// Single field
query(userModel).orderBy('created_at', 'desc').build()
// { orderBy: { created_at: 'desc' } }

// Default direction (asc)
query(userModel).orderBy('name').build()
// { orderBy: { name: 'asc' } }

// Multiple fields with chaining
query(userModel).orderBy('role', 'asc').orderBy('name', 'desc').build()
// { orderBy: { role: 'asc', name: 'desc' } }

// Object syntax
query(userModel).orderBy({ role: 'asc', created_at: 'desc' }).build()
// { orderBy: { role: 'asc', created_at: 'desc' } }

// Merge multiple orderBy calls
query(userModel).orderBy({ name: 'asc' }).orderBy({ age: 'desc' }).build()
// { orderBy: { name: 'asc', age: 'desc' } }
```

##### `.take(limit)`

Sets the maximum number of records to return (LIMIT).

**Parameters:**

- `limit` (number): Maximum number of records

**Returns:** `this` (for chaining)

**Example:**

```javascript
query(userModel).take(10).build()
// { take: 10 }
```

##### `.skip(offset)`

Sets the number of records to skip (OFFSET).

**Parameters:**

- `offset` (number): Number of records to skip

**Returns:** `this` (for chaining)

**Example:**

```javascript
query(userModel).skip(20).build()
// { skip: 20 }

// Pagination example
query(userModel).skip(20).take(10).build()
// { skip: 20, take: 10 }
// Page 3 of results (assuming 10 per page)
```

---

#### Relations

##### `.with(relationName, callbackOrProjection)`

Includes a related resource in the query results.

**Parameters:**

- `relationName` (string): Name of the relation from model.relations
- `callbackOrProjection` (Function | string | undefined):
  - Function: Nested QueryBuilder callback
  - String: Projection name
  - Undefined: Uses default 'details' projection

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// Simple projection string
query(userModel).with('posts', 'summary').build()
// { each: { posts: { projection: 'summary' } } }

// Default projection
query(userModel).with('posts').build()
// { each: { posts: { projection: 'details' } } }

// Nested builder callback
query(userModel)
  .with('posts', q =>
    q
      .projection('details')
      .where(w => w.equals('published', true))
      .orderBy('created_at', 'desc')
      .take(5)
  )
  .build()
// {
//   each: {
//     posts: {
//       projection: 'details',
//       where: { published: true },
//       orderBy: { created_at: 'desc' },
//       take: 5
//     }
//   }
// }

// Multiple relations
query(userModel).with('posts', 'summary').with('folders', 'short').build()
// {
//   each: {
//     posts: { projection: 'summary' },
//     folders: { projection: 'short' }
//   }
// }
```

##### `.withCounts(relationName, whereCallbackOrBoolean)`

Adds related record counts per parent record.

**Parameters:**

- `relationName` (string): Name of the relation from model.relations
- `whereCallbackOrBoolean` (Function | Object | boolean):
  - Function: WhereBuilder callback for filtering
  - Object: Plain where object for filtering
  - Boolean: true to count all (default)

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// Count all related records
query(userModel).withCounts('posts', true).build()
// { withRelatedCounts: { posts: true } }

// Default (count all)
query(userModel).withCounts('posts').build()
// { withRelatedCounts: { posts: true } }

// With where object
query(userModel).withCounts('posts', { published: true }).build()
// { withRelatedCounts: { posts: { published: true } } }

// With fluent where callback
query(userModel)
  .withCounts('posts', w =>
    w.equals('published', true).gte('created_at', '2024-01-01')
  )
  .build()
// {
//   withRelatedCounts: {
//     posts: {
//       where: {
//         published: true,
//         created_at: { gte: '2024-01-01' }
//       }
//     }
//   }
// }

// Multiple withCounts
query(userModel)
  .withCounts('posts', { published: true })
  .withCounts('folders')
  .build()
// {
//   withRelatedCounts: {
//     posts: { published: true },
//     folders: true
//   }
// }
```

---

#### Modifiers and Metadata

##### `.modifier(nameOrObject, params)`

Applies named modifiers from the model.

**Parameters:**

- `nameOrObject` (string | Object): Modifier name or object with modifier-params pairs
- `params` (Object): Parameters to pass to the modifier (when using string syntax)

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// Single modifier
query(userModel).modifier('forRole', { role: 'admin' }).build()
// { modifiers: { forRole: { role: 'admin' } } }

// Modifier without params
query(userModel).modifier('activeOnly').build()
// { modifiers: { activeOnly: {} } }

// Multiple modifiers with chaining
query(userModel)
  .modifier('forRole', { role: 'admin' })
  .modifier('withMinPosts', { minCount: 10 })
  .build()
// {
//   modifiers: {
//     forRole: { role: 'admin' },
//     withMinPosts: { minCount: 10 }
//   }
// }

// Object syntax
query(userModel)
  .modifier({
    forRole: { role: 'admin' },
    activeOnly: {}
  })
  .build()
// {
//   modifiers: {
//     forRole: { role: 'admin' },
//     activeOnly: {}
//   }
// }
```

##### `.metadata(options)`

Sets metadata options for the query.

**Parameters:**

- `options` (Object): Metadata configuration

**Returns:** `this` (for chaining)

**Example:**

```javascript
query(userModel)
  .metadata({
    counts: { total: true, filtered: true }
  })
  .build()
// { metadata: { counts: { total: true, filtered: true } } }
```

##### `.counts(options)`

Shorthand for setting metadata counts.

**Parameters:**

- `options` (Object): Counts configuration

**Returns:** `this` (for chaining)

**Examples:**

```javascript
query(userModel).counts({ total: true, filtered: true }).build()
// { metadata: { counts: { total: true, filtered: true } } }

// Merges with existing metadata
query(userModel).metadata({ foo: 'bar' }).counts({ total: true }).build()
// { metadata: { foo: 'bar', counts: { total: true } } }
```

---

#### Build and Execute

##### `.build()`

Returns the constructed query configuration object.

**Returns:** `Object` - Query configuration for buildQuery

**Example:**

```javascript
const config = query(userModel)
  .projection('details')
  .where(w => w.equals('active', true))
  .orderBy('created_at', 'desc')
  .take(10)
  .build()

// Use with buildQuery
const result = await buildQuery(knex, userModel, config)
```

##### `.execute(knexInstance)`

Executes the query immediately by calling `buildQuery`.

**Parameters:**

- `knexInstance` (Knex): Knex database instance

**Returns:** `Promise<Object>` - Query results

**Example:**

```javascript
const result = await query(userModel)
  .projection('details')
  .where(w => w.equals('role', 'admin'))
  .execute(knex)

// Result: { data: [...], metadata: {...} }
```

---

#### Complete Examples

**Basic query with filtering, sorting, and pagination:**

```javascript
const result = await query(userModel)
  .projection('details')
  .where(w =>
    w.equals('active', true).in('role', ['admin', 'manager']).gte('age', 18)
  )
  .orderBy('created_at', 'desc')
  .skip(20)
  .take(10)
  .execute(knex)
```

**Nested data fetching with relations:**

```javascript
const result = await query(postModel)
  .projection('details')
  .with('author', 'profile')
  .with('comments', q =>
    q
      .projection('summary')
      .where(w => w.equals('approved', true))
      .orderBy('created_at', 'desc')
      .take(5)
  )
  .with('tags', 'name')
  .execute(knex)
```

**With metadata counts:**

```javascript
const result = await query(userModel)
  .projection('details')
  .where(w => w.equals('active', true))
  .counts({ total: true, filtered: true })
  .execute(knex)

// Result:
// {
//   data: [...],
//   metadata: { counts: { total: 1000, filtered: 250 } }
// }
```

**With related counts:**

```javascript
const result = await query(userModel)
  .projection('details')
  .withCounts('posts', { published: true })
  .withCounts('folders')
  .with('posts', q => q.projection('summary').withCounts('comments'))
  .execute(knex)

// Result:
// {
//   data: [
//     {
//       id: 1,
//       name: 'John',
//       _counts: { posts: 10, folders: 3 },
//       posts: {
//         data: [
//           { id: 1, title: 'Post 1', _counts: { comments: 5 } }
//         ]
//       }
//     }
//   ]
// }
```

**Using modifiers:**

```javascript
const result = await query(userModel)
  .projection('details')
  .modifier('forRole', { role: 'admin' })
  .modifier('withMinPosts', { minCount: 10 })
  .execute(knex)
```

**Dynamic query building:**

```javascript
function buildUserQuery(filters) {
  const q = query(userModel).projection('details')

  if (filters.role) {
    q.modifier('forRole', { role: filters.role })
  }

  if (filters.search) {
    q.where(w =>
      w.or(or =>
        or.contains('name', filters.search).contains('email', filters.search)
      )
    )
  }

  if (filters.sortBy) {
    q.orderBy(filters.sortBy, filters.sortDir || 'asc')
  }

  q.skip(filters.offset || 0).take(filters.limit || 10)

  if (filters.includeCounts) {
    q.counts({ total: true, filtered: true })
  }

  return q
}

const result = await buildUserQuery({
  role: 'admin',
  search: 'john',
  sortBy: 'created_at',
  sortDir: 'desc',
  offset: 20,
  limit: 10,
  includeCounts: true
}).execute(knex)
```

---

### CountsBuilder

**Fluent API for building counts query configurations with method chaining.**

```javascript
const { countQuery } = require('knex-tools')
```

The `CountsBuilder` class provides a fluent interface for constructing `counts` query configurations for lightweight count-only queries without fetching data.

#### Factory Function

```javascript
countQuery(modelObject)
```

Creates a new `CountsBuilder` instance for the given model.

**Parameters:**

- `modelObject` (Object): Model definition

**Returns:** `CountsBuilder` instance

**Example:**

```javascript
const userCounts = countQuery(userModel)
  .where(w => w.equals('active', true))
  .total()
  .filtered()

const config = userCounts.build()
// Or execute directly
const result = await userCounts.execute(knex)
// Returns: { total: 1000, filtered: 250 }
```

---

#### Count Methods

##### `.total()`

Includes total count (all records in table) in the results.

**Returns:** `this` (for chaining)

**Example:**

```javascript
countQuery(userModel).total().build()
// { counts: { total: true } }
```

##### `.filtered()`

Includes filtered count (records matching where conditions) in the results.

**Returns:** `this` (for chaining)

**Example:**

```javascript
countQuery(userModel).filtered().build()
// { counts: { filtered: true } }
```

**Combined example:**

```javascript
countQuery(userModel).total().filtered().build()
// { counts: { total: true, filtered: true } }
```

---

#### Filtering

##### `.where(callbackOrObject)`

Adds where conditions using either a fluent callback or plain object.

**Parameters:**

- `callbackOrObject` (Function | Object): WhereBuilder callback or plain where object

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// With fluent callback
countQuery(userModel)
  .where(w => w.equals('role', 'admin').gte('age', 18))
  .total()
  .filtered()
  .build()
// {
//   where: { role: 'admin', age: { gte: 18 } },
//   counts: { total: true, filtered: true }
// }

// With plain object
countQuery(userModel).where({ role: 'admin', active: true }).filtered().build()
// {
//   where: { role: 'admin', active: true },
//   counts: { filtered: true }
// }

// Multiple where calls are merged
countQuery(userModel)
  .where({ role: 'admin' })
  .where({ active: true })
  .total()
  .build()
// {
//   where: { role: 'admin', active: true },
//   counts: { total: true }
// }
```

---

#### Modifiers

##### `.modifier(nameOrObject, params)`

Applies named modifiers to create custom count queries.

**Parameters:**

- `nameOrObject` (string | Object): Modifier name or object with modifier-params pairs
- `params` (Object): Parameters to pass to the modifier (when using string syntax)

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// Single modifier
countQuery(userModel)
  .total()
  .filtered()
  .modifier('inactive', { active: false })
  .build()
// {
//   counts: {
//     total: true,
//     filtered: true,
//     modifiers: { inactive: { active: false } }
//   }
// }

// Modifier without params
countQuery(userModel).total().modifier('activeOnly').build()
// {
//   counts: {
//     total: true,
//     modifiers: { activeOnly: {} }
//   }
// }

// Multiple modifiers with chaining
countQuery(userModel)
  .total()
  .modifier('inactive', { active: false })
  .modifier('verified', { verified: true })
  .build()
// {
//   counts: {
//     total: true,
//     modifiers: {
//       inactive: { active: false },
//       verified: { verified: true }
//     }
//   }
// }

// Object syntax
countQuery(userModel)
  .total()
  .modifier({
    inactive: { active: false },
    verified: { verified: true }
  })
  .build()
// {
//   counts: {
//     total: true,
//     modifiers: {
//       inactive: { active: false },
//       verified: { verified: true }
//     }
//   }
// }
```

---

#### Build and Execute

##### `.build()`

Returns the constructed counts configuration object.

**Returns:** `Object` - Counts configuration for counts function

**Example:**

```javascript
const config = countQuery(userModel)
  .where(w => w.equals('role', 'admin'))
  .total()
  .filtered()
  .build()

// Use with counts function
const result = await counts(knex, userModel, config)
```

##### `.execute(knexInstance)`

Executes the count query immediately by calling `counts`.

**Parameters:**

- `knexInstance` (Knex): Knex database instance

**Returns:** `Promise<Object>` - Count results

**Example:**

```javascript
const result = await countQuery(userModel)
  .where(w => w.equals('role', 'admin'))
  .total()
  .filtered()
  .execute(knex)

// Result: { total: 1000, filtered: 50 }
```

---

#### Complete Examples

**Basic counts:**

```javascript
const result = await countQuery(userModel).total().filtered().execute(knex)

// Result: { total: 1000, filtered: 1000 }
```

**With filtering:**

```javascript
const result = await countQuery(userModel)
  .where(w =>
    w
      .equals('active', true)
      .in('role', ['admin', 'manager'])
      .gte('created_at', '2024-01-01')
  )
  .total()
  .filtered()
  .execute(knex)

// Result: { total: 1000, filtered: 150 }
```

**With custom modifier counts:**

```javascript
const result = await countQuery(userModel)
  .where(w => w.gte('age', 18))
  .total()
  .filtered()
  .modifier('inactive', { active: false })
  .modifier('verified', { verified: true })
  .execute(knex)

// Result: { total: 1000, filtered: 800, inactive: 200, verified: 600 }
```

**Conditional counting:**

```javascript
function getCountStats(filters) {
  const q = countQuery(userModel).total().filtered()

  if (filters.role) {
    q.where(w => w.equals('role', filters.role))
  }

  if (filters.minAge) {
    q.where(w => w.gte('age', filters.minAge))
  }

  if (filters.includeInactive) {
    q.modifier('inactive', { active: false })
  }

  return q
}

const result = await getCountStats({
  role: 'admin',
  minAge: 18,
  includeInactive: true
}).execute(knex)

// Result: { total: 1000, filtered: 50, inactive: 10 }
```

**Using with plain where object:**

```javascript
const result = await countQuery(userModel)
  .where({ role: 'admin', active: true })
  .total()
  .filtered()
  .execute(knex)

// Result: { total: 1000, filtered: 45 }
```

**Complex filtering:**

```javascript
const result = await countQuery(userModel)
  .where(w =>
    w
      .equals('verified', true)
      .or(or =>
        or.contains('email', '@company.com').contains('email', '@partner.com')
      )
      .and(and => and.gte('age', 18).lte('age', 65))
  )
  .total()
  .filtered()
  .execute(knex)

// Result: { total: 1000, filtered: 120 }
```

---

### ExistsBuilder

**Fluent API for building exists query configurations with method chaining.**

```javascript
const { existsQuery } = require('knex-tools')
```

The `ExistsBuilder` class provides a fluent interface for constructing `exists` query configurations for lightweight existence checks without fetching data. Uses `SELECT 1 LIMIT 1` internally for optimal performance.

#### Factory Function

```javascript
existsQuery(modelObject)
```

Creates a new `ExistsBuilder` instance for the given model.

**Parameters:**

- `modelObject` (Object): Model definition

**Returns:** `ExistsBuilder` instance

**Example:**

```javascript
const hasAdmins = existsQuery(userModel).where(w => w.equals('role', 'admin'))

const config = hasAdmins.build()
// Or execute directly
const result = await hasAdmins.execute(knex)
// Returns: true or false
```

---

#### Filtering

##### `.where(callbackOrObject)`

Adds where conditions using either a fluent callback or plain object.

**Parameters:**

- `callbackOrObject` (Function | Object): WhereBuilder callback or plain where object

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// With fluent callback
existsQuery(userModel)
  .where(w => w.equals('role', 'admin').gte('age', 18))
  .build()
// { where: { role: 'admin', age: { gte: 18 } } }

// With plain object
existsQuery(userModel).where({ role: 'admin', active: true }).build()
// { where: { role: 'admin', active: true } }

// Multiple where calls are merged
existsQuery(userModel)
  .where({ role: 'admin' })
  .where({ active: true })
  .where({ verified: true })
  .build()
// { where: { role: 'admin', active: true, verified: true } }
```

---

#### Modifiers

##### `.modifier(nameOrObject, params)`

Applies named modifiers from the model.

**Parameters:**

- `nameOrObject` (string | Object): Modifier name or object with modifier-params pairs
- `params` (Object): Parameters to pass to the modifier (when using string syntax)

**Returns:** `this` (for chaining)

**Examples:**

```javascript
// Single modifier
existsQuery(userModel).modifier('forRole', { role: 'admin' }).build()
// { modifiers: { forRole: { role: 'admin' } } }

// Modifier without params
existsQuery(userModel).modifier('activeOnly').build()
// { modifiers: { activeOnly: {} } }

// Multiple modifiers with chaining
existsQuery(userModel)
  .modifier('forRole', { role: 'admin' })
  .modifier('activeOnly')
  .build()
// {
//   modifiers: {
//     forRole: { role: 'admin' },
//     activeOnly: {}
//   }
// }

// Object syntax
existsQuery(userModel)
  .modifier({
    forRole: { role: 'admin' },
    activeOnly: {}
  })
  .build()
// {
//   modifiers: {
//     forRole: { role: 'admin' },
//     activeOnly: {}
//   }
// }

// Combining where and modifiers
existsQuery(userModel)
  .where(w => w.equals('active', true))
  .modifier('forRole', { role: 'admin' })
  .build()
// {
//   where: { active: true },
//   modifiers: { forRole: { role: 'admin' } }
// }
```

---

#### Build and Execute

##### `.build()`

Returns the constructed exists configuration object.

**Returns:** `Object` - Exists configuration for exists function

**Example:**

```javascript
const config = existsQuery(userModel)
  .where(w => w.equals('role', 'admin'))
  .build()

// Use with exists function
const result = await exists(knex, userModel, config)
```

##### `.execute(knexInstance)`

Executes the existence check immediately by calling `exists`.

**Parameters:**

- `knexInstance` (Knex): Knex database instance

**Returns:** `Promise<boolean>` - true if records exist, false otherwise

**Example:**

```javascript
const hasAdmins = await existsQuery(userModel)
  .where(w => w.equals('role', 'admin'))
  .execute(knex)

// Returns: true or false
```

---

#### Complete Examples

**Basic existence check:**

```javascript
const hasUsers = await existsQuery(userModel).execute(knex)

// Returns: true (if any users exist)
```

**With filtering:**

```javascript
const hasAdmins = await existsQuery(userModel)
  .where(w => w.equals('role', 'admin'))
  .execute(knex)

// Returns: true or false
```

**Complex filtering:**

```javascript
const hasActiveAdmins = await existsQuery(userModel)
  .where(w =>
    w
      .equals('role', 'admin')
      .equals('active', true)
      .gte('lastLogin', '2024-01-01')
  )
  .execute(knex)

// Returns: true or false
```

**With modifiers:**

```javascript
const hasActiveUsers = await existsQuery(userModel)
  .modifier('activeOnly')
  .execute(knex)

// Returns: true or false
```

**Combining where and modifiers:**

```javascript
const hasActiveAdmins = await existsQuery(userModel)
  .where(w => w.equals('role', 'admin'))
  .modifier('activeOnly')
  .execute(knex)

// Returns: true or false
```

**With logical operators:**

```javascript
const hasSpecialUsers = await existsQuery(userModel)
  .where(w =>
    w
      .or(or => or.equals('role', 'admin').equals('role', 'owner'))
      .and(and => and.equals('verified', true).gte('age', 18))
  )
  .execute(knex)

// Returns: true or false
```

**Conditional existence check:**

```javascript
async function checkUserExists(filters) {
  const q = existsQuery(userModel)

  if (filters.role) {
    q.where(w => w.equals('role', filters.role))
  }

  if (filters.email) {
    q.where(w => w.equals('email', filters.email))
  }

  if (filters.activeOnly) {
    q.modifier('activeOnly')
  }

  return await q.execute(knex)
}

const exists = await checkUserExists({
  role: 'admin',
  email: 'admin@example.com',
  activeOnly: true
})

// Returns: true or false
```

**Using with plain where object:**

```javascript
const hasInactiveAdmins = await existsQuery(userModel)
  .where({ role: 'admin', active: false })
  .execute(knex)

// Returns: true or false
```

**Row-level security check:**

```javascript
const canAccessPost = await existsQuery(postModel)
  .where(w =>
    w
      .equals('id', postId)
      .exists('memberships', m =>
        m.equals('user_id', currentUserId).in('role', ['admin', 'owner'])
      )
  )
  .execute(knex)

// Returns: true if user has access, false otherwise
```

**Performance-optimized checks:**

```javascript
// Efficient - uses SELECT 1 LIMIT 1
const hasAnyPosts = await existsQuery(postModel).execute(knex)

// Better than counting
// const count = await countQuery(postModel).total().execute(knex)
// const hasAnyPosts = count.total > 0

// Better than fetching data
// const posts = await query(postModel).take(1).execute(knex)
// const hasAnyPosts = posts.data.length > 0
```
