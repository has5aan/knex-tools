# API Reference

Documentation for all knex-tools functions and their parameters.

## Table of Contents

- [Core Functions](#core-functions)
  - [buildQuery](#buildquery)
  - [applyWhereClauses](#applywhereclauses)
  - [applySortingClauses](#applysortingclauses)
  - [applyPagingClauses](#applypagingclauses)
  - [processJoins](#processjoins)
  - [buildMakeTransaction](#buildmaketransaction)
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

| Property     | Type     | Description               | Example                                       |
| ------------ | -------- | ------------------------- | --------------------------------------------- |
| `projection` | `string` | Projection key from model | `'details'`                                   |
| `where`      | `Object` | Filtering conditions      | `{ age: { gte: 18 } }`                        |
| `orderBy`    | `Object` | Sorting configuration     | `{ name: 'asc', created_at: 'desc' }`         |
| `take`       | `number` | Limit results             | `10`                                          |
| `skip`       | `number` | Offset results            | `20`                                          |
| `each`       | `Object` | Nested relation loading   | `{ posts: { projection: 'summary' } }`        |
| `modifiers`  | `Object` | Apply named modifiers     | `{ forRole: { role: 'admin' } }`              |
| `metadata`   | `Object` | Include metadata counts   | `{ counts: { total: true, filtered: true } }` |

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
    role: 'admin',
    _condition: user.isAdmin, // Only apply if true
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

| Operator     | Description              | Example                                       |
| ------------ | ------------------------ | --------------------------------------------- |
| `_exists`    | Subquery existence check | `{ _exists: { posts: { published: true } } }` |
| `_condition` | Conditional application  | `{ role: 'admin', _condition: user.isOwner }` |

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
