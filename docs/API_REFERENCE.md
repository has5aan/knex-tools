# API Reference

Documentation for all knex-tools functions and their parameters.

## Table of Contents

- [Core Functions](#core-functions)
  - [buildQuery](#buildquery)
  - [applyWhereClauses](#applywhereclauses)
  - [applySortingClauses](#applysortingclauses)
  - [applyPagingClauses](#applypagingclauses)
  - [applyJoinConditions](#applyjoinconditions)
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

| Property     | Type     | Description               | Example                                |
| ------------ | -------- | ------------------------- | -------------------------------------- |
| `projection` | `string` | Projection key from model | `'details'`                            |
| `where`      | `Object` | Filtering conditions      | `{ age: { gte: 18 } }`                 |
| `orderBy`    | `Object` | Sorting configuration     | `{ name: 'asc', created_at: 'desc' }`  |
| `take`       | `number` | Limit results             | `10`                                   |
| `skip`       | `number` | Offset results            | `20`                                   |
| `each`       | `Object` | Nested relation loading   | `{ posts: { projection: 'summary' } }` |
| `modifiers`  | `Object` | Apply named modifiers     | `{ forRole: { role: 'admin' } }`       |

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

### applyJoinConditions

**Apply filtering conditions to JOIN clauses.**

```javascript
applyJoinConditions(joinQuery, table, conditions)
```

#### Parameters

| Parameter    | Type     | Description                                 |
| ------------ | -------- | ------------------------------------------- |
| `joinQuery`  | `Object` | Knex join builder instance                  |
| `table`      | `string` | Table alias to apply conditions to          |
| `conditions` | `Object` | Filtering conditions (same format as where) |

#### Examples

```javascript
// Used internally by processJoins for JOIN-time filtering
const joinQuery = query.leftJoin('posts as p', function () {
  this.on('u.id', 'p.user_id')
  applyJoinConditions(this, 'p', {
    published: true,
    created_at: { gte: '2024-01-01' }
  })
})
```

---

### processJoins

**JOIN operations with conditions and logical operators.**

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

| Property   | Type     | Description                 | Default      |
| ---------- | -------- | --------------------------- | ------------ |
| `joinType` | `string` | Type of JOIN                | `'leftJoin'` |
| `on`       | `Object` | JOIN conditions             | `{}`         |
| `where`    | `Object` | Additional WHERE conditions | `{}`         |

#### Examples

**Basic JOINs**

```javascript
processJoins(
  query,
  userModel,
  {
    posts: {
      on: { published: true }
    },
    profile: {
      joinType: 'innerJoin'
    }
  },
  userModel.relations
)
```

**JOIN Conditions**

```javascript
processJoins(
  query,
  postModel,
  {
    author: {
      on: {
        active: true,
        role: { in: ['author', 'editor'] }
      },
      where: {
        created_at: { gte: '2024-01-01' }
      }
    },
    tags: {
      joinType: 'leftJoin',
      on: {
        active: true
      }
    }
  },
  postModel.relations
)
// Generated SQL for manyToMany:
// LEFT JOIN post_tags as pt ON p.id = pt.post_id
// LEFT JOIN tags as t ON pt.tag_id = t.id AND t.active = true
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
    details: (knexInstanceOrQuery, alias, relationName = null) => [
      `${alias}.id`,
      `${alias}.name`,
      `${alias}.email`,
      `${alias}.role`,
      `${alias}.created_at`
    ],
    summary: (knexInstanceOrQuery, alias, relationName = null) => [
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
