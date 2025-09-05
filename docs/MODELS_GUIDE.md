# Models Guide

Complete guide to defining and using models in knex-tools. Models are the foundation of the query system, defining structure, relationships, projections, and reusable query logic.

## Table of Contents

- [Model Anatomy](#model-anatomy)
- [Basic Properties](#basic-properties)
- [Projections](#projections)
- [Relations](#relations)
- [Modifiers](#modifiers)
- [Advanced Patterns](#advanced-patterns)
- [Best Practices](#best-practices)

---

## Model Anatomy

A complete knex-tools model consists of four main sections:

```javascript
const userModel = {
  // 1. Basic Properties - Required
  tableName: 'users',
  alias: 'u',

  // 2. Projections - REQUIRED for buildQuery
  projections: {
    details: (knexInstance, alias) => [...],
    summary: (knexInstance, alias) => [...]
  },

  // 3. Relations - For data fetching and exists clause filtering
  relations: {
    posts: { type: 'hasMany', ... },
    profile: { type: 'belongsTo', ... }
  },

  // 4. Modifiers - Reusable query logic
  modifiers: {
    default: (query, knex, alias) => {...},
    forRole: (query, knex, alias, { role }) => {...}
  }
}
```

---

## Basic Properties

### tableName

**Required** - The actual database table name.

```javascript
{
  tableName: 'users' // Maps to 'users' table in database
}
```

### alias

**Required** - Short alias used in SQL queries to prevent conflicts.

```javascript
{
  alias: 'u' // Will generate SQL like: SELECT u.id FROM users as u
}
```

**Best Practices:**

- Use short, memorable aliases (`'u'`, `'p'`, `'c'`)
- Make aliases unique across your application
- Use the same alias consistently for each model

---

## Projections

**REQUIRED for buildQuery** - Projections define what columns to select for different use cases. They're functions that return arrays of column expressions.

> ⚠️ **Important**: The `buildQuery` function requires models to have at least one projection. Without projections, `buildQuery` will throw an error.

### Basic Projections

```javascript
projections: {
  // Simple column selection
  summary: (knexInstance, alias) => [
    `${alias}.id`,
    `${alias}.name`
  ],

  // Full details
  details: (knexInstance, alias) => [
    `${alias}.id`,
    `${alias}.name`,
    `${alias}.email`,
    `${alias}.role`,
    `${alias}.created_at`
  ]
}
```

### Advanced Projections with Raw SQL

```javascript
projections: {
  withStats: (knexInstance, alias) => [
    `${alias}.id`,
    `${alias}.name`,
    knexInstance.raw(`COUNT(posts.id) as post_count`),
    knexInstance.raw(`MAX(posts.created_at) as last_post_date`)
  ],

  computed: (knexInstance, alias) => [
    `${alias}.id`,
    `${alias}.name`,
    knexInstance.raw(`
      CASE
        WHEN ${alias}.role = 'admin' THEN 'Administrator'
        WHEN ${alias}.role = 'user' THEN 'Regular User'
        ELSE 'Unknown'
      END as role_display
    `)
  ]
}
```

### Conditional Projections

```javascript
projections: {
  forRole: (knexInstance, alias, { role }) => {
    const base = [`${alias}.id`, `${alias}.name`]

    if (role === 'admin') {
      return [
        ...base,
        `${alias}.email`,
        `${alias}.internal_notes`,
        knexInstance.raw(`COUNT(posts.id) as managed_posts`)
      ]
    }

    return base
  }
}
```

### Projection Best Practices

1. **Always use alias parameter**: `${alias}.column` instead of hardcoded table names
2. **Name projections by use case**: `summary`, `details`, `forApi`, `forReport`
3. **Use knexInstance for raw SQL**: Access to `knex.raw()` and other methods
4. **Keep projections focused**: Each projection should serve a specific purpose

---

## Relations

Relations define how models connect to each other. They enable nested data fetching and exists clause filtering.

### belongsTo Relations

When this model has a foreign key pointing to another table.

```javascript
// User belongs to a department
relations: {
  department: {
    type: 'belongsTo',
    model: 'department',
    table: 'departments',
    foreignKey: 'department_id',  // Column in THIS table
    primaryKey: 'id',             // Column in RELATED table
    modelDefinition: () => require('./department.model')
  }
}
```

**SQL Generated:**

```sql
LEFT JOIN departments as d ON u.department_id = d.id
```

### hasMany Relations

When the related table has a foreign key pointing to this model.

```javascript
// User has many posts
relations: {
  posts: {
    type: 'hasMany',
    model: 'post',
    table: 'posts',
    foreignKey: 'user_id',       // Column in RELATED table
    primaryKey: 'id',            // Column in THIS table
    modelDefinition: () => require('./post.model')
  }
}
```

**SQL Generated:**

```sql
LEFT JOIN posts as p ON u.id = p.user_id
```

### manyToMany Relations

When models are connected through a junction table.

```javascript
// User has many tags through user_tags
relations: {
  tags: {
    type: 'manyToMany',
    model: 'tag',
    table: 'tags',
    through: {
      table: 'user_tags',        // Junction table
      foreignKey: 'user_id',     // THIS model's key in junction
      otherKey: 'tag_id'         // OTHER model's key in junction
    },
    primaryKey: 'id',            // Optional, defaults to 'id'
    modelDefinition: () => require('./tag.model')
  }
}
```

**SQL Generated:**

```sql
LEFT JOIN user_tags as ut ON u.id = ut.user_id
LEFT JOIN tags as t ON ut.tag_id = t.id
```

### Self-Referencing Relations

```javascript
// Categories with parent-child relationships
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
      modelDefinition: () => categoryModel // Self-reference
    },
    children: {
      type: 'hasMany',
      model: 'category',
      table: 'categories',
      foreignKey: 'parent_id',
      primaryKey: 'id',
      modelDefinition: () => categoryModel // Self-reference
    }
  }
}
```

### Relation Best Practices

1. **Use lazy loading for modelDefinition**: `() => require('./model')` prevents circular imports
2. **Be explicit with keys**: Always specify `foreignKey` and `primaryKey`
3. **Match your database**: Relation types should match your actual foreign key constraints
4. **Use meaningful names**: Relation names should describe the relationship clearly

---

## Modifiers

Modifiers are reusable functions that apply common query logic. They come in two types: **default** (automatic) and **named** (manual).

### Default Modifiers - Horizontal Partitioning

Default modifiers are automatically applied to every query using the model. Perfect for:

- Soft deletes
- Multi-tenancy
- Status filtering
- Security constraints

```javascript
modifiers: {
  // Reserved 'default' key - automatically applied
  default: (query, knexInstance, tableAlias) => {
    query.where(`${tableAlias}.active`, true)
         .whereNull(`${tableAlias}.deleted_at`)
  }
}
```

**Usage:**

```javascript
// Default modifier is applied automatically
const users = await buildQuery(knex, userModel, {
  projection: 'details'
})
// SQL: SELECT ... FROM users as u WHERE u.active = true AND u.deleted_at IS NULL
```

**Real-World Example - Multi-tenant:**

```javascript
// tenant-user.model.js - Only users from current tenant
const userModel = require('./user.model')

module.exports = {
  ...userModel,
  alias: 'tu', // Different alias
  modifiers: {
    default: (query, knexInstance, tableAlias) => {
      const tenantId = getCurrentTenantId() // Your tenant logic
      query
        .where(`${tableAlias}.tenant_id`, tenantId)
        .where(`${tableAlias}.active`, true)
    }
  }
}
```

### Named Modifiers - Parameterized Logic

Named modifiers are manually applied with parameters. Perfect for:

- Role-based filtering
- Date range queries
- Complex business logic
- Conditional joins

```javascript
modifiers: {
  forRole: (query, knexInstance, tableAlias, { role }) => {
    query.where(`${tableAlias}.role`, role)
  },

  createdAfter: (query, knexInstance, tableAlias, { date }) => {
    query.where(`${tableAlias}.created_at`, '>=', date)
  },

  withPosts: (query, knexInstance, tableAlias, { minCount = 1 }) => {
    query.join('posts as p', `${tableAlias}.id`, 'p.user_id')
         .groupBy(`${tableAlias}.id`)
         .having(knexInstance.raw('COUNT(p.id)'), '>=', minCount)
  },

  inDepartments: (query, knexInstance, tableAlias, { departments }) => {
    query.join('departments as d', `${tableAlias}.department_id`, 'd.id')
         .where('d.name', 'in', departments)
  }
}
```

**Usage:**

```javascript
// Single named modifier
const admins = await buildQuery(knex, userModel, {
  projection: 'details',
  modifiers: {
    forRole: { role: 'admin' }
  }
})

// Multiple named modifiers
const recentActiveWriters = await buildQuery(knex, userModel, {
  projection: 'details',
  modifiers: {
    forRole: { role: 'writer' },
    createdAfter: { date: '2024-01-01' },
    withPosts: { minCount: 5 }
  }
})
```

### Modifier Signature

All modifiers follow the same signature:

```javascript
;(query, knexInstance, tableAlias, parameters) => {
  // Modify the query object
}
```

- **query**: Knex QueryBuilder instance to modify
- **knexInstance**: Full Knex instance for raw queries, transactions, etc.
- **tableAlias**: The model's alias for safe column references
- **parameters**: Object with destructured parameters (named modifiers only)

### Modifier Best Practices

1. **Use tableAlias consistently**: `${tableAlias}.column` prevents conflicts
2. **Destructure parameters**: `{ role }` instead of `params.role`
3. **Provide defaults**: `{ minCount = 1 }` for optional parameters
4. **Chain wisely**: Remember `query.where().join()` modifies the same object
5. **Keep focused**: Each modifier should do one logical operation

---

## Advanced Patterns

### Model Inheritance

Create specialized models that extend base models:

```javascript
// base-user.model.js
const baseUserModel = {
  tableName: 'users',
  alias: 'u',
  columns: ['id', 'name', 'email', 'role', 'active'],
  projections: {
    details: (knex, alias) => [`${alias}.id`, `${alias}.name`, `${alias}.email`]
  }
}

// active-user.model.js - Only active users
module.exports = {
  ...baseUserModel,
  alias: 'au', // Different alias to avoid conflicts
  modifiers: {
    default: (query, knex, alias) => {
      query.where(`${alias}.active`, true)
    }
  }
}

// admin-user.model.js - Only admin users
module.exports = {
  ...baseUserModel,
  alias: 'admin_u',
  modifiers: {
    default: (query, knex, alias) => {
      query.where(`${alias}.role`, 'admin').where(`${alias}.active`, true)
    }
  },
  projections: {
    ...baseUserModel.projections,
    // Add admin-specific projections
    withPermissions: (knex, alias) => [
      `${alias}.id`,
      `${alias}.name`,
      knex.raw(`array_agg(permissions.name) as permissions`)
    ]
  }
}
```

### Dynamic Projections

Projections that adapt based on context:

```javascript
projections: {
  forUser: (knexInstance, alias, { currentUser }) => {
    const base = [`${alias}.id`, `${alias}.name`]

    // Add email only if viewing own profile or admin
    if (
      currentUser.id === parseInt(alias.split('.')[1]) ||
      currentUser.role === 'admin'
    ) {
      base.push(`${alias}.email`)
    }

    // Add internal fields for admins only
    if (currentUser.role === 'admin') {
      base.push(`${alias}.internal_notes`, `${alias}.created_at`)
    }

    return base
  }
}
```

### Complex Modifiers with Subqueries

```javascript
modifiers: {
  withRecentActivity: (query, knexInstance, tableAlias, { days = 30 }) => {
    const recentDate = new Date()
    recentDate.setDate(recentDate.getDate() - days)

    query.whereExists(function() {
      this.select(1)
          .from('user_activities as ua')
          .whereRaw(`ua.user_id = ${tableAlias}.id`)
          .where('ua.created_at', '>=', recentDate)
    })
  },

  excludingBlocked: (query, knexInstance, tableAlias, { byUserId }) => {
    query.whereNotExists(function() {
      this.select(1)
          .from('blocked_users as bu')
          .whereRaw(`bu.blocked_user_id = ${tableAlias}.id`)
          .where('bu.user_id', byUserId)
    })
  }
}
```

---

## Best Practices

### 1. Consistent Naming

```javascript
// Good - Clear, consistent naming
{
  tableName: 'users',        // Database table name
  alias: 'u',               // Short SQL alias
  projections: {
    summary: ...,           // Use case based names
    details: ...,
    forApi: ...
  },
  relations: {
    posts: ...,            // Plural for hasMany
    profile: ...,          // Singular for belongsTo
    tags: ...              // Plural for manyToMany
  }
}
```

### 2. Security-First Design

```javascript
// Always filter sensitive data by default
modifiers: {
  default: (query, knex, alias) => {
    query.where(`${alias}.active`, true)
         .whereNull(`${alias}.deleted_at`)
         // Hide internal test accounts
         .where(`${alias}.email`, 'not like', '%@internal.test')
  }
}
```

### 3. Performance Optimization

```javascript
projections: {
  // Lightweight projection for lists
  summary: (knex, alias) => [
    `${alias}.id`, `${alias}.name`, `${alias}.created_at`
  ],

  // Heavy projection with computed fields for details
  full: (knex, alias) => [
    `${alias}.*`,
    knex.raw(`
      (SELECT COUNT(*) FROM posts WHERE user_id = ${alias}.id) as post_count
    `),
    knex.raw(`
      (SELECT MAX(created_at) FROM posts WHERE user_id = ${alias}.id) as last_post
    `)
  ]
}
```

### 4. Documentation and Types

```javascript
/**
 * User model with role-based access and activity tracking
 *
 * Projections:
 *   - summary: Basic info for lists (id, name, email)
 *   - details: Full profile information
 *   - withStats: Includes post count and activity metrics
 *
 * Modifiers:
 *   - forRole({ role }): Filter by user role
 *   - withRecentActivity({ days }): Users active within N days
 */
const userModel = {
  tableName: 'users',
  alias: 'u'
  // ... rest of model
}
```

### 5. Testing Models

```javascript
// test/models/user.model.test.js
const userModel = require('../../src/models/user.model')

describe('User Model', () => {
  it('should have required properties', () => {
    expect(userModel.tableName).toBe('users')
    expect(userModel.alias).toBe('u')
    expect(userModel.columns).toContain('id')
  })

  it('should have working projections', () => {
    const knex = require('knex')({ client: 'pg' })
    const columns = userModel.projections.details(knex, 'u')
    expect(columns).toContain('u.id')
    expect(columns).toContain('u.name')
  })
})
```

---

## Model Organization

### File Structure

```
models/
├── base/
│   ├── user.model.js          # Base user model
│   └── audit.model.js         # Base audit fields
├── specialized/
│   ├── active-user.model.js   # Active users only
│   ├── admin-user.model.js    # Admin users only
│   └── tenant-user.model.js   # Multi-tenant users
├── core/
│   ├── post.model.js
│   ├── comment.model.js
│   └── tag.model.js
└── index.js                   # Model registry
```

### Model Registry

```javascript
// models/index.js
module.exports = {
  user: require('./base/user.model'),
  activeUser: require('./specialized/active-user.model'),
  adminUser: require('./specialized/admin-user.model'),
  post: require('./core/post.model'),
  comment: require('./core/comment.model'),
  tag: require('./core/tag.model')
}
```

This guide provides the foundation for building robust, secure, and maintainable models with knex-tools. Models are the key to leveraging the full power of the query system.
