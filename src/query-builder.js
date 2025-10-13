const { buildQuery } = require('./knex-tools')

/**
 * WhereBuilder - Fluent API for building where conditions
 */
class WhereBuilder {
  constructor() {
    this.conditions = {}
  }

  // Comparison operators
  equals(field, value) {
    this.conditions[field] = value
    return this
  }

  not(field, value) {
    this.conditions[field] = { not: value }
    return this
  }

  gt(field, value) {
    this._addCondition(field, 'gt', value)
    return this
  }

  gte(field, value) {
    this._addCondition(field, 'gte', value)
    return this
  }

  lt(field, value) {
    this._addCondition(field, 'lt', value)
    return this
  }

  lte(field, value) {
    this._addCondition(field, 'lte', value)
    return this
  }

  contains(field, value) {
    this._addCondition(field, 'contains', value)
    return this
  }

  startsWith(field, value) {
    this._addCondition(field, 'startsWith', value)
    return this
  }

  endsWith(field, value) {
    this._addCondition(field, 'endsWith', value)
    return this
  }

  in(field, values) {
    this._addCondition(field, 'in', values)
    return this
  }

  notIn(field, values) {
    this._addCondition(field, 'notIn', values)
    return this
  }

  isNull(field) {
    this._addCondition(field, 'isNull', true)
    return this
  }

  isNotNull(field) {
    this._addCondition(field, 'isNotNull', true)
    return this
  }

  // Logical operators
  and(callback) {
    if (!this.conditions.AND) {
      this.conditions.AND = []
    }
    const nestedBuilder = new WhereBuilder()
    callback(nestedBuilder)
    this.conditions.AND.push(nestedBuilder.build())
    return this
  }

  or(callback) {
    if (!this.conditions.OR) {
      this.conditions.OR = []
    }
    const nestedBuilder = new WhereBuilder()
    callback(nestedBuilder)
    this.conditions.OR.push(nestedBuilder.build())
    return this
  }

  // Exists operator for RLS
  exists(relationName, callback) {
    if (!this.conditions._exists) {
      this.conditions._exists = {}
    }
    const nestedBuilder = new WhereBuilder()
    callback(nestedBuilder)
    this.conditions._exists[relationName] = nestedBuilder.build()
    return this
  }

  // Conditional filtering
  when(condition, callback) {
    if (condition) {
      callback(this)
    }
    return this
  }

  // Raw where object
  raw(whereObject) {
    this.conditions = { ...this.conditions, ...whereObject }
    return this
  }

  // Helper to add or merge conditions
  _addCondition(field, operator, value) {
    if (
      typeof this.conditions[field] === 'object' &&
      !Array.isArray(this.conditions[field])
    ) {
      this.conditions[field][operator] = value
    } else {
      this.conditions[field] = { [operator]: value }
    }
  }

  build() {
    return this.conditions
  }
}

/**
 * QueryBuilder - Fluent API for building buildQuery config
 */
class QueryBuilder {
  constructor(modelObject) {
    this.modelObject = modelObject
    this.config = {}
  }

  projection(name) {
    this.config.projection = name
    return this
  }

  where(callbackOrObject) {
    if (typeof callbackOrObject === 'function') {
      const whereBuilder = new WhereBuilder()
      callbackOrObject(whereBuilder)
      this.config.where = whereBuilder.build()
    } else {
      this.config.where = { ...this.config.where, ...callbackOrObject }
    }
    return this
  }

  orderBy(fieldOrObject, direction) {
    if (!this.config.orderBy) {
      this.config.orderBy = {}
    }

    if (typeof fieldOrObject === 'object') {
      this.config.orderBy = { ...this.config.orderBy, ...fieldOrObject }
    } else {
      this.config.orderBy[fieldOrObject] = direction || 'asc'
    }
    return this
  }

  take(limit) {
    this.config.take = limit
    return this
  }

  skip(offset) {
    this.config.skip = offset
    return this
  }

  with(relationName, callbackOrProjection) {
    if (!this.config.each) {
      this.config.each = {}
    }

    if (typeof callbackOrProjection === 'function') {
      // Nested builder for relation
      const relation = this.modelObject.relations[relationName]
      if (!relation) {
        throw new Error(`Relation '${relationName}' not found in model`)
      }
      const relationModel = relation.modelDefinition()
      const relationBuilder = new QueryBuilder(relationModel)
      callbackOrProjection(relationBuilder)
      this.config.each[relationName] = relationBuilder.build()
    } else if (typeof callbackOrProjection === 'string') {
      // Simple projection string
      this.config.each[relationName] = { projection: callbackOrProjection }
    } else {
      // Default projection
      this.config.each[relationName] = { projection: 'details' }
    }
    return this
  }

  withCounts(relationName, whereCallbackOrBoolean = true) {
    if (!this.config.withRelatedCounts) {
      this.config.withRelatedCounts = {}
    }

    if (typeof whereCallbackOrBoolean === 'function') {
      const whereBuilder = new WhereBuilder()
      whereCallbackOrBoolean(whereBuilder)
      this.config.withRelatedCounts[relationName] = {
        where: whereBuilder.build()
      }
    } else if (typeof whereCallbackOrBoolean === 'object') {
      this.config.withRelatedCounts[relationName] = whereCallbackOrBoolean
    } else {
      this.config.withRelatedCounts[relationName] = whereCallbackOrBoolean
    }
    return this
  }

  modifier(nameOrObject, params) {
    if (!this.config.modifiers) {
      this.config.modifiers = {}
    }

    if (typeof nameOrObject === 'object') {
      this.config.modifiers = { ...this.config.modifiers, ...nameOrObject }
    } else {
      this.config.modifiers[nameOrObject] = params || {}
    }
    return this
  }

  metadata(options) {
    this.config.metadata = options
    return this
  }

  counts(options) {
    if (!this.config.metadata) {
      this.config.metadata = {}
    }
    this.config.metadata.counts = options
    return this
  }

  build() {
    return this.config
  }

  async execute(knexInstance) {
    return await buildQuery(knexInstance, this.modelObject, this.config)
  }
}

/**
 * CountsBuilder - Fluent API for building counts config
 */
class CountsBuilder {
  constructor(modelObject) {
    this.modelObject = modelObject
    this.config = { counts: {} }
  }

  where(callbackOrObject) {
    if (typeof callbackOrObject === 'function') {
      const whereBuilder = new WhereBuilder()
      callbackOrObject(whereBuilder)
      this.config.where = whereBuilder.build()
    } else {
      this.config.where = { ...this.config.where, ...callbackOrObject }
    }
    return this
  }

  total() {
    this.config.counts.total = true
    return this
  }

  filtered() {
    this.config.counts.filtered = true
    return this
  }

  modifier(nameOrObject, params) {
    if (!this.config.counts.modifiers) {
      this.config.counts.modifiers = {}
    }

    if (typeof nameOrObject === 'object') {
      this.config.counts.modifiers = {
        ...this.config.counts.modifiers,
        ...nameOrObject
      }
    } else {
      this.config.counts.modifiers[nameOrObject] = params || {}
    }
    return this
  }

  build() {
    return this.config
  }

  async execute(knexInstance) {
    const { counts } = require('./knex-tools')
    return await counts(knexInstance, this.modelObject, this.config)
  }
}

/**
 * ExistsBuilder - Fluent API for building exists config
 */
class ExistsBuilder {
  constructor(modelObject) {
    this.modelObject = modelObject
    this.config = {}
  }

  where(callbackOrObject) {
    if (typeof callbackOrObject === 'function') {
      const whereBuilder = new WhereBuilder()
      callbackOrObject(whereBuilder)
      this.config.where = whereBuilder.build()
    } else {
      this.config.where = { ...this.config.where, ...callbackOrObject }
    }
    return this
  }

  modifier(nameOrObject, params) {
    if (!this.config.modifiers) {
      this.config.modifiers = {}
    }

    if (typeof nameOrObject === 'object') {
      this.config.modifiers = { ...this.config.modifiers, ...nameOrObject }
    } else {
      this.config.modifiers[nameOrObject] = params || {}
    }
    return this
  }

  build() {
    return this.config
  }

  async execute(knexInstance) {
    const { exists } = require('./knex-tools')
    return await exists(knexInstance, this.modelObject, this.config)
  }
}

// Factory functions
function query(modelObject) {
  return new QueryBuilder(modelObject)
}

function where() {
  return new WhereBuilder()
}

function countQuery(modelObject) {
  return new CountsBuilder(modelObject)
}

function existsQuery(modelObject) {
  return new ExistsBuilder(modelObject)
}

module.exports = {
  QueryBuilder,
  WhereBuilder,
  CountsBuilder,
  ExistsBuilder,
  query,
  where,
  countQuery,
  existsQuery
}
