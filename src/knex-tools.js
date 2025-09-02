function applyWhereClauses(query, table, criteria, relations) {
  if (!criteria || !criteria.where) {
    return query
  }

  const conditions = criteria.where
  const logicalOperators = ['OR', 'AND']
  const hasLogicalOps = logicalOperators.some(op => conditions[op])

  // Process logical operators
  if (hasLogicalOps) {
    if (conditions.AND && Array.isArray(conditions.AND)) {
      conditions.AND.forEach(condition => {
        // Only apply if no _condition property exists or if it's true
        if (
          condition._condition === undefined ||
          condition._condition === true
        ) {
          query.where(builder => {
            applyWhereClauses(builder, table, { where: condition }, relations)
          })
        }
      })
    }

    if (conditions.OR && Array.isArray(conditions.OR)) {
      conditions.OR.forEach(condition => {
        query.orWhere(builder => {
          applyWhereClauses(builder, table, { where: condition }, relations)
        })
      })
    }
  }

  // Handle field conditions directly in the where object (for backward compatibility)
  Object.entries(conditions).forEach(([field, fieldConditions]) => {
    // Skip logical operators, columns and related
    if (
      logicalOperators.includes(field) ||
      field === 'columns' ||
      field === 'related'
    ) {
      return
    }

    // Skip special properties
    if (field.startsWith('_')) {
      return
    }

    // Handle null value directly (Prisma style)
    if (fieldConditions === null) {
      query = query.whereNull(`${table}.${field}`)
      return
    }

    // Extract condition flag if present
    const { _condition, ...actualConditions } =
      typeof fieldConditions === 'object' && !Array.isArray(fieldConditions)
        ? fieldConditions
        : { _condition: undefined }

    // If _condition is provided and not true, skip
    if (_condition !== undefined && _condition !== true) {
      return
    }

    // If conditions is a primitive value, use equality
    if (typeof fieldConditions !== 'object') {
      query = query.where(`${table}.${field}`, '=', fieldConditions)
      return
    }

    // Handle multiple operators for the same field
    Object.entries(actualConditions).forEach(([operator, value]) => {
      applyOperator(query, table, field, operator, value)
    })
  })

  return query
}

// Helper function to apply the right operator
function applyOperator(query, table, field, operator, value) {
  switch (operator) {
    case 'equals':
      query = query.where(`${table}.${field}`, '=', value)
      break
    case 'not':
      query = query.where(`${table}.${field}`, '!=', value)
      break
    case 'gt':
      query = query.where(`${table}.${field}`, '>', value)
      break
    case 'gte':
      query = query.where(`${table}.${field}`, '>=', value)
      break
    case 'lt':
      query = query.where(`${table}.${field}`, '<', value)
      break
    case 'lte':
      query = query.where(`${table}.${field}`, '<=', value)
      break
    case 'contains': {
      const containsOperator =
        query.client.config.client === 'pg' ? 'ilike' : 'like'
      query = query.where(`${table}.${field}`, containsOperator, `%${value}%`)
      break
    }
    case 'startsWith': {
      const startsWithOperator =
        query.client.config.client === 'pg' ? 'ilike' : 'like'
      query = query.where(`${table}.${field}`, startsWithOperator, `${value}%`)
      break
    }
    case 'endsWith': {
      const endsWithOperator =
        query.client.config.client === 'pg' ? 'ilike' : 'like'
      query = query.where(`${table}.${field}`, endsWithOperator, `%${value}`)
      break
    }
    case 'in':
      query = query.whereIn(
        `${table}.${field}`,
        Array.isArray(value) ? value : [value]
      )
      break
    case 'notIn':
      query = query.whereNotIn(
        `${table}.${field}`,
        Array.isArray(value) ? value : [value]
      )
      break
    case 'isNull':
      query = query.whereNull(`${table}.${field}`)
      break
    case 'isNotNull':
      query = query.whereNotNull(`${table}.${field}`)
      break
    case 'hasAll': {
      const values = Array.isArray(value) ? value : [value]
      const cteName = `${field}_hasall_values`

      // Create CTE SQL manually to ensure proper UNION construction
      const cteSelects = values.map(() => 'select ? as value').join(' union ')
      const cteQuery = query.client.raw(cteSelects, values)

      // Create CTE with field-specific name
      query = query.with(cteName, cteQuery)

      // Add EXISTS clause with COUNT
      query = query.whereExists(function () {
        this.select(1)
          .from(cteName)
          .whereRaw(`??.value = ??.??`, [cteName, table, field])
          .havingRaw('COUNT(*) = ?', [values.length])
      })
      break
    }
  }
}

function applyPagingClauses(query, criteria) {
  if (!criteria) {
    return query
  }

  if (criteria.take) {
    query.limit(criteria.take)
  }

  if (criteria.skip) {
    query.offset(criteria.skip)
  }

  return query
}

function applySortingClauses(query, table, criteria, defaultSortOptions) {
  if (!criteria || Object.keys(criteria).length === 0) {
    query.orderBy(
      `${table}.${defaultSortOptions.field}`,
      defaultSortOptions.direction
    )

    return query
  }

  Object.entries(criteria).forEach(([field, direction]) => {
    query.orderBy(`${table}.${field}`, direction)
  })

  return query
}

async function executeUnitOfWork(knexInstance, callback) {
  const trx = await knexInstance.transaction()
  try {
    const result = await callback(trx)
    await trx.commit()
    return result
  } catch (error) {
    await trx.rollback()
    throw error
  }
}

function buildMakeTransaction(knexInstance) {
  return async function (callback) {
    return executeUnitOfWork(knexInstance, callback)
  }
}

function processJoins(query, table, joins, relations) {
  if (!joins || !relations) {
    return
  }

  Object.entries(joins).forEach(([relationName, options]) => {
    const relationInfo = relations[relationName]
    if (!relationInfo) {
      throw new Error(
        `Relation '${relationName}' not found in relations config`
      )
    }

    const joinOptions = options === true ? {} : options

    // Determine join type (enforce = INNER, include = LEFT, default = LEFT)
    const joinType = joinOptions.type === 'enforce' ? 'innerJoin' : 'leftJoin'

    // Extract join conditions (join-time filtering)
    const joinConditions = joinOptions.on

    // Extract WHERE conditions (post-join filtering)
    const whereConditions = joinOptions.where

    // Select columns with aliases
    const columns = relationInfo
      .modelDefinition()
      .columns.map(col => `${relationName}.${col} as ${relationName}_${col}`)
    query.select(columns)

    switch (relationInfo.type) {
      case 'belongsTo':
        query[joinType](
          `${relationInfo.table} as ${relationName}`,
          function () {
            this.on(
              `${relationName}.${relationInfo.primaryKey}`,
              `${table}.${relationInfo.foreignKey}`
            )

            if (joinConditions) {
              applyJoinConditions(this, relationName, joinConditions)
            }
          }
        )

        // Apply post-join WHERE filtering if both 'on' and 'where' are specified
        if (joinConditions && whereConditions) {
          applyWhereClauses(query, relationName, { where: whereConditions })
        }
        break

      case 'hasMany':
        query[joinType](
          `${relationInfo.table} as ${relationName}`,
          function () {
            this.on(
              `${relationName}.${relationInfo.foreignKey}`,
              `${table}.${relationInfo.primaryKey}`
            )

            if (joinConditions) {
              applyJoinConditions(this, relationName, joinConditions)
            }
          }
        )

        // Apply post-join WHERE filtering if both 'on' and 'where' are specified
        if (joinConditions && whereConditions) {
          applyWhereClauses(query, relationName, { where: whereConditions })
        }
        break

      case 'manyToMany': {
        query[joinType](
          relationInfo.through.table,
          `${table}.${relationInfo.primaryKey || 'id'}`,
          `${relationInfo.through.table}.${relationInfo.through.foreignKey}`
        )
        query[joinType](
          `${relationInfo.table} as ${relationName}`,
          function () {
            this.on(
              `${relationInfo.through.table}.${relationInfo.through.otherKey}`,
              `${relationName}.${relationInfo.primaryKey || 'id'}`
            )

            if (joinConditions) {
              applyJoinConditions(this, relationName, joinConditions)
            }
          }
        )

        // Apply post-join WHERE filtering if both 'on' and 'where' are specified
        if (joinConditions && whereConditions) {
          applyWhereClauses(query, relationName, { where: whereConditions })
        }
        break
      }
    }

    // Process nested joins if relation has its own relations defined
    if (joinOptions.join && relationInfo.relations) {
      processJoins(
        query,
        relationInfo.table,
        joinOptions.join,
        relationInfo.relations
      )
    }
  })
}

function applyJoinConditions(joinQuery, table, conditions) {
  if (!conditions) {
    return joinQuery
  }

  const logicalOperators = ['OR', 'AND']
  const hasLogicalOps = logicalOperators.some(op => conditions[op])

  // Process logical operators
  if (hasLogicalOps) {
    if (conditions.AND && Array.isArray(conditions.AND)) {
      conditions.AND.forEach(condition => {
        // Only apply if no _condition property exists or if it's true
        if (
          condition._condition === undefined ||
          condition._condition === true
        ) {
          joinQuery.on(builder => {
            applyJoinConditions(builder, table, condition)
          })
        }
      })
    }

    if (conditions.OR && Array.isArray(conditions.OR)) {
      conditions.OR.forEach(condition => {
        joinQuery.orOn(builder => {
          applyJoinConditions(builder, table, condition)
        })
      })
    }
  }

  // Handle field conditions directly in the conditions object (for backward compatibility)
  Object.entries(conditions).forEach(([field, fieldConditions]) => {
    // Skip logical operators, columns and related
    if (
      logicalOperators.includes(field) ||
      field === 'columns' ||
      field === 'related'
    ) {
      return
    }

    // Skip special properties
    if (field.startsWith('_')) {
      return
    }

    // Handle null value directly (Prisma style)
    if (fieldConditions === null) {
      joinQuery.andOnNull(`${table}.${field}`)
      return
    }

    // Extract condition flag if present
    const { _condition, ...actualConditions } =
      typeof fieldConditions === 'object' && !Array.isArray(fieldConditions)
        ? fieldConditions
        : { _condition: undefined }

    // If _condition is provided and not true, skip
    if (_condition !== undefined && _condition !== true) {
      return
    }

    // If conditions is a primitive value, use equality with parameterization
    if (typeof fieldConditions !== 'object') {
      joinQuery.andOnVal(`${table}.${field}`, '=', fieldConditions)
      return
    }

    // Handle multiple operators for the same field
    Object.entries(actualConditions).forEach(([operator, value]) => {
      applyJoinOperator(joinQuery, `${table}.${field}`, operator, value)
    })
  })

  return joinQuery
}

// Helper function to apply the right join operator with parameterization
function applyJoinOperator(joinQuery, field, operator, value) {
  switch (operator) {
    case 'equals':
      joinQuery.andOnVal(`${field}`, '=', value)
      break
    case 'not':
      if (value === null) {
        joinQuery.andOnNotNull(`${field}`)
      } else {
        joinQuery.andOnVal(`${field}`, '!=', value)
      }
      break
    case 'gt':
      joinQuery.andOnVal(`${field}`, '>', value)
      break
    case 'gte':
      joinQuery.andOnVal(`${field}`, '>=', value)
      break
    case 'lt':
      joinQuery.andOnVal(`${field}`, '<', value)
      break
    case 'lte':
      joinQuery.andOnVal(`${field}`, '<=', value)
      break
    case 'contains': {
      // PostgreSQL optimization - use ILIKE for case-insensitive search
      const containsOperator =
        joinQuery.client &&
        joinQuery.client.config &&
        joinQuery.client.config.client === 'pg'
          ? 'ilike'
          : 'like'
      joinQuery.andOnVal(`${field}`, containsOperator, `%${value}%`)
      break
    }
    case 'startsWith': {
      const startsWithOperator =
        joinQuery.client &&
        joinQuery.client.config &&
        joinQuery.client.config.client === 'pg'
          ? 'ilike'
          : 'like'
      joinQuery.andOnVal(`${field}`, startsWithOperator, `${value}%`)
      break
    }
    case 'endsWith': {
      const endsWithOperator =
        joinQuery.client &&
        joinQuery.client.config &&
        joinQuery.client.config.client === 'pg'
          ? 'ilike'
          : 'like'
      joinQuery.andOnVal(`${field}`, endsWithOperator, `%${value}`)
      break
    }
    case 'in':
      joinQuery.andOnIn(`${field}`, Array.isArray(value) ? value : [value])
      break
    case 'notIn':
      joinQuery.andOnNotIn(`${field}`, Array.isArray(value) ? value : [value])
      break
    case 'isNull':
      joinQuery.andOnNull(`${field}`)
      break
    case 'isNotNull':
      joinQuery.andOnNotNull(`${field}`)
      break
  }
}

module.exports = {
  applyWhereClauses,
  applyPagingClauses,
  applySortingClauses,
  applyJoinConditions,
  processJoins,
  buildMakeTransaction
}
