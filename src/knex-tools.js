function applyWhereClauses(query, table, criteria, relations) {
  if (!criteria || !criteria.where) {
    return query
  }

  const conditions = criteria.where
  const logicalOperators = ['OR', 'AND', 'NOT']
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

    if (conditions.NOT) {
      query.whereNot(builder => {
        applyWhereClauses(builder, table, { where: conditions.NOT }, relations)
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
      applyOperator(query, `${table}.${field}`, operator, value)
    })
  })

  return query
}

// Helper function to apply the right operator
function applyOperator(query, field, operator, value) {
  switch (operator) {
    case 'equals':
      query = query.where(`${field}`, '=', value)
      break
    case 'not':
      query = query.where(`${field}`, '!=', value)
      break
    case 'gt':
      query = query.where(`${field}`, '>', value)
      break
    case 'gte':
      query = query.where(`${field}`, '>=', value)
      break
    case 'lt':
      query = query.where(`${field}`, '<', value)
      break
    case 'lte':
      query = query.where(`${field}`, '<=', value)
      break
    case 'contains': {
      const containsOperator =
        query.client.config.client === 'pg' ? 'ilike' : 'like'
      query = query.where(`${field}`, containsOperator, `%${value}%`)
      break
    }
    case 'startsWith': {
      const startsWithOperator =
        query.client.config.client === 'pg' ? 'ilike' : 'like'
      query = query.where(`${field}`, startsWithOperator, `${value}%`)
      break
    }
    case 'endsWith': {
      const endsWithOperator =
        query.client.config.client === 'pg' ? 'ilike' : 'like'
      query = query.where(`${field}`, endsWithOperator, `%${value}`)
      break
    }
    case 'in':
      query = query.whereIn(`${field}`, Array.isArray(value) ? value : [value])
      break
    case 'notIn':
      query = query.whereNotIn(
        `${field}`,
        Array.isArray(value) ? value : [value]
      )
      break
    case 'isNull':
      query = query.whereNull(`${field}`)
      break
    case 'isNotNull':
      query = query.whereNotNull(`${field}`)
      break
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

    // Select columns with aliases
    const columns = relationInfo
      .modelDefinition()
      .columns.map(col => `${relationName}.${col} as ${relationName}_${col}`)
    query.select(columns)

    switch (relationInfo.type) {
      case 'belongsTo':
        query.leftJoin(`${relationInfo.table} as ${relationName}`, function () {
          this.on(
            `${relationName}.${relationInfo.foreignKey}`,
            `${table}.${relationInfo.primaryKey}`
          )

          if (joinOptions.where) {
            applyJoinConditions(this, relationInfo.table, joinOptions.where)
          }
        })
        break

      case 'hasMany':
        query.leftJoin(`${relationInfo.table} as ${relationName}`, function () {
          this.on(
            `${relationName}.${relationInfo.foreignKey}`,
            `${table}.${relationInfo.primaryKey}`
          )

          if (joinOptions.where) {
            //applyJoinConditions(this, relationInfo.table, joinOptions.where)
            applyWhereClauses(
              query,
              table,
              { where: joinOptions.where },
              relations
            )
          }
        })
        break

      case 'manyToMany':
        query
          .leftJoin(
            relationInfo.through.table,
            `${table}.${relationInfo.primaryKey}`,
            `${relationInfo.through.table}.${relationInfo.through.foreignKey} as ${relationName}`
          )
          .leftJoin(`${relationInfo.table} as ${relationName}`, function () {
            this.on(
              `${relationInfo.through.table}.${relationInfo.through.otherKey}`,
              `${table}.${relationInfo.primaryKey}`
            )

            if (joinOptions.where) {
              applyJoinConditions(this, relationInfo.table, joinOptions.where)
            }
          })
        break
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
  Object.entries(conditions).forEach(([field, condition]) => {
    // Handle null
    if (condition === null) {
      joinQuery.andOnNull(`${table}.${field}`)
      return
    }

    // Handle direct value equality
    if (typeof condition !== 'object') {
      joinQuery.andOn(`${table}.${field}`, '=', condition)
      return
    }

    // Handle operators - matching all our where operators
    Object.entries(condition).forEach(([operator, value]) => {
      switch (operator) {
        case 'equals':
          joinQuery.andOn(`${table}.${field}`, '=', value)
          break
        case 'not':
          if (value === null) {
            joinQuery.andOnNotNull(`${table}.${field}`)
          } else {
            joinQuery.andOn(`${table}.${field}`, '!=', value)
          }
          break
        case 'gt':
          joinQuery.andOn(`${table}.${field}`, '>', value)
          break
        case 'gte':
          joinQuery.andOn(`${table}.${field}`, '>=', value)
          break
        case 'lt':
          joinQuery.andOn(`${table}.${field}`, '<', value)
          break
        case 'lte':
          joinQuery.andOn(`${table}.${field}`, '<=', value)
          break
        case 'contains':
          joinQuery.andOn(`${table}.${field}`, 'like', `%${value}%`)
          break
        case 'startsWith':
          joinQuery.andOn(`${table}.${field}`, 'like', `${value}%`)
          break
        case 'endsWith':
          joinQuery.andOn(`${table}.${field}`, 'like', `%${value}`)
          break
        case 'in':
          joinQuery.andOnIn(
            `${table}.${field}`,
            Array.isArray(value) ? value : [value]
          )
          break
        case 'notIn':
          joinQuery.andOnNotIn(
            `${table}.${field}`,
            Array.isArray(value) ? value : [value]
          )
          break
      }
    })
  })
}

module.exports = {
  applyWhereClauses,
  applyPagingClauses,
  applySortingClauses,
  processJoins,
  buildMakeTransaction
}
