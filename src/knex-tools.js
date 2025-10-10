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
        if (condition == null) {
          return
        } // Skip null/undefined conditions

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
        if (condition == null) {
          return
        } // Skip null/undefined conditions

        // Only apply if no _condition property exists or if it's true
        if (
          condition._condition === undefined ||
          condition._condition === true
        ) {
          query.orWhere(builder => {
            applyWhereClauses(builder, table, { where: condition }, relations)
          })
        }
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

    // Handle _exists for RLS functionality
    if (field === '_exists') {
      applyExistsClause(query, table, fieldConditions, relations)
      return
    }

    // Skip other special properties
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
      if (value === null) {
        query = query.whereNotNull(`${table}.${field}`)
      } else {
        query = query.where(`${table}.${field}`, '!=', value)
      }
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
  }
}

// Helper function to apply EXISTS clauses for RLS functionality
function applyExistsClause(query, table, existsConditions, relations) {
  if (!relations) {
    throw new Error('Relations must be provided to use _exists functionality')
  }

  Object.entries(existsConditions).forEach(([relationName, conditions]) => {
    const relation = relations[relationName]
    if (!relation) {
      throw new Error(
        `Relation '${relationName}' not found in relations config`
      )
    }

    // Use relationName as alias (consistent with processJoins pattern)
    const alias = relationName

    // Build the EXISTS subquery based on relation type
    query.whereExists(subquery => {
      subquery.select(1)

      switch (relation.type) {
        case 'belongsTo':
          // main_table has foreign_key, related_table has primary_key
          // EXISTS (SELECT 1 FROM related_table WHERE related_table.primary_key = main_table.foreign_key)
          subquery
            .from(`${relation.table} as ${alias}`)
            .where(
              `${alias}.${relation.primaryKey}`,
              '=',
              query.client.raw(`${table}.${relation.foreignKey}`)
            )
          break

        case 'hasMany':
          // related_table has foreign_key, main_table has primary_key
          // EXISTS (SELECT 1 FROM related_table WHERE related_table.foreign_key = main_table.primary_key)
          subquery
            .from(`${relation.table} as ${alias}`)
            .where(
              `${alias}.${relation.foreignKey}`,
              '=',
              query.client.raw(`${table}.${relation.primaryKey}`)
            )
          break

        case 'manyToMany': {
          // Need to join through junction table
          // EXISTS (SELECT 1 FROM junction_table JOIN related_table WHERE junction.main_key = main_table.primary_key)
          const junctionTable = relation.through.table
          subquery
            .from(junctionTable)
            .join(
              `${relation.table} as ${alias}`,
              `${junctionTable}.${relation.through.otherKey}`,
              `${alias}.${relation.primaryKey || 'id'}`
            )
            .where(
              `${junctionTable}.${relation.through.foreignKey}`,
              '=',
              query.client.raw(`${table}.${relation.primaryKey || 'id'}`)
            )
          break
        }
      }

      // Use applyWhereClauses for full operator support (logical operators, _condition flags, etc.)
      applyWhereClauses(subquery, alias, { where: conditions }, relations)
    })
  })

  return query
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

function processJoins(query, rootModel, joins, relations) {
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

    // Get the relation model to access its alias
    const relationModel = relationInfo.modelDefinition()
    // For self-referencing relationships, use relationName as alias to avoid conflicts
    // For other relationships, use model alias if available, otherwise fallback to relationName
    const relationAlias =
      relationModel.tableName === rootModel.tableName
        ? relationName // Use relationName for self-referencing to avoid alias conflicts
        : relationModel.alias || relationName

    // Select columns with aliases - projection is required
    if (options.projection) {
      // If projection is specified, it MUST exist
      if (
        !relationModel.projections ||
        !relationModel.projections[options.projection]
      ) {
        throw new Error(
          `Projection '${options.projection}' not found in model '${relationModel.tableName}'`
        )
      }
      const columns = relationModel.projections[options.projection](
        query,
        relationAlias,
        relationName
      )
      query.select(columns)
    }

    switch (relationInfo.type) {
      case 'belongsTo':
        query[joinType](
          `${relationInfo.table} as ${relationAlias}`,
          function () {
            this.on(
              `${relationAlias}.${relationInfo.primaryKey}`,
              `${rootModel.alias}.${relationInfo.foreignKey}`
            )

            if (joinConditions) {
              applyJoinConditions(this, relationAlias, joinConditions)
            }
          }
        )

        // Apply post-join WHERE filtering if both 'on' and 'where' are specified
        if (joinConditions && whereConditions) {
          applyWhereClauses(
            query,
            relationAlias,
            { where: whereConditions },
            relations
          )
        }
        break

      case 'hasMany':
        query[joinType](
          `${relationInfo.table} as ${relationAlias}`,
          function () {
            this.on(
              `${relationAlias}.${relationInfo.foreignKey}`,
              `${rootModel.alias}.${relationInfo.primaryKey}`
            )

            if (joinConditions) {
              applyJoinConditions(this, relationAlias, joinConditions)
            }
          }
        )

        // Apply post-join WHERE filtering if both 'on' and 'where' are specified
        if (joinConditions && whereConditions) {
          applyWhereClauses(
            query,
            relationAlias,
            { where: whereConditions },
            relations
          )
        }
        break

      case 'manyToMany': {
        const junctionAlias = relationInfo.through.alias
        query[joinType](
          `${relationInfo.through.table} as ${junctionAlias}`,
          `${rootModel.alias}.${relationInfo.primaryKey || 'id'}`,
          `${junctionAlias}.${relationInfo.through.foreignKey}`
        )
        query[joinType](
          `${relationInfo.table} as ${relationAlias}`,
          function () {
            this.on(
              `${junctionAlias}.${relationInfo.through.otherKey}`,
              `${relationAlias}.${relationInfo.primaryKey || 'id'}`
            )

            if (joinConditions) {
              applyJoinConditions(this, relationAlias, joinConditions)
            }
          }
        )

        // Apply post-join WHERE filtering if both 'on' and 'where' are specified
        if (joinConditions && whereConditions) {
          applyWhereClauses(
            query,
            relationAlias,
            { where: whereConditions },
            relations
          )
        }
        break
      }
    }

    // Process nested joins if relation has its own relations defined
    if (joinOptions.join && relationModel.relations) {
      processJoins(
        query,
        relationModel,
        joinOptions.join,
        relationModel.relations
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
        if (condition == null) {
          return
        } // Skip null/undefined conditions

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
        if (condition == null) {
          return
        } // Skip null/undefined conditions

        // Only apply if no _condition property exists or if it's true
        if (
          condition._condition === undefined ||
          condition._condition === true
        ) {
          joinQuery.orOn(builder => {
            applyJoinConditions(builder, table, condition)
          })
        }
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

async function buildQuery(knexInstance, modelObject, queryConfig) {
  // Execute main query with alias
  const query = knexInstance(`${modelObject.tableName} as ${modelObject.alias}`)

  // Apply default modifier if present
  if (modelObject.modifiers && modelObject.modifiers.default) {
    modelObject.modifiers.default(query, modelObject.alias)
  }

  // Apply requested modifiers with parameters
  if (queryConfig.modifiers && modelObject.modifiers) {
    Object.entries(queryConfig.modifiers).forEach(([modifierName, params]) => {
      const modifier = modelObject.modifiers[modifierName]
      if (!modifier) {
        throw new Error(`Modifier '${modifierName}' not found in model`)
      }
      // Apply modifier with destructured parameters
      modifier(query, modelObject.alias, params)
    })
  }

  // Apply projection - projection is required
  const projection = modelObject.projections[queryConfig.projection]
  if (!projection) {
    throw new Error(`Projection '${queryConfig.projection}' not found in model`)
  }
  // All projections are functions that receive (knexInstance, alias)
  const projectionColumns = projection(knexInstance, modelObject.alias)
  query.select(projectionColumns)

  // Apply where clauses
  if (queryConfig.where) {
    applyWhereClauses(
      query,
      modelObject.alias,
      { where: queryConfig.where },
      modelObject.relations
    )
  }

  // Apply sorting
  if (queryConfig.orderBy) {
    applySortingClauses(query, modelObject.alias, queryConfig.orderBy, {
      field: 'id',
      direction: 'asc'
    })
  }

  // Apply paging
  if (queryConfig.take || queryConfig.skip) {
    applyPagingClauses(query, {
      take: queryConfig.take,
      skip: queryConfig.skip
    })
  }

  // Execute main query
  const records = await query

  // Populate related counts for each record
  if (queryConfig.withRelatedCounts && records.length > 0) {
    await populateRelatedCounts(
      knexInstance,
      records,
      modelObject,
      queryConfig.withRelatedCounts
    )
  }

  // Collect metadata if requested
  let metadata = {}
  if (queryConfig.metadata?.counts) {
    metadata.counts = await collectCounts(
      knexInstance,
      modelObject,
      queryConfig.metadata.counts,
      queryConfig.where
    )
  }

  // Process 'each' relations for every record
  if (queryConfig.each && records.length > 0) {
    await populateEachRelations(
      knexInstance,
      records,
      modelObject,
      queryConfig.each
    )
  }

  // Always return object format for consistency
  const result = { data: records }

  // Only add metadata key if there is metadata
  if (Object.keys(metadata).length > 0) {
    result.metadata = metadata
  }

  return result
}

async function populateEachRelations(
  knexInstance,
  records,
  modelObject,
  eachConfig
) {
  for (const [relationKey, relationQuery] of Object.entries(eachConfig)) {
    const relation = modelObject.relations[relationKey]
    if (!relation) {
      throw new Error(`Relation '${relationKey}' not found in model`)
    }

    const relatedModel = relation.modelDefinition()

    // Handle different relation types
    switch (relation.type) {
      case 'belongsTo': {
        await populateBelongsToRelation(
          knexInstance,
          records,
          relation,
          relatedModel,
          relationKey,
          relationQuery
        )
        break
      }
      case 'hasMany': {
        await populateHasManyRelation(
          knexInstance,
          records,
          relation,
          relatedModel,
          relationKey,
          relationQuery
        )
        break
      }
      case 'manyToMany': {
        await populateManyToManyRelation(
          knexInstance,
          records,
          relation,
          relatedModel,
          relationKey,
          relationQuery
        )
        break
      }
    }
  }
}

async function populateBelongsToRelation(
  knexInstance,
  records,
  relation,
  relatedModel,
  relationKey,
  relationQuery
) {
  // Validate that foreign key is included in projection
  if (records.length > 0 && !(relation.foreignKey in records[0])) {
    throw new Error(
      `Cannot populate '${relationKey}' relation: projection must include '${relation.foreignKey}' field`
    )
  }

  // Get unique foreign key values
  const foreignKeys = [
    ...new Set(
      records.map(record => record[relation.foreignKey]).filter(Boolean)
    )
  ]
  if (foreignKeys.length === 0) {
    // No foreign keys, populate with null for consistency
    records.forEach(record => {
      record[relationKey] = null
    })
    return { metadata: null }
  }

  // Build query for related records
  const result = await buildQuery(knexInstance, relatedModel, {
    ...relationQuery,
    where: {
      ...relationQuery.where,
      [relation.primaryKey]: { in: foreignKeys }
    }
  })

  // Extract data and metadata
  const relatedRecords = result.data || result
  const metadata = result.metadata

  // Create lookup map
  const relatedMap = new Map(
    relatedRecords.map(record => [record[relation.primaryKey], record])
  )

  // Attach related records to main records with consistent structure
  records.forEach(record => {
    const relatedRecord = relatedMap.get(record[relation.foreignKey])

    if (relatedRecord) {
      // Always use consistent structure: { data: record, metadata?: {...} }
      const relationResult = { data: relatedRecord }
      if (metadata) {
        relationResult.metadata = metadata
      }
      record[relationKey] = relationResult
    } else {
      record[relationKey] = null
    }
  })

  return { metadata: null } // Don't propagate metadata up
}

async function populateHasManyRelation(
  knexInstance,
  records,
  relation,
  relatedModel,
  relationKey,
  relationQuery
) {
  // Get unique primary key values
  const primaryKeys = [
    ...new Set(
      records.map(record => record[relation.primaryKey]).filter(Boolean)
    )
  ]
  if (primaryKeys.length === 0) {
    // Populate all records with empty relation data to maintain consistency
    records.forEach(record => {
      record[relationKey] = { data: [] }
    })
    return
  }

  // Build query for related records
  const result = await buildQuery(knexInstance, relatedModel, {
    ...relationQuery,
    where: {
      ...relationQuery.where,
      [relation.foreignKey]: { in: primaryKeys }
    }
  })

  // Extract data and metadata
  const relatedRecords = result.data || result
  const metadata = result.metadata

  // Group related records by foreign key
  const relatedMap = new Map()
  relatedRecords.forEach(record => {
    const key = record[relation.foreignKey]
    if (!relatedMap.has(key)) {
      relatedMap.set(key, [])
    }
    relatedMap.get(key).push(record)
  })

  // Attach related records to main records with consistent structure
  records.forEach(record => {
    const relatedRecords = relatedMap.get(record[relation.primaryKey]) || []

    // Always use consistent structure: { data: [...], metadata?: {...} }
    const relationResult = { data: relatedRecords }
    if (metadata) {
      relationResult.metadata = metadata
    }
    record[relationKey] = relationResult
  })

  return { metadata: null } // Don't propagate metadata up
}

async function populateManyToManyRelation(
  knexInstance,
  records,
  relation,
  relatedModel,
  relationKey,
  relationQuery
) {
  // Get unique primary key values
  const primaryKeys = [
    ...new Set(
      records.map(record => record[relation.primaryKey || 'id']).filter(Boolean)
    )
  ]
  if (primaryKeys.length === 0) {
    return
  }

  // First get the junction table records
  const junctionRecords = await knexInstance(relation.through.table)
    .select([relation.through.foreignKey, relation.through.otherKey])
    .whereIn(relation.through.foreignKey, primaryKeys)

  // Get unique other keys
  const otherKeys = [
    ...new Set(junctionRecords.map(record => record[relation.through.otherKey]))
  ]
  if (otherKeys.length === 0) {
    // No related records, but still populate with empty array for consistency
    records.forEach(record => {
      record[relationKey] = { data: [] }
    })
    return { metadata: null }
  }

  // Build query for related records
  const result = await buildQuery(knexInstance, relatedModel, {
    ...relationQuery,
    where: {
      ...relationQuery.where,
      [relation.primaryKey || 'id']: { in: otherKeys }
    }
  })

  // Extract data and metadata
  const relatedRecords = result.data || result
  const metadata = result.metadata

  // Create lookup map for related records
  const relatedMap = new Map(
    relatedRecords.map(record => [record[relation.primaryKey || 'id'], record])
  )

  // Group junction records by foreign key
  const junctionMap = new Map()
  junctionRecords.forEach(record => {
    const key = record[relation.through.foreignKey]
    if (!junctionMap.has(key)) {
      junctionMap.set(key, [])
    }
    junctionMap.get(key).push(record[relation.through.otherKey])
  })

  // Attach related records to main records with nested metadata structure
  records.forEach(record => {
    const otherKeys = junctionMap.get(record[relation.primaryKey || 'id']) || []
    const relatedRecords = otherKeys
      .map(key => relatedMap.get(key))
      .filter(Boolean)

    // Always use consistent structure: { data: [...], metadata?: {...} }
    const relationResult = { data: relatedRecords }
    if (metadata) {
      relationResult.metadata = metadata
    }
    record[relationKey] = relationResult
  })

  return { metadata: null } // Don't propagate metadata up
}

async function populateRelatedCounts(
  knexInstance,
  records,
  modelObject,
  relatedCountsConfig
) {
  const primaryKey = modelObject.primaryKey || 'id'
  const recordIds = records.map(r => r[primaryKey]).filter(Boolean)

  if (recordIds.length === 0) {
    return
  }

  for (const [relationName, config] of Object.entries(relatedCountsConfig)) {
    if (!config) {
      continue
    }

    const relation = modelObject.relations[relationName]
    if (!relation) {
      throw new Error(
        `Relation '${relationName}' not found in model for related counts.`
      )
    }

    if (relation.type === 'hasMany') {
      const relatedModel = relation.modelDefinition()
      const relatedAlias = relatedModel.alias
      const relatedTable = relatedModel.tableName
      const foreignKey = relation.foreignKey

      const countsQuery = knexInstance(`${relatedTable} as ${relatedAlias}`)
        .select(`${relatedAlias}.${foreignKey}`)
        .count('* as count')
        .whereIn(`${relatedAlias}.${foreignKey}`, recordIds)
        .groupBy(`${relatedAlias}.${foreignKey}`)

      const where = typeof config === 'object' ? config.where : null
      if (where) {
        applyWhereClauses(
          countsQuery,
          relatedAlias,
          { where },
          relatedModel.relations
        )
      }

      const counts = await countsQuery
      const countsMap = new Map(
        counts.map(c => [c[foreignKey], parseInt(c.count, 10)])
      )

      records.forEach(record => {
        if (!record._counts) {
          record._counts = {}
        }
        record._counts[relationName] = countsMap.get(record[primaryKey]) || 0
      })
    } else if (relation.type === 'manyToMany') {
      const relatedModel = relation.modelDefinition()
      const relatedAlias = relatedModel.alias
      const relatedTable = relatedModel.tableName
      const junctionTable = relation.through.table
      const parentForeignKey = relation.through.foreignKey
      const relatedForeignKey = relation.through.otherKey

      const countsQuery = knexInstance(`${junctionTable}`)
        .select(`${junctionTable}.${parentForeignKey}`)
        .count(`* as count`)
        .whereIn(`${junctionTable}.${parentForeignKey}`, recordIds)
        .groupBy(`${junctionTable}.${parentForeignKey}`)

      const where = typeof config === 'object' ? config.where : null
      if (where) {
        countsQuery.join(
          `${relatedTable} as ${relatedAlias}`,
          `${junctionTable}.${relatedForeignKey}`,
          `${relatedAlias}.${relation.primaryKey || 'id'}`
        )
        applyWhereClauses(
          countsQuery,
          relatedAlias,
          { where },
          relatedModel.relations
        )
      }

      const counts = await countsQuery
      const countsMap = new Map(
        counts.map(c => [c[parentForeignKey], parseInt(c.count, 10)])
      )

      records.forEach(record => {
        if (!record._counts) {
          record._counts = {}
        }
        record._counts[relationName] = countsMap.get(record[primaryKey]) || 0
      })
    }
    // Other relation types can be added here.
  }
}

// Helper function to collect count metadata
async function collectCounts(
  knexInstance,
  modelObject,
  countConfig,
  whereClause
) {
  const countPromises = []

  // Basic total count
  if (countConfig.total) {
    countPromises.push(
      getTotalCount(knexInstance, modelObject).then(count => ({ total: count }))
    )
  }

  // Basic filtered count
  if (countConfig.filtered && whereClause) {
    countPromises.push(
      getFilteredCount(knexInstance, modelObject, whereClause).then(count => ({
        filtered: count
      }))
    )
  }

  // Modifier-based counts
  if (countConfig.modifiers && modelObject.modifiers) {
    Object.entries(countConfig.modifiers).forEach(([modifierName, params]) => {
      const modifier = modelObject.modifiers[modifierName]
      if (!modifier) {
        throw new Error(`Modifier '${modifierName}' not found in model`)
      }

      countPromises.push(
        getModifierCount(knexInstance, modelObject, modifier, params).then(
          count => ({ [modifierName]: count })
        )
      )
    })
  }

  // Execute all count queries in parallel
  const countResults = await Promise.all(countPromises)
  return Object.assign({}, ...countResults)
}

// Get total count
async function getTotalCount(knexInstance, modelObject) {
  const result = await knexInstance(modelObject.tableName)
    .count('* as count')
    .first()
  return parseInt(result.count)
}

// Get filtered count
async function getFilteredCount(knexInstance, modelObject, whereClause) {
  const query = knexInstance(`${modelObject.tableName} as ${modelObject.alias}`)
  applyWhereClauses(
    query,
    modelObject.alias,
    { where: whereClause },
    modelObject.relations
  )
  const result = await query.count('* as count').first()
  return parseInt(result.count)
}

// Get modifier-based count
async function getModifierCount(knexInstance, modelObject, modifier, params) {
  const query = knexInstance(`${modelObject.tableName} as ${modelObject.alias}`)
  modifier(query, modelObject.alias, params)

  // If modifier didn't add count, add it
  const sql = query.toSQL().sql.toLowerCase()
  if (!sql.includes('count(')) {
    query.count('* as count')
  }

  const result = await query.first()
  return parseInt(result.count || result[Object.keys(result)[0]])
}

async function counts(knexInstance, modelObject, queryConfig = {}) {
  if (!queryConfig.counts) {
    throw new Error(
      `'counts' configuration is required in counts. e.g., { counts: { total: true } }`
    )
  }

  const countsResult = await collectCounts(
    knexInstance,
    modelObject,
    queryConfig.counts,
    queryConfig.where
  )

  return countsResult
}

async function exists(knexInstance, modelObject, queryConfig = {}) {
  const query = knexInstance(`${modelObject.tableName} as ${modelObject.alias}`)
    .select(1)
    .limit(1)

  // Apply default modifier if present
  if (modelObject.modifiers && modelObject.modifiers.default) {
    modelObject.modifiers.default(query, modelObject.alias)
  }

  // Apply ALL requested modifiers to the SAME query
  if (queryConfig.modifiers && modelObject.modifiers) {
    Object.entries(queryConfig.modifiers).forEach(([modifierName, params]) => {
      const modifier = modelObject.modifiers[modifierName]
      if (!modifier) {
        throw new Error(`Modifier '${modifierName}' not found in model`)
      }
      modifier(query, modelObject.alias, params)
    })
  }

  // Apply where clauses to the SAME query
  if (queryConfig.where) {
    applyWhereClauses(
      query,
      modelObject.alias,
      { where: queryConfig.where },
      modelObject.relations
    )
  }

  const result = await query.first()
  return !!result
}

module.exports = {
  applyWhereClauses,
  applyPagingClauses,
  applySortingClauses,
  applyJoinConditions,
  processJoins,
  buildMakeTransaction,
  buildQuery,
  counts,
  exists
}
