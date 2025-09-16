module.exports = {
  tableName: 'folder',
  alias: 'f',
  projections: {
    details: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.name as ${prefix}name`,
        `${alias}.user_id as ${prefix}user_id`,
        `${alias}.parent_id as ${prefix}parent_id`,
        `${alias}.created_at as ${prefix}created_at`,
        `${alias}.updated_at as ${prefix}updated_at`
      ]
    },
    short: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.name as ${prefix}name`,
        `${alias}.user_id as ${prefix}user_id`,
        `${alias}.parent_id as ${prefix}parent_id`
      ]
    }
  },
  relations: {
    parent: {
      type: 'belongsTo',
      model: 'folder',
      table: 'folder',
      foreignKey: 'parent_id',
      primaryKey: 'id',
      modelDefinition: () => require('./folder.model')
    },
    children: {
      type: 'hasMany',
      model: 'folder',
      table: 'folder',
      foreignKey: 'parent_id',
      primaryKey: 'id',
      modelDefinition: () => require('./folder.model')
    },
    memos: {
      type: 'hasMany',
      model: 'memo',
      table: 'memo',
      foreignKey: 'folder_id',
      primaryKey: 'id',
      modelDefinition: () => require('./memo.model')
    },
    user: {
      type: 'belongsTo',
      model: 'user',
      table: 'user',
      foreignKey: 'user_id',
      primaryKey: 'id',
      modelDefinition: () => require('./user.model')
    }
  },
  modifiers: {
    forUser: (query, knexInstance, tableAlias, { userId }) => {
      query.innerJoin('user as u', {
        [`${tableAlias}.user_id`]: 'u.id',
        'u.id': userId
      })
    }
  }
}
