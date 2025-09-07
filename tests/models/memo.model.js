module.exports = {
  tableName: 'memo',
  alias: 'm',
  projections: {
    details: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.user_id as ${prefix}user_id`,
        `${alias}.folder_id as ${prefix}folder_id`,
        `${alias}.content as ${prefix}content`,
        `${alias}.created_at as ${prefix}created_at`,
        `${alias}.updated_at as ${prefix}updated_at`
      ]
    },
    short: function (knexInstance, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        knexInstance.raw(`substr(${alias}.content, 1, 32) as ${prefix}content`)
      ]
    },
    basic: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.content as ${prefix}content`
      ]
    }
  },
  relations: {
    user: {
      type: 'belongsTo',
      model: 'user',
      table: 'user',
      foreignKey: 'user_id',
      primaryKey: 'id',
      modelDefinition: () => require('./user.model')
    },
    tags: {
      type: 'manyToMany',
      model: 'tag',
      table: 'tag',
      through: {
        table: 'memo_tag',
        alias: 'mt',
        foreignKey: 'memo_id',
        otherKey: 'tag_id'
      },
      modelDefinition: () => require('./tag.model')
    },
    folder: {
      type: 'belongsTo',
      model: 'folder',
      table: 'folder',
      foreignKey: 'folder_id',
      primaryKey: 'id',
      modelDefinition: () => require('./folder.model')
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
