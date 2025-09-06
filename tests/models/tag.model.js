module.exports = {
  tableName: 'tag',
  alias: 't',
  projections: {
    details: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.name as ${prefix}name`,
        `${alias}.created_at as ${prefix}created_at`,
        `${alias}.updated_at as ${prefix}updated_at`
      ]
    },
    short: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [`${alias}.id as ${prefix}id`, `${alias}.name as ${prefix}name`]
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
    memos: {
      type: 'manyToMany',
      model: 'memo',
      table: 'memo',
      through: {
        table: 'memo_tag',
        foreignKey: 'tag_id',
        otherKey: 'memo_id'
      },
      modelDefinition: () => require('./memo.model')
    }
  },
  modifiers: {
    // Reserved key - automatically applied by buildQuery
    // default: (query, knexInstance, tableAlias) => {
    //   query.where(`${tableAlias}.status`, 'active')
    // },

    // Manual modifiers for future use
    forUser: (query, knexInstance, tableAlias, { userId }) => {
      query
        .innerJoin('user as u', `${tableAlias}.user_id`, 'u.id')
        .where('u.id', userId)
    }
  }
}
