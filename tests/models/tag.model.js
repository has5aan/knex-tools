module.exports = {
  tableName: 'tag',
  alias: 't',
  columns: ['id', 'name', 'created_at', 'updated_at'],
  projections: {
    details: function (_, alias) {
      return [
        `${alias}.id`,
        `${alias}.name`,
        `${alias}.created_at`,
        `${alias}.updated_at`
      ]
    },
    short: function (_, alias) {
      return [`${alias}.id`, `${alias}.name`]
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
    forUser: (query, userId) => {
      query.innerJoin('user', 'tag.user_id', 'user.id').where('user.id', userId)
    }
  }
}
