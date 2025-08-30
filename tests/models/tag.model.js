module.exports = {
  tableName: 'tag',
  projections: {
    details: ['id', 'name', 'created_at', 'updated_at'],
    short: ['id', 'name']
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
