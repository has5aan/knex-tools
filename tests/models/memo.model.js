module.exports = {
  tableName: 'memo',
  columns: [
    'id',
    'user_id',
    'folder_id',
    'content',
    'created_at',
    'updated_at'
  ],
  projections: {
    details: [
      'id',
      'user_id',
      'folder_id',
      'content',
      'created_at',
      'updated_at'
    ],
    short: ['id', 'substring(content, 0, 32) as content']
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
    forUser: (query, userId) => {
      query.innerJoin('user', {
        'memo.user_id': 'user.id',
        'user.id': userId
      })
    }
  }
}
