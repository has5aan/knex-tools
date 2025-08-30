module.exports = {
  tableName: 'folder',
  columns: ['id', 'user_id', 'parent_id', 'name', 'created_at', 'updated_at'],
  projections: {
    details: ['id', 'name', 'parent_id', 'created_at', 'updated_at'],
    short: ['id', 'name']
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
    forUser: (query, userId) => {
      query.innerJoin('user', {
        'folder.user_id': 'user.id',
        'user.id': userId
      })
    }
  }
}
