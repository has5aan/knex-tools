module.exports = {
  tableName: 'user',
  columns: [
    'id',
    'name',
    'email',
    'role',
    'active',
    'premium',
    'verified',
    'created_at',
    'updated_at'
  ],
  projections: {
    details: [
      'id',
      'name',
      'email',
      'role',
      'active',
      'premium',
      'verified',
      'created_at',
      'updated_at'
    ],
    short: ['id', 'name', 'email']
  },
  relations: {
    folders: {
      type: 'hasMany',
      model: 'folder',
      table: 'folder',
      foreignKey: 'user_id',
      primaryKey: 'id',
      modelDefinition: () => require('./folder.model')
    }
  }
}
