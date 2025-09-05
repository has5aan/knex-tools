module.exports = {
  tableName: 'user',
  alias: 'u',
  projections: {
    details: function (_, alias) {
      return [
        `${alias}.id`,
        `${alias}.name`,
        `${alias}.email`,
        `${alias}.role`,
        `${alias}.active`,
        `${alias}.premium`,
        `${alias}.verified`,
        `${alias}.created_at`,
        `${alias}.updated_at`
      ]
    },
    short: function (_, alias) {
      return [`${alias}.id`, `${alias}.name`, `${alias}.email`]
    }
  },
  relations: {
    folders: {
      type: 'hasMany',
      model: 'folder',
      table: 'folder',
      foreignKey: 'user_id',
      primaryKey: 'id',
      modelDefinition: () => require('./folder.model')
    },
    memos: {
      type: 'hasMany',
      model: 'memo',
      table: 'memo',
      foreignKey: 'user_id',
      primaryKey: 'id',
      modelDefinition: () => require('./memo.model')
    }
  }
}
