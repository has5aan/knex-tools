module.exports = {
  tableName: 'user',
  alias: 'u',
  projections: {
    details: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.name as ${prefix}name`,
        `${alias}.email as ${prefix}email`,
        `${alias}.role as ${prefix}role`,
        `${alias}.active as ${prefix}active`,
        `${alias}.premium as ${prefix}premium`,
        `${alias}.verified as ${prefix}verified`,
        `${alias}.created_at as ${prefix}created_at`,
        `${alias}.updated_at as ${prefix}updated_at`
      ]
    },
    short: function (_, alias, relationName = null) {
      const prefix = relationName ? `${relationName}_` : ''
      return [
        `${alias}.id as ${prefix}id`,
        `${alias}.name as ${prefix}name`,
        `${alias}.email as ${prefix}email`
      ]
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
