module.exports = {
  tableName: 'memo',
  alias: 'm',
  columns: [
    'id',
    'user_id',
    'folder_id',
    'content',
    'created_at',
    'updated_at'
  ],
  projections: {
    details: function (_, alias) {
      return [
        `${alias}.id`,
        `${alias}.user_id`,
        `${alias}.folder_id`,
        `${alias}.content`,
        `${alias}.created_at`,
        `${alias}.updated_at`
      ]
    },
    short: function (knexInstance, alias) {
      return [
        `${alias}.id`,
        knexInstance.raw(`substr(${alias}.content, 1, 32) as content`)
      ]
    },
    basic: function (_, alias) {
      return [`${alias}.id`, `${alias}.content`]
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
