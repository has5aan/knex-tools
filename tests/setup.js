const knex = require('knex')

async function createInMemoryEmptyDatabase() {
  const db = knex({
    client: 'sqlite3',
    connection: {
      filename: ':memory:'
    },
    useNullAsDefault: true
  })

  // Create a basic table for testing
  await db.schema.createTable('folder', table => {
    table.increments('id').primary()
    table.string('name')
    table.integer('user_id')
    table.integer('parent_id').nullable()
    table.timestamps(true, true)
  })

  return db
}

module.exports = {
  createInMemoryEmptyDatabase
}
