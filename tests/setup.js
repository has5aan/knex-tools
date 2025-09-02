const knex = require('knex')

async function createInMemoryEmptyDatabase() {
  const db = knex({
    client: 'sqlite3',
    connection: {
      filename: ':memory:'
    },
    useNullAsDefault: true
  })

  // Create user table
  await db.schema.createTable('user', table => {
    table.increments('id').primary()
    table.string('name')
    table.string('email')
    table.string('role')
    table.boolean('active').defaultTo(true)
    table.boolean('premium').defaultTo(false)
    table.boolean('verified').defaultTo(false)
    table.timestamps(true, true)
  })

  // Create folder table
  await db.schema.createTable('folder', table => {
    table.increments('id').primary()
    table.string('name')
    table.integer('user_id')
    table.integer('parent_id').nullable()
    table.timestamps(true, true)
  })

  // Create memo table
  await db.schema.createTable('memo', table => {
    table.increments('id').primary()
    table.integer('user_id')
    table.integer('folder_id').nullable()
    table.text('content')
    table.timestamps(true, true)
  })

  // Create tag table
  await db.schema.createTable('tag', table => {
    table.increments('id').primary()
    table.string('name')
    table.integer('user_id').nullable()
    table.timestamps(true, true)
  })

  // Create memo_tag junction table
  await db.schema.createTable('memo_tag', table => {
    table.integer('memo_id')
    table.integer('tag_id')
    table.primary(['memo_id', 'tag_id'])
  })

  return db
}

module.exports = {
  createInMemoryEmptyDatabase
}
