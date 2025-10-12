const {
  query,
  where,
  countQuery,
  existsQuery
} = require('../src/query-builder')
const { createInMemoryEmptyDatabase } = require('./setup')
const userModel = require('./models/user.model')

describe('query-builder', () => {
  let db

  beforeAll(async () => {
    db = await createInMemoryEmptyDatabase()
  })

  afterAll(async () => {
    await db.destroy()
  })

  afterEach(async () => {
    await db('memo_tag').del()
    await db('tag').del()
    await db('memo').del()
    await db('folder').del()
    await db('user').del()
  })

  describe('WhereBuilder', () => {
    describe('comparison operators', () => {
      const testCases = [
        {
          name: 'builds equals condition',
          builder: () => where().equals('role', 'admin'),
          expected: { role: 'admin' }
        },
        {
          name: 'builds not condition',
          builder: () => where().not('role', 'user'),
          expected: { role: { not: 'user' } }
        },
        {
          name: 'builds gt condition',
          builder: () => where().gt('age', 18),
          expected: { age: { gt: 18 } }
        },
        {
          name: 'builds gte condition',
          builder: () => where().gte('age', 18),
          expected: { age: { gte: 18 } }
        },
        {
          name: 'builds lt condition',
          builder: () => where().lt('age', 65),
          expected: { age: { lt: 65 } }
        },
        {
          name: 'builds lte condition',
          builder: () => where().lte('age', 65),
          expected: { age: { lte: 65 } }
        },
        {
          name: 'builds contains condition',
          builder: () => where().contains('name', 'John'),
          expected: { name: { contains: 'John' } }
        },
        {
          name: 'builds startsWith condition',
          builder: () => where().startsWith('email', 'admin'),
          expected: { email: { startsWith: 'admin' } }
        },
        {
          name: 'builds endsWith condition',
          builder: () => where().endsWith('email', '@example.com'),
          expected: { email: { endsWith: '@example.com' } }
        },
        {
          name: 'builds in condition',
          builder: () => where().in('role', ['admin', 'manager']),
          expected: { role: { in: ['admin', 'manager'] } }
        },
        {
          name: 'builds notIn condition',
          builder: () => where().notIn('role', ['guest', 'banned']),
          expected: { role: { notIn: ['guest', 'banned'] } }
        },
        {
          name: 'builds isNull condition',
          builder: () => where().isNull('deleted_at'),
          expected: { deleted_at: { isNull: true } }
        },
        {
          name: 'builds isNotNull condition',
          builder: () => where().isNotNull('email'),
          expected: { email: { isNotNull: true } }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('chaining multiple conditions', () => {
      const testCases = [
        {
          name: 'chains multiple conditions on different fields',
          builder: () =>
            where().equals('role', 'admin').gte('age', 18).isNotNull('email'),
          expected: {
            role: 'admin',
            age: { gte: 18 },
            email: { isNotNull: true }
          }
        },
        {
          name: 'chains multiple conditions on same field',
          builder: () => where().gte('age', 18).lte('age', 65),
          expected: { age: { gte: 18, lte: 65 } }
        },
        {
          name: 'chains with contains and in operators',
          builder: () =>
            where().contains('name', 'John').in('role', ['admin', 'manager']),
          expected: {
            name: { contains: 'John' },
            role: { in: ['admin', 'manager'] }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('logical operators', () => {
      const testCases = [
        {
          name: 'builds OR condition',
          builder: () =>
            where().or(or =>
              or.equals('role', 'admin').contains('email', '@admin.com')
            ),
          expected: {
            OR: [{ role: 'admin', email: { contains: '@admin.com' } }]
          }
        },
        {
          name: 'builds AND condition',
          builder: () =>
            where().and(and => and.gte('age', 18).equals('active', true)),
          expected: {
            AND: [{ age: { gte: 18 }, active: true }]
          }
        },
        {
          name: 'builds nested OR and AND conditions',
          builder: () =>
            where()
              .or(or => or.equals('role', 'admin'))
              .and(and => and.equals('active', true)),
          expected: {
            OR: [{ role: 'admin' }],
            AND: [{ active: true }]
          }
        },
        {
          name: 'combines conditions with OR',
          builder: () =>
            where()
              .equals('active', true)
              .or(or =>
                or.equals('role', 'admin').contains('email', '@admin.com')
              ),
          expected: {
            active: true,
            OR: [{ role: 'admin', email: { contains: '@admin.com' } }]
          }
        },
        {
          name: 'builds multiple AND blocks',
          builder: () =>
            where()
              .and(a1 => a1.equals('verified', true))
              .and(a2 => a2.gte('age', 18)),
          expected: {
            AND: [{ verified: true }, { age: { gte: 18 } }]
          }
        },
        {
          name: 'builds multiple OR blocks',
          builder: () =>
            where()
              .or(or1 => or1.equals('role', 'admin'))
              .or(or2 => or2.equals('role', 'manager')),
          expected: {
            OR: [{ role: 'admin' }, { role: 'manager' }]
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('exists operator', () => {
      const testCases = [
        {
          name: 'builds exists condition',
          builder: () =>
            where().exists('posts', posts => posts.equals('published', true)),
          expected: {
            _exists: {
              posts: { published: true }
            }
          }
        },
        {
          name: 'builds exists with multiple conditions',
          builder: () =>
            where().exists('user', user =>
              user.equals('id', 1).in('role', ['admin', 'owner'])
            ),
          expected: {
            _exists: {
              user: { id: 1, role: { in: ['admin', 'owner'] } }
            }
          }
        },
        {
          name: 'builds multiple exists blocks',
          builder: () =>
            where()
              .exists('posts', p => p.equals('published', true))
              .exists('comments', c => c.equals('approved', true)),
          expected: {
            _exists: {
              posts: { published: true },
              comments: { approved: true }
            }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('conditional building', () => {
      const testCases = [
        {
          name: 'applies condition when predicate is true',
          builder: () =>
            where()
              .equals('active', true)
              .when(true, w => w.equals('role', 'admin')),
          expected: { active: true, role: 'admin' }
        },
        {
          name: 'skips condition when predicate is false',
          builder: () =>
            where()
              .equals('active', true)
              .when(false, w => w.equals('role', 'admin')),
          expected: { active: true }
        },
        {
          name: 'applies multiple conditional filters',
          builder: () =>
            where()
              .equals('active', true)
              .when(true, w => w.equals('role', 'admin'))
              .when(false, w => w.gte('age', 18)),
          expected: { active: true, role: 'admin' }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('raw where object', () => {
      test('merges raw where object', () => {
        const result = where()
          .equals('active', true)
          .raw({ role: 'admin', age: { gte: 18 } })
          .build()

        expect(result).toEqual({
          active: true,
          role: 'admin',
          age: { gte: 18 }
        })
      })
    })

    describe('edge cases', () => {
      const testCases = [
        {
          name: 'overwrites equals with operator on same field',
          builder: () => where().equals('role', 'admin').gte('role', 'manager'),
          expected: { role: { gte: 'manager' } }
        },
        {
          name: 'overwrites operator with equals on same field',
          builder: () => where().gte('age', 18).equals('age', 25),
          expected: { age: 25 }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })
  })

  describe('QueryBuilder', () => {
    beforeEach(async () => {
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' }
      ])

      await db('folder').insert([
        { id: 1, name: 'Work', user_id: 1 },
        { id: 2, name: 'Personal', user_id: 2 }
      ])

      await db('memo').insert([
        { id: 1, content: 'Meeting notes', user_id: 1, folder_id: 1 },
        { id: 2, content: 'Shopping list', user_id: 2, folder_id: 2 }
      ])

      await db('tag').insert([
        { id: 1, name: 'urgent' },
        { id: 2, name: 'personal' }
      ])

      await db('memo_tag').insert([
        { memo_id: 1, tag_id: 1 },
        { memo_id: 2, tag_id: 2 }
      ])
    })

    describe('basic building', () => {
      const testCases = [
        {
          name: 'builds projection',
          builder: () => query(userModel).projection('details'),
          expected: { projection: 'details' }
        },
        {
          name: 'builds orderBy with single field',
          builder: () => query(userModel).orderBy('name', 'asc'),
          expected: { orderBy: { name: 'asc' } }
        },
        {
          name: 'builds orderBy with multiple fields',
          builder: () =>
            query(userModel).orderBy('role', 'asc').orderBy('name', 'desc'),
          expected: { orderBy: { role: 'asc', name: 'desc' } }
        },
        {
          name: 'builds orderBy with object',
          builder: () =>
            query(userModel).orderBy({ role: 'asc', name: 'desc' }),
          expected: { orderBy: { role: 'asc', name: 'desc' } }
        },
        {
          name: 'merges orderBy when called with object twice',
          builder: () =>
            query(userModel).orderBy({ name: 'asc' }).orderBy({ age: 'desc' }),
          expected: { orderBy: { name: 'asc', age: 'desc' } }
        },
        {
          name: 'builds orderBy with default direction',
          builder: () => query(userModel).orderBy('name'),
          expected: { orderBy: { name: 'asc' } }
        },
        {
          name: 'builds take',
          builder: () => query(userModel).take(10),
          expected: { take: 10 }
        },
        {
          name: 'builds skip',
          builder: () => query(userModel).skip(20),
          expected: { skip: 20 }
        },
        {
          name: 'builds complete query config',
          builder: () =>
            query(userModel)
              .projection('details')
              .orderBy('created_at', 'desc')
              .take(10)
              .skip(20),
          expected: {
            projection: 'details',
            orderBy: { created_at: 'desc' },
            take: 10,
            skip: 20
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('where conditions', () => {
      const testCases = [
        {
          name: 'builds where with fluent API',
          builder: () =>
            query(userModel).where(w =>
              w.equals('role', 'admin').gte('age', 18)
            ),
          expected: { where: { role: 'admin', age: { gte: 18 } } }
        },
        {
          name: 'builds where with object',
          builder: () => query(userModel).where({ role: 'admin' }),
          expected: { where: { role: 'admin' } }
        },
        {
          name: 'chains multiple where calls',
          builder: () =>
            query(userModel).where({ role: 'admin' }).where({ active: true }),
          expected: { where: { role: 'admin', active: true } }
        },
        {
          name: 'merges where when called with object twice',
          builder: () =>
            query(userModel)
              .where({ role: 'admin' })
              .where({ active: true })
              .where({ verified: true }),
          expected: { where: { role: 'admin', active: true, verified: true } }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('relations', () => {
      const testCases = [
        {
          name: 'builds simple relation with projection string',
          builder: () => query(userModel).with('folders', 'short'),
          expected: { each: { folders: { projection: 'short' } } }
        },
        {
          name: 'builds relation with default projection',
          builder: () => query(userModel).with('folders'),
          expected: { each: { folders: { projection: 'details' } } }
        },
        {
          name: 'builds multiple relations',
          builder: () =>
            query(userModel).with('folders', 'short').with('memos', 'details'),
          expected: {
            each: {
              folders: { projection: 'short' },
              memos: { projection: 'details' }
            }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })

      test('builds nested relation with callback', () => {
        const result = query(userModel)
          .with('memos', q =>
            q
              .projection('short')
              .where(w => w.equals('user_id', 1))
              .take(5)
          )
          .build()

        expect(result).toEqual({
          each: {
            memos: {
              projection: 'short',
              where: { user_id: 1 },
              take: 5
            }
          }
        })
      })
    })

    describe('withCounts', () => {
      const testCases = [
        {
          name: 'builds withCounts with boolean',
          builder: () => query(userModel).withCounts('folders', true),
          expected: { withRelatedCounts: { folders: true } }
        },
        {
          name: 'builds withCounts with default parameter',
          builder: () => query(userModel).withCounts('folders'),
          expected: { withRelatedCounts: { folders: true } }
        },
        {
          name: 'builds withCounts with where object',
          builder: () =>
            query(userModel).withCounts('memos', { published: true }),
          expected: {
            withRelatedCounts: { memos: { published: true } }
          }
        },
        {
          name: 'builds multiple withCounts',
          builder: () =>
            query(userModel)
              .withCounts('folders', true)
              .withCounts('memos', { published: true }),
          expected: {
            withRelatedCounts: {
              folders: true,
              memos: { published: true }
            }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })

      test('builds withCounts with where callback', () => {
        const result = query(userModel)
          .withCounts('memos', w => w.equals('published', true))
          .build()

        expect(result).toEqual({
          withRelatedCounts: {
            memos: { where: { published: true } }
          }
        })
      })
    })

    describe('modifiers', () => {
      const testCases = [
        {
          name: 'builds single modifier',
          builder: () =>
            query(userModel).modifier('forRole', { role: 'admin' }),
          expected: { modifiers: { forRole: { role: 'admin' } } }
        },
        {
          name: 'builds modifier without params',
          builder: () => query(userModel).modifier('activeOnly'),
          expected: { modifiers: { activeOnly: {} } }
        },
        {
          name: 'builds multiple modifiers with chaining',
          builder: () =>
            query(userModel)
              .modifier('forRole', { role: 'admin' })
              .modifier('withMinPosts', { minCount: 10 }),
          expected: {
            modifiers: {
              forRole: { role: 'admin' },
              withMinPosts: { minCount: 10 }
            }
          }
        },
        {
          name: 'builds modifiers with object',
          builder: () =>
            query(userModel).modifier({
              forRole: { role: 'admin' },
              activeOnly: {}
            }),
          expected: {
            modifiers: {
              forRole: { role: 'admin' },
              activeOnly: {}
            }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('metadata', () => {
      const testCases = [
        {
          name: 'builds metadata',
          builder: () =>
            query(userModel).metadata({
              counts: { total: true, filtered: true }
            }),
          expected: {
            metadata: { counts: { total: true, filtered: true } }
          }
        },
        {
          name: 'builds counts shorthand',
          builder: () =>
            query(userModel).counts({ total: true, filtered: true }),
          expected: {
            metadata: { counts: { total: true, filtered: true } }
          }
        },
        {
          name: 'builds counts when metadata does not exist',
          builder: () => query(userModel).counts({ total: true }),
          expected: {
            metadata: { counts: { total: true } }
          }
        },
        {
          name: 'builds counts when metadata already exists',
          builder: () =>
            query(userModel).metadata({ foo: 'bar' }).counts({ total: true }),
          expected: {
            metadata: { foo: 'bar', counts: { total: true } }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('query execution', () => {
      test('executes query and returns data', async () => {
        const result = await query(userModel)
          .projection('details')
          .where(w => w.equals('role', 'admin'))
          .execute(db)

        expect(result.data).toHaveLength(1)
        expect(result.data[0]).toMatchObject({
          id: 1,
          name: 'Alice',
          role: 'admin'
        })
      })
    })

    describe('error handling', () => {
      test('throws error for invalid relation', () => {
        expect(() => {
          query(userModel)
            .with('nonexistent', q => q.projection('details'))
            .build()
        }).toThrow("Relation 'nonexistent' not found in model")
      })
    })
  })

  describe('CountsBuilder', () => {
    beforeEach(async () => {
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' },
        { id: 3, name: 'Charlie', email: 'charlie@example.com', role: 'user' }
      ])
    })

    describe('basic building', () => {
      const testCases = [
        {
          name: 'builds total count',
          builder: () => countQuery(userModel).total(),
          expected: { counts: { total: true } }
        },
        {
          name: 'builds filtered count',
          builder: () => countQuery(userModel).filtered(),
          expected: { counts: { filtered: true } }
        },
        {
          name: 'builds both total and filtered',
          builder: () => countQuery(userModel).total().filtered(),
          expected: { counts: { total: true, filtered: true } }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('with where conditions', () => {
      const testCases = [
        {
          name: 'builds with fluent where',
          builder: () =>
            countQuery(userModel)
              .where(w => w.equals('role', 'admin'))
              .total()
              .filtered(),
          expected: {
            where: { role: 'admin' },
            counts: { total: true, filtered: true }
          }
        },
        {
          name: 'builds with object where',
          builder: () =>
            countQuery(userModel).where({ role: 'admin' }).filtered(),
          expected: {
            where: { role: 'admin' },
            counts: { filtered: true }
          }
        },
        {
          name: 'chains multiple where calls',
          builder: () =>
            countQuery(userModel)
              .where({ role: 'admin' })
              .where({ active: true })
              .total(),
          expected: {
            where: { role: 'admin', active: true },
            counts: { total: true }
          }
        },
        {
          name: 'merges where when called with object twice',
          builder: () =>
            countQuery(userModel)
              .where({ role: 'admin' })
              .where({ active: true })
              .where({ verified: true })
              .total(),
          expected: {
            where: { role: 'admin', active: true, verified: true },
            counts: { total: true }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('with modifiers', () => {
      const testCases = [
        {
          name: 'builds single modifier',
          builder: () =>
            countQuery(userModel)
              .total()
              .filtered()
              .modifier('inactive', { active: false }),
          expected: {
            counts: {
              total: true,
              filtered: true,
              modifiers: { inactive: { active: false } }
            }
          }
        },
        {
          name: 'builds modifier without params',
          builder: () => countQuery(userModel).total().modifier('activeOnly'),
          expected: {
            counts: {
              total: true,
              modifiers: { activeOnly: {} }
            }
          }
        },
        {
          name: 'builds multiple modifiers with chaining',
          builder: () =>
            countQuery(userModel)
              .total()
              .modifier('inactive', { active: false })
              .modifier('verified', { verified: true }),
          expected: {
            counts: {
              total: true,
              modifiers: {
                inactive: { active: false },
                verified: { verified: true }
              }
            }
          }
        },
        {
          name: 'builds modifiers with object',
          builder: () =>
            countQuery(userModel)
              .total()
              .modifier({
                inactive: { active: false },
                verified: { verified: true }
              }),
          expected: {
            counts: {
              total: true,
              modifiers: {
                inactive: { active: false },
                verified: { verified: true }
              }
            }
          }
        },
        {
          name: 'merges modifiers when called with object twice',
          builder: () =>
            countQuery(userModel)
              .total()
              .modifier({ inactive: { active: false } })
              .modifier({ verified: { verified: true } }),
          expected: {
            counts: {
              total: true,
              modifiers: {
                inactive: { active: false },
                verified: { verified: true }
              }
            }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('query execution', () => {
      test('executes and returns counts', async () => {
        const result = await countQuery(userModel)
          .where(w => w.equals('role', 'admin'))
          .total()
          .filtered()
          .execute(db)

        expect(result).toEqual({ total: 3, filtered: 1 })
      })
    })
  })

  describe('ExistsBuilder', () => {
    beforeEach(async () => {
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' }
      ])

      await db('memo').insert([
        { id: 1, content: 'Meeting notes', user_id: 1, folder_id: 1 }
      ])
    })

    describe('basic building', () => {
      const testCases = [
        {
          name: 'builds empty config',
          builder: () => existsQuery(userModel),
          expected: {}
        },
        {
          name: 'builds with where',
          builder: () =>
            existsQuery(userModel).where(w => w.equals('role', 'admin')),
          expected: { where: { role: 'admin' } }
        },
        {
          name: 'builds with object where',
          builder: () => existsQuery(userModel).where({ role: 'admin' }),
          expected: { where: { role: 'admin' } }
        },
        {
          name: 'merges where when called with object twice',
          builder: () =>
            existsQuery(userModel)
              .where({ role: 'admin' })
              .where({ active: true })
              .where({ verified: true }),
          expected: {
            where: { role: 'admin', active: true, verified: true }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('with modifiers', () => {
      const testCases = [
        {
          name: 'builds single modifier',
          builder: () =>
            existsQuery(userModel).modifier('forRole', { role: 'admin' }),
          expected: { modifiers: { forRole: { role: 'admin' } } }
        },
        {
          name: 'builds multiple modifiers with chaining',
          builder: () =>
            existsQuery(userModel)
              .modifier('forRole', { role: 'admin' })
              .modifier('activeOnly'),
          expected: {
            modifiers: {
              forRole: { role: 'admin' },
              activeOnly: {}
            }
          }
        },
        {
          name: 'builds modifiers with object',
          builder: () =>
            existsQuery(userModel).modifier({
              forRole: { role: 'admin' },
              activeOnly: {}
            }),
          expected: {
            modifiers: {
              forRole: { role: 'admin' },
              activeOnly: {}
            }
          }
        },
        {
          name: 'merges modifiers when called with object twice',
          builder: () =>
            existsQuery(userModel)
              .modifier({ forRole: { role: 'admin' } })
              .modifier({ activeOnly: {} }),
          expected: {
            modifiers: {
              forRole: { role: 'admin' },
              activeOnly: {}
            }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('combining where and modifiers', () => {
      const testCases = [
        {
          name: 'builds with both where and modifier',
          builder: () =>
            existsQuery(userModel)
              .where(w => w.equals('active', true))
              .modifier('forRole', { role: 'admin' }),
          expected: {
            where: { active: true },
            modifiers: { forRole: { role: 'admin' } }
          }
        }
      ]

      test.each(testCases)('$name', ({ builder, expected }) => {
        const result = builder().build()
        expect(result).toEqual(expected)
      })
    })

    describe('query execution', () => {
      test('executes and returns true when exists', async () => {
        const result = await existsQuery(userModel)
          .where(w => w.equals('role', 'admin'))
          .execute(db)

        expect(result).toBe(true)
      })
    })
  })
})
