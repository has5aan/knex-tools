const { createInMemoryEmptyDatabase } = require('./setup')
const knexTools = require('../src/knex-tools')
const folderModel = require('./models/folder.model')

describe('knexTools', () => {
  let db

  beforeEach(async () => {
    db = await createInMemoryEmptyDatabase()
  })

  afterEach(async () => {
    await db.destroy()
  })

  describe('applyWhereClauses', () => {
    describe('operators', () => {
      const testCases = [
        {
          name: 'equals operator',
          parameters: {
            criteria: { where: { user_id: { equals: 1 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'not operator',
          parameters: {
            criteria: { where: { user_id: { not: 1 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` != ?',
            bindings: [1]
          }
        },
        {
          name: 'gt operator',
          parameters: {
            criteria: { where: { user_id: { gt: 0 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` > ?',
            bindings: [0]
          }
        },
        {
          name: 'gte operator',
          parameters: {
            criteria: { where: { user_id: { gte: 0 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` >= ?',
            bindings: [0]
          }
        },
        {
          name: 'lt operator',
          parameters: {
            criteria: { where: { user_id: { lt: 1 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` < ?',
            bindings: [1]
          }
        },
        {
          name: 'lte operator',
          parameters: {
            criteria: { where: { user_id: { lte: 1 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` <= ?',
            bindings: [1]
          }
        },
        {
          name: 'contains operator',
          parameters: {
            criteria: { where: { name: { contains: 'Test' } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`name` like ?',
            bindings: ['%Test%']
          }
        },
        {
          name: 'startsWith operator',
          parameters: {
            criteria: { where: { name: { startsWith: 'Test' } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`name` like ?',
            bindings: ['Test%']
          }
        },
        {
          name: 'endsWith operator',
          parameters: {
            criteria: { where: { name: { endsWith: 'Test' } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`name` like ?',
            bindings: ['%Test']
          }
        },
        {
          name: 'in operator',
          parameters: {
            criteria: { where: { user_id: { in: [1, 2] } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` in (?, ?)',
            bindings: [1, 2]
          }
        },
        {
          name: 'notIn operator',
          parameters: {
            criteria: { where: { user_id: { notIn: [1, 2] } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` not in (?, ?)',
            bindings: [1, 2]
          }
        },
        {
          name: 'isNull operator',
          parameters: {
            criteria: { where: { user_id: { isNull: true } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` is null',
            bindings: []
          }
        },
        {
          name: 'isNotNull operator',
          parameters: {
            criteria: { where: { user_id: { isNotNull: true } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` is not null',
            bindings: []
          }
        },
        {
          name: 'defaults to isNull operator if conditions of fields are null',
          parameters: {
            criteria: { where: { user_id: null } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` is null',
            bindings: []
          }
        },
        {
          name: 'defaults to equals operator if no operator is specfied',
          parameters: {
            criteria: { where: { user_id: 1 } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'multiple conditions on same field defaults to AND as logical operator',
          parameters: {
            criteria: { where: { user_id: { gt: 0, not: 2 } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` > ? and `folder`.`user_id` != ?',
            bindings: [0, 2]
          }
        },
        {
          name: 'conditions are ignored if the _condition field is false for a filter field',
          parameters: {
            criteria: {
              where: { user_id: { gt: 0, not: 2, _condition: false } }
            }
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        knexTools.applyWhereClauses(query, 'folder', parameters.criteria)
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('logical operators', () => {
      const testCases = [
        {
          name: 'multiple conditions for a single field defaults to AND as the logical operator',
          parameters: {
            criteria: {
              where: {
                name: 'Test Folder',
                user_id: { gt: 0, not: 2 }
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`name` = ? and `folder`.`user_id` > ? and `folder`.`user_id` != ?',
            bindings: ['Test Folder', 0, 2]
          }
        },
        {
          name: 'AND logical operator',
          parameters: {
            criteria: {
              where: {
                AND: [
                  { user_id: { gt: 0, not: 2 } },
                  { name: 'Test Folder' },
                  { parent_id: null }
                ]
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where (`folder`.`user_id` > ? and `folder`.`user_id` != ?) and (`folder`.`name` = ?) and (`folder`.`parent_id` is null)',
            bindings: [0, 2, 'Test Folder']
          }
        },
        {
          name: 'OR logical operator',
          parameters: {
            criteria: {
              where: {
                OR: [
                  { user_id: { gt: 0, not: 2 } },
                  { name: 'Test Folder' },
                  { user_id: { not: 1 } }
                ]
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where (`folder`.`user_id` > ? and `folder`.`user_id` != ?) or (`folder`.`name` = ?) or (`folder`.`user_id` != ?)',
            bindings: [0, 2, 'Test Folder']
          }
        },
        {
          name: 'NOT logical operator',
          parameters: {
            criteria: {
              where: {
                NOT: {
                  user_id: { gt: 0, not: 2 },
                  name: 'Test Folder'
                }
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where not (`folder`.`user_id` > ? and `folder`.`user_id` != ? and `folder`.`name` = ?)',
            bindings: [0, 2, 'Test Folder']
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        knexTools.applyWhereClauses(query, 'folder', parameters.criteria)
        expect(query.toSQL().sql).toMatch(expected.sql)
      })
    })
  })

  describe('applyPagingClauses', () => {
    const testCases = [
      {
        name: 'applies skip and take',
        parameters: {
          criteria: {
            skip: 10,
            take: 20
          }
        },
        expected: {
          sql: 'select * from `folder` limit ? offset ?',
          bindings: [20, 10]
        }
      },
      {
        name: 'applies only skip',
        parameters: {
          criteria: {
            skip: 10
          }
        },
        expected: {
          sql: 'select * from `folder` limit ? offset ?',
          bindings: [-1, 10]
        }
      },
      {
        name: 'applies only take',
        parameters: {
          criteria: {
            take: 20
          }
        },
        expected: {
          sql: 'select * from `folder` limit ?',
          bindings: [20]
        }
      }
    ]

    test.each(
      testCases.filter(testCase => testCase.name === 'applies only skip')
    )('$name', ({ parameters, expected }) => {
      const query = db('folder').select('*')
      knexTools.applyPagingClauses(query, parameters.criteria)
      expect(query.toSQL().sql).toMatch(expected.sql)
      expect(query.toSQL().bindings).toEqual(expected.bindings)
    })
  })

  describe('applySortingClauses', () => {
    const testCases = [
      {
        name: 'applies single field sort',
        parameters: {
          criteria: {
            name: 'asc'
          }
        },
        expected: {
          sql: 'select * from `folder` order by `folder`.`name` asc',
          bindings: []
        }
      },
      {
        name: 'applies multiple field sort',
        parameters: {
          criteria: {
            name: 'asc',
            created_at: 'desc'
          }
        },
        expected: {
          sql: 'select * from `folder` order by `folder`.`name` asc, `folder`.`created_at` desc',
          bindings: []
        }
      },
      {
        name: 'applies default sort',
        parameters: {
          criteria: {},
          defaultSortOptions: {
            field: 'name',
            direction: 'asc'
          }
        },
        expected: {
          sql: 'select * from `folder` order by `folder`.`name` asc',
          bindings: []
        }
      }
    ]

    test.each(testCases)('$name', ({ parameters, expected }) => {
      const query = db('folder').select('*')
      knexTools.applySortingClauses(
        query,
        'folder',
        parameters.criteria,
        parameters.defaultSortOptions
      )
      expect(query.toSQL().sql).toMatch(expected.sql)
      expect(query.toSQL().bindings).toEqual(expected.bindings)
    })
  })

  describe('processJoins', () => {
    describe('one to many join', () => {
      const testCases = [
        {
          name: 'one to many join',
          parameters: {
            join: {
              children: {
                join: {
                  children: {}
                },
                where: {
                  user_id: { gt: 0, not: 2 }
                }
              }
            }
          },
          expected: {
            sql: 'select * from `folder` left join `folder` as `children` on `folder`.`id` = `children`.`parent_id`',
            bindings: []
          }
        }
      ]

      testCases.forEach(({ name, parameters, expected }) => {
        it(name, () => {
          const query = db('folder').select('*')
          knexTools.processJoins(
            query,
            'folder',
            parameters.join,
            folderModel.relations
          )
          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })
    })
  })
})

// const _ = require('lodash');

// const groupedResults = _(results)
//   .groupBy('id')
//   .map(rows => {
//     const parent = _.omit(rows[0], key => key.startsWith('children_'));
//     const children = _(rows)
//       .filter('children_id')
//       .map(row => _(row)
//         .pickBy((value, key) => key.startsWith('children_'))
//         .mapKeys((value, key) => key.replace('children_', ''))
//         .value()
//       )
//       .value();

//     return { ...parent, children };
//   })
//   .value();
