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

  describe('applyJoinConditions', () => {
    describe('operators', () => {
      const testCases = [
        {
          name: 'equals operator in join condition',
          parameters: {
            joinConditions: { user_id: { equals: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` = 1',
            bindings: []
          }
        },
        {
          name: 'not operator in join condition',
          parameters: {
            joinConditions: { user_id: { not: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` != 1',
            bindings: []
          }
        },
        {
          name: 'gt operator in join condition',
          parameters: {
            joinConditions: { user_id: { gt: 0 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` > 0',
            bindings: []
          }
        },
        {
          name: 'gte operator in join condition',
          parameters: {
            joinConditions: { user_id: { gte: 0 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` >= 0',
            bindings: []
          }
        },
        {
          name: 'lt operator in join condition',
          parameters: {
            joinConditions: { user_id: { lt: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` < 1',
            bindings: []
          }
        },
        {
          name: 'lte operator in join condition',
          parameters: {
            joinConditions: { user_id: { lte: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` <= 1',
            bindings: []
          }
        },
        {
          name: 'contains operator in join condition',
          parameters: {
            joinConditions: { name: { contains: 'Test' } }
          },
          expected: {
            sql: 'left join `user` on `user`.`name` like `%Test%`',
            bindings: []
          }
        },
        {
          name: 'startsWith operator in join condition',
          parameters: {
            joinConditions: { name: { startsWith: 'Test' } }
          },
          expected: {
            sql: 'left join `user` on `user`.`name` like `Test%`',
            bindings: []
          }
        },
        {
          name: 'endsWith operator in join condition',
          parameters: {
            joinConditions: { name: { endsWith: 'Test' } }
          },
          expected: {
            sql: 'left join `user` on `user`.`name` like `%Test`',
            bindings: []
          }
        },
        {
          name: 'in operator in join condition',
          parameters: {
            joinConditions: { user_id: { in: [1, 2] } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` in (?, ?)',
            bindings: [1, 2]
          }
        },
        {
          name: 'notIn operator in join condition',
          parameters: {
            joinConditions: { user_id: { notIn: [1, 2] } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` not in (?, ?)',
            bindings: [1, 2]
          }
        },
        {
          name: 'isNull operator in join condition',
          parameters: {
            joinConditions: { deleted_at: { isNull: true } }
          },
          expected: {
            sql: 'left join `user` on `user`.`deleted_at` is null',
            bindings: []
          }
        },
        {
          name: 'isNotNull operator in join condition',
          parameters: {
            joinConditions: { email: { isNotNull: true } }
          },
          expected: {
            sql: 'left join `user` on `user`.`email` is not null',
            bindings: []
          }
        },
        {
          name: 'direct null assignment in join condition',
          parameters: {
            joinConditions: { middle_name: null }
          },
          expected: {
            sql: 'left join `user` on `user`.`middle_name` is null',
            bindings: []
          }
        },
        {
          name: 'direct value equality in join condition',
          parameters: {
            joinConditions: { user_id: 1 }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` = 1',
            bindings: []
          }
        },
        {
          name: 'multiple conditions on same field in join',
          parameters: {
            joinConditions: { user_id: { gt: 0, not: 2 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` > 0 and `user`.`user_id` != 2',
            bindings: []
          }
        },
        {
          name: 'conditions ignored when _condition is false in join',
          parameters: {
            joinConditions: {
              user_id: { equals: 1, _condition: false }
            }
          },
          expected: {
            sql: 'left join `user`',
            bindings: []
          }
        },
        {
          name: 'conditions applied when _condition is true in join',
          parameters: {
            joinConditions: {
              user_id: { equals: 1, _condition: true }
            }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` = 1',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        query.leftJoin('user', function () {
          knexTools.applyJoinConditions(this, 'user', parameters.joinConditions)
        })
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('logical operators', () => {
      const testCases = [
        {
          name: 'AND logical operator in join conditions',
          parameters: {
            joinConditions: {
              AND: [
                { active: true },
                { verified: true },
                { role: { not: 'banned' } }
              ]
            }
          },
          expected: {
            sql: 'left join `user` on (`user`.`active` = `true`) and (`user`.`verified` = `true`) and (`user`.`role` != `banned`)',
            bindings: []
          }
        },
        {
          name: 'OR logical operator in join conditions',
          parameters: {
            joinConditions: {
              OR: [{ role: 'admin' }, { premium: true }]
            }
          },
          expected: {
            sql: 'left join `user` on (`user`.`role` = `admin`) or (`user`.`premium` = `true`)',
            bindings: []
          }
        },
        {
          name: 'NOT logical operator in join conditions',
          parameters: {
            joinConditions: {
              NOT: {
                banned: true,
                deleted_at: { isNotNull: true }
              }
            }
          },
          expected: {
            sql: 'left join `user` on ((`user`.`banned` != `true` and `user`.`deleted_at` is not null))',
            bindings: []
          }
        },
        {
          name: 'complex nested logical operators in join conditions',
          parameters: {
            joinConditions: {
              AND: [
                { active: true },
                {
                  OR: [
                    { role: 'admin' },
                    {
                      AND: [{ premium: true }, { verified: true }]
                    }
                  ]
                }
              ]
            }
          },
          expected: {
            sql: 'left join `user` on (`user`.`active` = `true`) and ((`user`.`role` = `admin`) or ((`user`.`premium` = `true`) and (`user`.`verified` = `true`)))',
            bindings: []
          }
        },
        {
          name: 'AND conditions with _condition flags',
          parameters: {
            joinConditions: {
              AND: [
                { active: true, _condition: true },
                { verified: true, _condition: false },
                { role: 'admin' }
              ]
            }
          },
          expected: {
            sql: 'left join `user` on (`user`.`active` = `true`) and (`user`.`role` = `admin`)',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        query.leftJoin('user', function () {
          knexTools.applyJoinConditions(this, 'user', parameters.joinConditions)
        })
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })
  })

  describe('processJoins', () => {
    describe('one to many join', () => {
      const testCases = [
        {
          name: 'one to many join without conditions',
          parameters: {
            join: {
              children: {}
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` left join `folder` as `children` on `children`.`parent_id` = `folder`.`id`',
            bindings: []
          }
        },
        {
          name: 'one to many join with where conditions',
          parameters: {
            join: {
              children: {
                where: {
                  user_id: { gt: 0, not: 2 }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` left join `folder` as `children` on `children`.`parent_id` = `folder`.`id` and `children`.`user_id` > 0 and `children`.`user_id` != 2',
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
