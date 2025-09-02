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
          name: 'hasAll operator with single value',
          parameters: {
            criteria: {
              where: {
                tags: { hasAll: ['urgent'] }
              }
            }
          },
          expected: {
            sql: 'with `tags_hasall_values` as (select ? as value) select * from `folder` where exists (select 1 from `tags_hasall_values` where `tags_hasall_values`.value = `folder`.`tags` having COUNT(*) = ?)',
            bindings: ['urgent', 1]
          }
        },
        {
          name: 'hasAll operator with multiple values',
          parameters: {
            criteria: {
              where: {
                tags: { hasAll: ['urgent', 'priority'] }
              }
            }
          },
          expected: {
            sql: 'with `tags_hasall_values` as (select ? as value union select ? as value) select * from `folder` where exists (select 1 from `tags_hasall_values` where `tags_hasall_values`.value = `folder`.`tags` having COUNT(*) = ?)',
            bindings: ['urgent', 'priority', 2]
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
            sql: 'left join `user` on `user`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'not operator in join condition',
          parameters: {
            joinConditions: { user_id: { not: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` != ?',
            bindings: [1]
          }
        },
        {
          name: 'gt operator in join condition',
          parameters: {
            joinConditions: { user_id: { gt: 0 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` > ?',
            bindings: [0]
          }
        },
        {
          name: 'gte operator in join condition',
          parameters: {
            joinConditions: { user_id: { gte: 0 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` >= ?',
            bindings: [0]
          }
        },
        {
          name: 'lt operator in join condition',
          parameters: {
            joinConditions: { user_id: { lt: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` < ?',
            bindings: [1]
          }
        },
        {
          name: 'lte operator in join condition',
          parameters: {
            joinConditions: { user_id: { lte: 1 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` <= ?',
            bindings: [1]
          }
        },
        {
          name: 'contains operator in join condition',
          parameters: {
            joinConditions: { name: { contains: 'Test' } }
          },
          expected: {
            sql: 'left join `user` on `user`.`name` like ?',
            bindings: ['%Test%']
          }
        },
        {
          name: 'startsWith operator in join condition',
          parameters: {
            joinConditions: { name: { startsWith: 'Test' } }
          },
          expected: {
            sql: 'left join `user` on `user`.`name` like ?',
            bindings: ['Test%']
          }
        },
        {
          name: 'endsWith operator in join condition',
          parameters: {
            joinConditions: { name: { endsWith: 'Test' } }
          },
          expected: {
            sql: 'left join `user` on `user`.`name` like ?',
            bindings: ['%Test']
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
            sql: 'left join `user` on `user`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'multiple conditions on same field in join',
          parameters: {
            joinConditions: { user_id: { gt: 0, not: 2 } }
          },
          expected: {
            sql: 'left join `user` on `user`.`user_id` > ? and `user`.`user_id` != ?',
            bindings: [0, 2]
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
            sql: 'left join `user` on `user`.`user_id` = ?',
            bindings: [1]
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
            sql: 'left join `user` on (`user`.`active` = ?) and (`user`.`verified` = ?) and (`user`.`role` != ?)',
            bindings: [true, true, 'banned']
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
            sql: 'left join `user` on (`user`.`role` = ?) or (`user`.`premium` = ?)',
            bindings: ['admin', true]
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
            sql: 'left join `user` on (`user`.`active` = ?) and ((`user`.`role` = ?) or ((`user`.`premium` = ?) and (`user`.`verified` = ?)))',
            bindings: [true, 'admin', true, true]
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
            sql: 'left join `user` on (`user`.`active` = ?) and (`user`.`role` = ?)',
            bindings: [true, 'admin']
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
          name: 'one to many join with on conditions',
          parameters: {
            join: {
              children: {
                on: {
                  user_id: { gt: 0, not: 2 }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` left join `folder` as `children` on `children`.`parent_id` = `folder`.`id` and `children`.`user_id` > ? and `children`.`user_id` != ?',
            bindings: [0, 2]
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

    describe('joins with both on and where conditions', () => {
      const testCases = [
        {
          name: 'belongsTo join with both on and where conditions',
          parameters: {
            join: {
              user: {
                on: {
                  active: true
                },
                where: {
                  role: 'admin'
                }
              }
            }
          },
          expected: {
            sql: 'select *, `user`.`id` as `user_id`, `user`.`name` as `user_name`, `user`.`email` as `user_email`, `user`.`role` as `user_role`, `user`.`active` as `user_active`, `user`.`premium` as `user_premium`, `user`.`verified` as `user_verified`, `user`.`created_at` as `user_created_at`, `user`.`updated_at` as `user_updated_at` from `folder` left join `user` as `user` on `user`.`id` = `folder`.`user_id` and `user`.`active` = ? where `user`.`role` = ?',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'hasMany join with complex on and where conditions',
          parameters: {
            join: {
              children: {
                on: {
                  user_id: { gt: 0 },
                  name: { contains: 'test' }
                },
                where: {
                  created_at: { gte: '2023-01-01' },
                  updated_at: { isNotNull: true }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` left join `folder` as `children` on `children`.`parent_id` = `folder`.`id` and `children`.`user_id` > ? and `children`.`name` like ? where `children`.`created_at` >= ? and `children`.`updated_at` is not null',
            bindings: [0, '%test%', '2023-01-01']
          }
        },
        {
          name: 'join with logical operators in both on and where',
          parameters: {
            join: {
              user: {
                on: {
                  OR: [{ active: true }, { verified: true }]
                },
                where: {
                  AND: [{ role: { in: ['admin', 'user'] } }, { premium: true }]
                }
              }
            }
          },
          expected: {
            sql: 'select *, `user`.`id` as `user_id`, `user`.`name` as `user_name`, `user`.`email` as `user_email`, `user`.`role` as `user_role`, `user`.`active` as `user_active`, `user`.`premium` as `user_premium`, `user`.`verified` as `user_verified`, `user`.`created_at` as `user_created_at`, `user`.`updated_at` as `user_updated_at` from `folder` left join `user` as `user` on `user`.`id` = `folder`.`user_id` or (`user`.`active` = ?) or (`user`.`verified` = ?) where (`user`.`role` in (?, ?)) and (`user`.`premium` = ?)',
            bindings: [true, true, 'admin', 'user', true]
          }
        },
        {
          name: 'enforce join type with both on and where conditions',
          parameters: {
            join: {
              user: {
                type: 'enforce',
                on: {
                  active: true,
                  role: { not: 'guest' }
                },
                where: {
                  verified: true,
                  premium: { isNotNull: true }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `user`.`id` as `user_id`, `user`.`name` as `user_name`, `user`.`email` as `user_email`, `user`.`role` as `user_role`, `user`.`active` as `user_active`, `user`.`premium` as `user_premium`, `user`.`verified` as `user_verified`, `user`.`created_at` as `user_created_at`, `user`.`updated_at` as `user_updated_at` from `folder` inner join `user` as `user` on `user`.`id` = `folder`.`user_id` and `user`.`active` = ? and `user`.`role` != ? where `user`.`verified` = ? and `user`.`premium` is not null',
            bindings: [true, 'guest', true]
          }
        },
        {
          name: 'join with conditional flags in both on and where',
          parameters: {
            join: {
              user: {
                on: {
                  active: { equals: true, _condition: true },
                  role: { equals: 'admin', _condition: false }
                },
                where: {
                  verified: { equals: true, _condition: true },
                  premium: { equals: true, _condition: false }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `user`.`id` as `user_id`, `user`.`name` as `user_name`, `user`.`email` as `user_email`, `user`.`role` as `user_role`, `user`.`active` as `user_active`, `user`.`premium` as `user_premium`, `user`.`verified` as `user_verified`, `user`.`created_at` as `user_created_at`, `user`.`updated_at` as `user_updated_at` from `folder` left join `user` as `user` on `user`.`id` = `folder`.`user_id` and `user`.`active` = ? where `user`.`verified` = ?',
            bindings: [true, true]
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

    describe('manyToMany join tests', () => {
      const testCases = [
        {
          name: 'manyToMany join without conditions',
          parameters: {
            join: {
              tags: {}
            }
          },
          expected: {
            sql: 'select *, `tags`.`id` as `tags_id`, `tags`.`name` as `tags_name`, `tags`.`created_at` as `tags_created_at`, `tags`.`updated_at` as `tags_updated_at` from `memo` left join `memo_tag` on `memo`.`id` = `memo_tag`.`memo_id` left join `tag` as `tags` on `memo_tag`.`tag_id` = `tags`.`id`',
            bindings: []
          }
        },
        {
          name: 'manyToMany join with on conditions',
          parameters: {
            join: {
              tags: {
                on: {
                  name: { contains: 'important' },
                  created_at: { gte: '2023-01-01' }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `tags`.`id` as `tags_id`, `tags`.`name` as `tags_name`, `tags`.`created_at` as `tags_created_at`, `tags`.`updated_at` as `tags_updated_at` from `memo` left join `memo_tag` on `memo`.`id` = `memo_tag`.`memo_id` left join `tag` as `tags` on `memo_tag`.`tag_id` = `tags`.`id` and `tags`.`name` like ? and `tags`.`created_at` >= ?',
            bindings: ['%important%', '2023-01-01']
          }
        },
        {
          name: 'manyToMany join with both on and where conditions',
          parameters: {
            join: {
              tags: {
                on: {
                  name: { startsWith: 'tag_' }
                },
                where: {
                  created_at: { isNotNull: true },
                  updated_at: { gte: '2023-06-01' }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `tags`.`id` as `tags_id`, `tags`.`name` as `tags_name`, `tags`.`created_at` as `tags_created_at`, `tags`.`updated_at` as `tags_updated_at` from `memo` left join `memo_tag` on `memo`.`id` = `memo_tag`.`memo_id` left join `tag` as `tags` on `memo_tag`.`tag_id` = `tags`.`id` and `tags`.`name` like ? where `tags`.`created_at` is not null and `tags`.`updated_at` >= ?',
            bindings: ['tag_%', '2023-06-01']
          }
        },
        {
          name: 'manyToMany enforce join with logical operators',
          parameters: {
            join: {
              tags: {
                type: 'enforce',
                on: {
                  OR: [{ name: 'urgent' }, { name: 'priority' }]
                },
                where: {
                  AND: [
                    { created_at: { gte: '2023-01-01' } },
                    { updated_at: { isNotNull: true } }
                  ]
                }
              }
            }
          },
          expected: {
            sql: 'select *, `tags`.`id` as `tags_id`, `tags`.`name` as `tags_name`, `tags`.`created_at` as `tags_created_at`, `tags`.`updated_at` as `tags_updated_at` from `memo` inner join `memo_tag` on `memo`.`id` = `memo_tag`.`memo_id` inner join `tag` as `tags` on `memo_tag`.`tag_id` = `tags`.`id` or (`tags`.`name` = ?) or (`tags`.`name` = ?) where (`tags`.`created_at` >= ?) and (`tags`.`updated_at` is not null)',
            bindings: ['urgent', 'priority', '2023-01-01']
          }
        }
      ]

      testCases.forEach(({ name, parameters, expected }) => {
        it(name, () => {
          const memoModel = require('./models/memo.model')
          const query = db('memo').select('*')
          knexTools.processJoins(
            query,
            'memo',
            parameters.join,
            memoModel.relations
          )
          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })
    })

    describe('additional join relationship tests', () => {
      const testCases = [
        {
          name: 'belongsTo parent-child self-join',
          model: 'folder',
          parameters: {
            join: {
              parent: {
                on: {
                  name: { contains: 'parent' }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `parent`.`id` as `parent_id`, `parent`.`user_id` as `parent_user_id`, `parent`.`parent_id` as `parent_parent_id`, `parent`.`name` as `parent_name`, `parent`.`created_at` as `parent_created_at`, `parent`.`updated_at` as `parent_updated_at` from `folder` left join `folder` as `parent` on `parent`.`id` = `folder`.`parent_id` and `parent`.`name` like ?',
            bindings: ['%parent%']
          }
        },
        {
          name: 'hasMany children self-join with where filter',
          model: 'folder',
          parameters: {
            join: {
              children: {
                on: {},
                where: {
                  created_at: { gte: '2023-01-01' }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` left join `folder` as `children` on `children`.`parent_id` = `folder`.`id` where `children`.`created_at` >= ?',
            bindings: ['2023-01-01']
          }
        },
        {
          name: 'hasMany memos with enforce join type',
          model: 'folder',
          parameters: {
            join: {
              memos: {
                type: 'enforce',
                on: {
                  content: { contains: 'important' }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `memos`.`id` as `memos_id`, `memos`.`user_id` as `memos_user_id`, `memos`.`folder_id` as `memos_folder_id`, `memos`.`content` as `memos_content`, `memos`.`created_at` as `memos_created_at`, `memos`.`updated_at` as `memos_updated_at` from `folder` inner join `memo` as `memos` on `memos`.`folder_id` = `folder`.`id` and `memos`.`content` like ?',
            bindings: ['%important%']
          }
        }
      ]

      testCases.forEach(({ name, model, parameters, expected }) => {
        it(name, () => {
          const modelDef = require(`./models/${model}.model`)
          const query = db(model).select('*')
          knexTools.processJoins(
            query,
            model,
            parameters.join,
            modelDef.relations
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
