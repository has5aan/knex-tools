const { createInMemoryEmptyDatabase } = require('./setup')
const knexTools = require('../src/knex-tools')

// Global model imports
const userModel = require('./models/user.model')
const memoModel = require('./models/memo.model')
const folderModel = require('./models/folder.model')
const longMemoModel = require('./models/long-memo.model')

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
          name: 'not operator with null value',
          parameters: {
            criteria: { where: { deleted_at: { not: null } } }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`deleted_at` is not null',
            bindings: []
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
          name: 'defaults to equals operator if no operator is specified',
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
            bindings: [0, 2, 'Test Folder', 1]
          }
        },
        {
          name: 'AND logical operator with null values in where clauses',
          parameters: {
            criteria: {
              where: {
                AND: [
                  { user_id: { equals: 1 } },
                  null,
                  { name: { equals: 'Test' } },
                  undefined
                ]
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where (`folder`.`user_id` = ?) and (`folder`.`name` = ?)',
            bindings: [1, 'Test']
          }
        },
        {
          name: 'OR logical operator with null values in where clauses',
          parameters: {
            criteria: {
              where: {
                OR: [
                  { user_id: { equals: 1 } },
                  null,
                  { name: { equals: 'Test' } },
                  undefined
                ]
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where (`folder`.`user_id` = ?) or (`folder`.`name` = ?)',
            bindings: [1, 'Test']
          }
        },
        {
          name: 'AND logical operator with only null values should not affect query',
          parameters: {
            criteria: {
              where: {
                AND: [null, undefined, null]
              }
            }
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'OR logical operator with only null values should not affect query',
          parameters: {
            criteria: {
              where: {
                OR: [null, undefined, null]
              }
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

    describe('edge cases', () => {
      const testCases = [
        {
          name: 'returns query unchanged when criteria is null',
          parameters: {
            criteria: null
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when criteria is undefined',
          parameters: {
            criteria: undefined
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when where clause is missing',
          parameters: {
            criteria: {}
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when where clause is null',
          parameters: {
            criteria: { where: null }
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'skips unknown underscore-prefixed fields',
          parameters: {
            criteria: {
              where: {
                user_id: 1,
                _customField: 'test',
                _internal: true
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'skips multiple unknown underscore-prefixed fields with complex query',
          parameters: {
            criteria: {
              where: {
                name: { contains: 'Work' },
                _metadata: { some: 'data' },
                user_id: { gt: 0 },
                _private: 'secret',
                _debug: true
              }
            }
          },
          expected: {
            sql: 'select * from `folder` where `folder`.`name` like ? and `folder`.`user_id` > ?',
            bindings: ['%Work%', 0]
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

    describe('_exists operator for RLS', () => {
      describe('belongsTo relationships', () => {
        const testCases = [
          {
            name: 'belongsTo with simple condition',
            parameters: {
              model: memoModel,
              where: {
                _exists: {
                  user: { id: 1 }
                }
              }
            },
            expected: {
              sql: 'select * from `memo` as `m` where exists (select 1 from `user` as `user` where `user`.`id` = m.user_id and `user`.`id` = ?)',
              bindings: [1]
            }
          },
          {
            name: 'belongsTo with multiple conditions',
            parameters: {
              model: memoModel,
              where: {
                _exists: {
                  user: {
                    role: { equals: 'admin' },
                    active: { equals: true }
                  }
                }
              }
            },
            expected: {
              sql: 'select * from `memo` as `m` where exists (select 1 from `user` as `user` where `user`.`id` = m.user_id and `user`.`role` = ? and `user`.`active` = ?)',
              bindings: ['admin', true]
            }
          }
        ]

        test.each(testCases)('$name', ({ parameters, expected }) => {
          const query = db(
            `${parameters.model.tableName} as ${parameters.model.alias}`
          ).select('*')
          knexTools.applyWhereClauses(
            query,
            parameters.model.alias,
            parameters,
            parameters.model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('hasMany relationships', () => {
        const testCases = [
          {
            name: 'hasMany with simple condition',
            parameters: {
              model: userModel,
              where: {
                _exists: {
                  memos: { content: { contains: 'important' } }
                }
              }
            },
            expected: {
              sql: 'select * from `user` as `u` where exists (select 1 from `memo` as `memos` where `memos`.`user_id` = u.id and `memos`.`content` like ?)',
              bindings: ['%important%']
            }
          }
        ]

        test.each(testCases)('$name', ({ parameters, expected }) => {
          const query = db(
            `${parameters.model.tableName} as ${parameters.model.alias}`
          ).select('*')
          knexTools.applyWhereClauses(
            query,
            parameters.model.alias,
            parameters,
            parameters.model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('manyToMany relationships', () => {
        const testCases = [
          {
            name: 'manyToMany with junction table',
            parameters: {
              model: memoModel,
              where: {
                _exists: {
                  tags: { name: { contains: 'urgent' } }
                }
              }
            },
            expected: {
              sql: 'select * from `memo` as `m` where exists (select 1 from `memo_tag` inner join `tag` as `tags` on `memo_tag`.`tag_id` = `tags`.`id` where `memo_tag`.`memo_id` = m.id and `tags`.`name` like ?)',
              bindings: ['%urgent%']
            }
          }
        ]

        test.each(testCases)('$name', ({ parameters, expected }) => {
          const query = db(
            `${parameters.model.tableName} as ${parameters.model.alias}`
          ).select('*')
          knexTools.applyWhereClauses(
            query,
            parameters.model.alias,
            parameters,
            parameters.model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('self-referencing relationships', () => {
        const testCases = [
          {
            name: 'self-referencing hasMany with aliases',
            parameters: {
              model: folderModel,
              where: {
                _exists: {
                  children: { name: { contains: 'sub' } }
                }
              }
            },
            expected: {
              sql: 'select * from `folder` as `f` where exists (select 1 from `folder` as `children` where `children`.`parent_id` = f.id and `children`.`name` like ?)',
              bindings: ['%sub%']
            }
          }
        ]

        test.each(testCases)('$name', ({ parameters, expected }) => {
          const query = db(
            `${parameters.model.tableName} as ${parameters.model.alias}`
          ).select('*')
          knexTools.applyWhereClauses(
            query,
            parameters.model.alias,
            parameters,
            parameters.model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('error handling', () => {
        const testCases = [
          {
            name: 'applyWhereClauses throws error when _exists used without relations provided',
            setup: () => db('memo').select('*'),
            parameters: {
              where: {
                _exists: {
                  user: { id: 1 }
                }
              }
            },
            expectedError:
              'Relations must be provided to use _exists functionality'
          },
          {
            name: 'applyWhereClauses throws error when _exists relation not found',
            setup: () => db('memo').select('*'),
            parameters: {
              where: {
                _exists: {
                  nonexistent: { id: 1 }
                }
              }
            },
            relations: memoModel.relations,
            expectedError:
              "Relation 'nonexistent' not found in relations config"
          }
        ]

        test.each(testCases)(
          '$name',
          ({ setup, parameters, relations, expectedError }) => {
            const query = setup()
            expect(() => {
              knexTools.applyWhereClauses(query, 'memo', parameters, relations)
            }).toThrow(expectedError)
          }
        )
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

    test.each(testCases)('$name', ({ parameters, expected }) => {
      const query = db('folder').select('*')
      knexTools.applyPagingClauses(query, parameters.criteria)
      expect(query.toSQL().sql).toMatch(expected.sql)
      expect(query.toSQL().bindings).toEqual(expected.bindings)
    })

    describe('edge cases', () => {
      const testCases = [
        {
          name: 'returns query unchanged when criteria is null',
          parameters: {
            criteria: null
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when criteria is undefined',
          parameters: {
            criteria: undefined
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when criteria is empty object',
          parameters: {
            criteria: {}
          },
          expected: {
            sql: 'select * from `folder`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        knexTools.applyPagingClauses(query, parameters.criteria)
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
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
            sql: 'select * from `folder` left join `user` on `user`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'not operator in join condition',
          parameters: {
            joinConditions: { user_id: { not: 1 } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` != ?',
            bindings: [1]
          }
        },
        {
          name: 'gt operator in join condition',
          parameters: {
            joinConditions: { user_id: { gt: 0 } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` > ?',
            bindings: [0]
          }
        },
        {
          name: 'gte operator in join condition',
          parameters: {
            joinConditions: { user_id: { gte: 0 } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` >= ?',
            bindings: [0]
          }
        },
        {
          name: 'lt operator in join condition',
          parameters: {
            joinConditions: { user_id: { lt: 1 } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` < ?',
            bindings: [1]
          }
        },
        {
          name: 'lte operator in join condition',
          parameters: {
            joinConditions: { user_id: { lte: 1 } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` <= ?',
            bindings: [1]
          }
        },
        {
          name: 'contains operator in join condition',
          parameters: {
            joinConditions: { name: { contains: 'Test' } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`name` like ?',
            bindings: ['%Test%']
          }
        },
        {
          name: 'startsWith operator in join condition',
          parameters: {
            joinConditions: { name: { startsWith: 'Test' } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`name` like ?',
            bindings: ['Test%']
          }
        },
        {
          name: 'endsWith operator in join condition',
          parameters: {
            joinConditions: { name: { endsWith: 'Test' } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`name` like ?',
            bindings: ['%Test']
          }
        },
        {
          name: 'in operator in join condition',
          parameters: {
            joinConditions: { user_id: { in: [1, 2] } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` in (?, ?)',
            bindings: [1, 2]
          }
        },
        {
          name: 'notIn operator in join condition',
          parameters: {
            joinConditions: { user_id: { notIn: [1, 2] } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` not in (?, ?)',
            bindings: [1, 2]
          }
        },
        {
          name: 'isNull operator in join condition',
          parameters: {
            joinConditions: { deleted_at: { isNull: true } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`deleted_at` is null',
            bindings: []
          }
        },
        {
          name: 'isNotNull operator in join condition',
          parameters: {
            joinConditions: { email: { isNotNull: true } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`email` is not null',
            bindings: []
          }
        },
        {
          name: 'not operator with null value in join condition',
          parameters: {
            joinConditions: { deleted_at: { not: null } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`deleted_at` is not null',
            bindings: []
          }
        },
        {
          name: 'direct null assignment in join condition',
          parameters: {
            joinConditions: { middle_name: null }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`middle_name` is null',
            bindings: []
          }
        },
        {
          name: 'direct value equality in join condition',
          parameters: {
            joinConditions: { user_id: 1 }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` = ?',
            bindings: [1]
          }
        },
        {
          name: 'multiple conditions on same field in join',
          parameters: {
            joinConditions: { user_id: { gt: 0, not: 2 } }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on `user`.`user_id` > ? and `user`.`user_id` != ?',
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
            sql: 'select * from `folder` left join `user`',
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
            sql: 'select * from `folder` left join `user` on `user`.`user_id` = ?',
            bindings: [1]
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        query.leftJoin('user', function () {
          knexTools.applyJoinConditions(this, 'user', parameters.joinConditions)
        })
        expect(query.toSQL().sql).toBe(expected.sql)
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
            sql: 'select * from `folder` left join `user` on (`user`.`active` = ?) and (`user`.`verified` = ?) and (`user`.`role` != ?)',
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
            sql: 'select * from `folder` left join `user` on (`user`.`role` = ?) or (`user`.`premium` = ?)',
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
            sql: 'select * from `folder` left join `user` on (`user`.`active` = ?) and ((`user`.`role` = ?) or ((`user`.`premium` = ?) and (`user`.`verified` = ?)))',
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
            sql: 'select * from `folder` left join `user` on (`user`.`active` = ?) and (`user`.`role` = ?)',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'OR conditions with _condition flags',
          parameters: {
            joinConditions: {
              OR: [
                { active: true, _condition: true },
                { verified: true, _condition: false },
                { role: 'admin' }
              ]
            }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on (`user`.`active` = ?) or (`user`.`role` = ?)',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'AND conditions with null values',
          parameters: {
            joinConditions: {
              AND: [{ active: true }, null, { role: 'admin' }, undefined]
            }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on (`user`.`active` = ?) and (`user`.`role` = ?)',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'OR conditions with null values',
          parameters: {
            joinConditions: {
              OR: [{ active: true }, null, { role: 'admin' }, undefined]
            }
          },
          expected: {
            sql: 'select * from `folder` left join `user` on (`user`.`active` = ?) or (`user`.`role` = ?)',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'AND join conditions with only null values should not affect query',
          parameters: {
            joinConditions: {
              AND: [null, undefined, null]
            }
          },
          expected: {
            sql: 'select * from `folder` left join `user`',
            bindings: []
          }
        },
        {
          name: 'OR join conditions with only null values should not affect query',
          parameters: {
            joinConditions: {
              OR: [null, undefined, null]
            }
          },
          expected: {
            sql: 'select * from `folder` left join `user`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        query.leftJoin('user', function () {
          knexTools.applyJoinConditions(this, 'user', parameters.joinConditions)
        })
        expect(query.toSQL().sql).toBe(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('edge cases', () => {
      const testCases = [
        {
          name: 'returns query unchanged when conditions is null',
          parameters: {
            joinConditions: null
          },
          expected: {
            sql: 'select * from `folder` left join `user`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when conditions is undefined',
          parameters: {
            joinConditions: undefined
          },
          expected: {
            sql: 'select * from `folder` left join `user`',
            bindings: []
          }
        },
        {
          name: 'returns query unchanged when conditions is empty object',
          parameters: {
            joinConditions: {}
          },
          expected: {
            sql: 'select * from `folder` left join `user`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db('folder').select('*')
        query.leftJoin('user', function () {
          knexTools.applyJoinConditions(this, 'user', parameters.joinConditions)
        })
        expect(query.toSQL().sql).toBe(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })
  })

  describe('processJoins', () => {
    describe('belongsTo relationships', () => {
      const testCases = [
        {
          name: 'belongsTo join without conditions',
          parameters: {
            model: folderModel,
            join: {
              user: { projection: 'details' }
            }
          },
          expected: {
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('hasMany relationships', () => {
      const testCases = [
        {
          name: 'hasMany join without conditions',
          parameters: {
            model: folderModel,
            join: {
              memos: { projection: 'details' }
            }
          },
          expected: {
            sql: 'select *, `m`.`id` as `memos_id`, `m`.`user_id` as `memos_user_id`, `m`.`folder_id` as `memos_folder_id`, `m`.`content` as `memos_content`, `m`.`created_at` as `memos_created_at`, `m`.`updated_at` as `memos_updated_at` from `folder` as `f` left join `memo` as `m` on `m`.`folder_id` = `f`.`id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('manyToMany relationships', () => {
      const testCases = [
        {
          name: 'manyToMany join without conditions',
          parameters: {
            model: memoModel,
            join: {
              tags: { projection: 'details' }
            }
          },
          expected: {
            sql: 'select *, `t`.`id` as `tags_id`, `t`.`name` as `tags_name`, `t`.`created_at` as `tags_created_at`, `t`.`updated_at` as `tags_updated_at` from `memo` as `m` left join `memo_tag` as `mt` on `m`.`id` = `mt`.`memo_id` left join `tag` as `t` on `mt`.`tag_id` = `t`.`id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('self-referencing relationships', () => {
      const testCases = [
        {
          name: 'self-referencing hasMany join without conditions (children)',
          parameters: {
            model: folderModel,
            join: {
              children: { projection: 'details' }
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`name` as `children_name`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` as `f` left join `folder` as `children` on `children`.`parent_id` = `f`.`id`',
            bindings: []
          }
        },
        {
          name: 'self-referencing belongsTo join without conditions (parent)',
          parameters: {
            model: folderModel,
            join: {
              parent: { projection: 'details' }
            }
          },
          expected: {
            sql: 'select *, `parent`.`id` as `parent_id`, `parent`.`name` as `parent_name`, `parent`.`user_id` as `parent_user_id`, `parent`.`parent_id` as `parent_parent_id`, `parent`.`created_at` as `parent_created_at`, `parent`.`updated_at` as `parent_updated_at` from `folder` as `f` left join `folder` as `parent` on `parent`.`id` = `f`.`parent_id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('joins with conditions', () => {
      const testCases = [
        {
          name: 'belongsTo join with both on and where conditions',
          parameters: {
            model: folderModel,
            join: {
              user: {
                projection: 'details',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id` and `u`.`active` = ? where `u`.`role` = ?',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'hasMany join with complex on and where conditions',
          parameters: {
            model: folderModel,
            join: {
              children: {
                projection: 'details',
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
            sql: 'select *, `children`.`id` as `children_id`, `children`.`name` as `children_name`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` as `f` left join `folder` as `children` on `children`.`parent_id` = `f`.`id` and `children`.`user_id` > ? and `children`.`name` like ? where `children`.`created_at` >= ? and `children`.`updated_at` is not null',
            bindings: [0, '%test%', '2023-01-01']
          }
        },
        {
          name: 'manyToMany join with both on and where conditions',
          parameters: {
            model: memoModel,
            join: {
              tags: {
                projection: 'details',
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
            sql: 'select *, `t`.`id` as `tags_id`, `t`.`name` as `tags_name`, `t`.`created_at` as `tags_created_at`, `t`.`updated_at` as `tags_updated_at` from `memo` as `m` left join `memo_tag` as `mt` on `m`.`id` = `mt`.`memo_id` left join `tag` as `t` on `mt`.`tag_id` = `t`.`id` and `t`.`name` like ? where `t`.`created_at` is not null and `t`.`updated_at` >= ?',
            bindings: ['tag_%', '2023-06-01']
          }
        },
        {
          name: 'join with logical operators in both on and where',
          parameters: {
            model: folderModel,
            join: {
              user: {
                projection: 'details',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id` or (`u`.`active` = ?) or (`u`.`verified` = ?) where (`u`.`role` in (?, ?)) and (`u`.`premium` = ?)',
            bindings: [true, true, 'admin', 'user', true]
          }
        },
        {
          name: 'enforce join type with both on and where conditions',
          parameters: {
            model: folderModel,
            join: {
              user: {
                projection: 'details',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` inner join `user` as `u` on `u`.`id` = `f`.`user_id` and `u`.`active` = ? and `u`.`role` != ? where `u`.`verified` = ? and `u`.`premium` is not null',
            bindings: [true, 'guest', true]
          }
        },
        {
          name: 'join with conditional flags in both on and where',
          parameters: {
            model: folderModel,
            join: {
              user: {
                projection: 'details',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id` and `u`.`active` = ? where `u`.`verified` = ?',
            bindings: [true, true]
          }
        },
        {
          name: 'manyToMany enforce join with logical operators',
          parameters: {
            model: memoModel,
            join: {
              tags: {
                projection: 'details',
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
            sql: 'select *, `t`.`id` as `tags_id`, `t`.`name` as `tags_name`, `t`.`created_at` as `tags_created_at`, `t`.`updated_at` as `tags_updated_at` from `memo` as `m` inner join `memo_tag` as `mt` on `m`.`id` = `mt`.`memo_id` inner join `tag` as `t` on `mt`.`tag_id` = `t`.`id` or (`t`.`name` = ?) or (`t`.`name` = ?) where (`t`.`created_at` >= ?) and (`t`.`updated_at` is not null)',
            bindings: ['urgent', 'priority', '2023-01-01']
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('error handling', () => {
      const testCases = [
        {
          name: 'processJoins throws error when joins parameter is missing',
          parameters: {
            model: memoModel,
            join: null
          },
          expectedError: 'joins parameter is required in processJoins'
        },
        {
          name: 'processJoins throws error when joins parameter is undefined',
          parameters: {
            model: memoModel,
            join: undefined
          },
          expectedError: 'joins parameter is required in processJoins'
        },
        {
          name: 'processJoins throws error when relations parameter is missing',
          parameters: {
            model: { ...memoModel, relations: null },
            join: { user: { projection: 'details' } }
          },
          expectedError: 'relations parameter is required in processJoins'
        },
        {
          name: 'processJoins throws error when relations parameter is undefined',
          parameters: {
            model: { ...memoModel, relations: undefined },
            join: { user: { projection: 'details' } }
          },
          expectedError: 'relations parameter is required in processJoins'
        },
        {
          name: 'processJoins throws error when relation not found in relations config',
          parameters: {
            model: memoModel,
            join: {
              nonexistent: { projection: 'details' }
            }
          },
          expectedError: "Relation 'nonexistent' not found in relations config"
        },
        {
          name: 'processJoins throws error when projection not found in model',
          parameters: {
            model: memoModel,
            join: {
              user: { projection: 'nonexistent' }
            }
          },
          expectedError: "Projection 'nonexistent' not found in model 'user'"
        }
      ]

      test.each(testCases)('$name', ({ parameters, expectedError }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        expect(() => {
          knexTools.processJoins(
            query,
            parameters.model,
            parameters.join,
            parameters.model.relations
          )
        }).toThrow(expectedError)
      })
    })

    describe('nested joins', () => {
      const testCases = [
        {
          name: 'nested join: memo → folder → parent folder',
          parameters: {
            model: memoModel,
            join: {
              folder: {
                projection: 'short',
                join: {
                  parent: { projection: 'short' }
                }
              }
            }
          },
          expected: {
            sql: 'select *, `f`.`id` as `folder_id`, `f`.`name` as `folder_name`, `f`.`user_id` as `folder_user_id`, `f`.`parent_id` as `folder_parent_id`, `parent`.`id` as `parent_id`, `parent`.`name` as `parent_name`, `parent`.`user_id` as `parent_user_id`, `parent`.`parent_id` as `parent_parent_id` from `memo` as `m` left join `folder` as `f` on `f`.`id` = `m`.`folder_id` left join `folder` as `parent` on `parent`.`id` = `f`.`parent_id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('edge cases', () => {
      const testCases = [
        {
          name: 'join with true (no conditions)',
          parameters: {
            model: folderModel,
            join: {
              user: true // Simple join, no conditions
            }
          },
          expected: {
            sql: 'select * from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ parameters, expected }) => {
        const query = db(
          `${parameters.model.tableName} as ${parameters.model.alias}`
        ).select('*')
        knexTools.processJoins(
          query,
          parameters.model,
          parameters.join,
          parameters.model.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })
  })

  describe('buildQuery', () => {
    beforeEach(async () => {
      // Insert test data with intentional empty relations
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' },
        { id: 3, name: 'Charlie', email: 'charlie@example.com', role: 'user' } // Note: Charlie intentionally has no memos/folders
      ])

      await db('folder').insert([
        { id: 1, name: 'Work', user_id: 1 },
        { id: 2, name: 'Personal', user_id: 1 },
        { id: 3, name: 'Projects', user_id: 2 }
        // Note: Charlie (user #3) intentionally has no folders
      ])

      await db('memo').insert([
        { id: 1, content: 'Important meeting notes', user_id: 1, folder_id: 1 },
        { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 },
        { id: 3, content: 'Project ideas', user_id: 2, folder_id: 3 },
        { id: 4, content: 'Untagged memo', user_id: 2, folder_id: 3 }, // Note: memo #4 intentionally has no tags
        { id: 5, content: 'Orphaned memo', user_id: null, folder_id: 1 } // Note: memo #5 intentionally has null user_id
      ])

      await db('tag').insert([
        { id: 1, name: 'urgent' },
        { id: 2, name: 'personal' },
        { id: 3, name: 'work' }
      ])

      await db('memo_tag').insert([
        { memo_id: 1, tag_id: 1 },
        { memo_id: 1, tag_id: 3 },
        { memo_id: 2, tag_id: 2 }
        // Note: memo #4 and #5 intentionally get no junction records
      ])
    })

    describe('core functionality', () => {
      const testCases = [
        {
          name: 'basic query with projection',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'short',
              where: { user_id: 1 }
            }
          },
          expected: {
            data: [
              {
                id: 1,
                content: 'Important meeting notes',
                user_id: 1,
                folder_id: 1
              },
              { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 }
            ]
          }
        },
        {
          name: 'query with custom projection function',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'basic',
              where: { id: 1 }
            }
          },
          expected: {
            data: [{ id: 1, content: 'Important meeting notes' }]
          }
        },
        {
          name: 'query with sorting and paging',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'short',
              orderBy: { id: 'desc' },
              take: 2,
              skip: 0
            }
          },
          expected: {
            data: [
              { id: 5, content: 'Orphaned memo', user_id: null, folder_id: 1 },
              { id: 4, content: 'Untagged memo', user_id: 2, folder_id: 3 }
            ]
          }
        },
        {
          name: 'buildQuery with _exists filter for RLS',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'short',
              where: {
                _exists: {
                  user: {
                    role: 'admin'
                  }
                }
              }
            }
          },
          expected: {
            data: [
              {
                id: 1,
                content: 'Important meeting notes',
                user_id: 1,
                folder_id: 1
              },
              { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 }
            ]
          }
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expected }) => {
        const result = await knexTools.buildQuery(
          db,
          parameters.model,
          parameters.queryConfig
        )
        expect(result).toEqual(expected)
      })
    })

    describe('each relations', () => {
      describe('belongsTo relationships', () => {
        const testCases = [
          {
            name: 'query with belongsTo relation',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'short',
                where: { id: 1 },
                each: {
                  user: {
                    projection: 'short'
                  }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  content: 'Important meeting notes',
                  user_id: 1,
                  folder_id: 1,
                  user: {
                    data: {
                      id: 1,
                      name: 'Alice',
                      email: 'alice@example.com'
                    }
                  }
                }
              ]
            }
          },
          {
            name: 'belongsTo relation returns null when foreign key is null',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'short',
                where: { id: 5 }, // Target the intentionally orphaned memo
                each: {
                  user: { projection: 'short' }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 5,
                  content: 'Orphaned memo',
                  user_id: null,
                  folder_id: 1,
                  user: null
                }
              ]
            }
          }
        ]

        test.each(testCases)('$name', async ({ parameters, expected }) => {
          const result = await knexTools.buildQuery(
            db,
            parameters.model,
            parameters.queryConfig
          )
          expect(result).toEqual(expected)
        })

        test('belongsTo relation validation is skipped when no records returned', async () => {
          // This should not throw even though 'basic' projection doesn't include user_id
          // because there are no records to validate
          const result = await knexTools.buildQuery(db, memoModel, {
            projection: 'basic', // doesn't include user_id
            where: { id: 999 }, // non-existent record
            each: {
              user: { projection: 'short' }
            }
          })

          expect(result).toEqual({
            data: []
          })
        })
      })

      describe('hasMany relationships', () => {
        const testCases = [
          {
            name: 'query with hasMany relation',
            parameters: {
              model: userModel,
              queryConfig: {
                projection: 'short',
                where: { id: 1 },
                each: {
                  folders: {
                    projection: 'short'
                  }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  name: 'Alice',
                  email: 'alice@example.com',
                  folders: {
                    data: [
                      { id: 1, name: 'Work', user_id: 1, parent_id: null },
                      { id: 2, name: 'Personal', user_id: 1, parent_id: null }
                    ]
                  }
                }
              ]
            }
          },
          {
            name: 'hasMany relation returns empty data array when no records exist',
            parameters: {
              model: userModel,
              queryConfig: {
                projection: 'short',
                where: { id: 3 }, // Target Charlie who intentionally has no memos
                each: {
                  memos: { projection: 'short' }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 3,
                  name: 'Charlie',
                  email: 'charlie@example.com',
                  memos: {
                    data: []
                  }
                }
              ]
            }
          }
        ]

        test.each(testCases)('$name', async ({ parameters, expected }) => {
          const result = await knexTools.buildQuery(
            db,
            parameters.model,
            parameters.queryConfig
          )
          expect(result).toEqual(expected)
        })

        test('hasMany relation returns empty array when projection excludes primary key', async () => {
          // Create a temporary model with a projection that doesn't include the primary key
          const userModelWithoutPK = {
            ...userModel,
            projections: {
              ...userModel.projections,
              nameOnly: (_, alias) => [`${alias}.name`, `${alias}.email`]
            }
          }

          const result = await knexTools.buildQuery(db, userModelWithoutPK, {
            projection: 'nameOnly', // This projection doesn't include 'id'
            where: { id: 1 },
            each: {
              folders: { projection: 'short' }
            }
          })

          // Since primary key 'id' is not in projection, all records will have undefined id
          // After filtering with .filter(Boolean), primaryKeys array is empty
          // So folders should be populated with empty array
          expect(result).toEqual({
            data: [
              {
                name: 'Alice',
                email: 'alice@example.com',
                folders: { data: [] }
              }
            ]
          })
        })
      })

      describe('manyToMany relationships', () => {
        const testCases = [
          {
            name: 'query with manyToMany relation',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'short',
                where: { id: 1 },
                each: {
                  tags: {
                    projection: 'short'
                  }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  content: 'Important meeting notes',
                  user_id: 1,
                  folder_id: 1,
                  tags: {
                    data: [
                      { id: 1, name: 'urgent' },
                      { id: 3, name: 'work' }
                    ]
                  }
                }
              ]
            }
          },
          {
            name: 'manyToMany relation returns empty data array when no junction records exist',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'short',
                where: { id: 4 }, // Target the intentionally untagged memo
                each: {
                  tags: { projection: 'short' }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 4,
                  content: 'Untagged memo',
                  user_id: 2,
                  folder_id: 3,
                  tags: {
                    data: []
                  }
                }
              ]
            }
          }
        ]

        test.each(testCases)('$name', async ({ parameters, expected }) => {
          const result = await knexTools.buildQuery(
            db,
            parameters.model,
            parameters.queryConfig
          )
          expect(result).toEqual(expected)
        })

        test('manyToMany relation is not populated when projection excludes primary key', async () => {
          // Create a temporary model with a projection that doesn't include the primary key
          const memoModelWithoutPK = {
            ...memoModel,
            projections: {
              ...memoModel.projections,
              contentOnly: (_, alias) => [
                `${alias}.content`,
                `${alias}.user_id`
              ]
            }
          }

          const result = await knexTools.buildQuery(db, memoModelWithoutPK, {
            projection: 'contentOnly', // This projection doesn't include 'id'
            where: { id: 1 },
            each: {
              tags: { projection: 'short' }
            }
          })

          // Since primary key 'id' is not in projection, all records will have undefined id
          // After filtering with .filter(Boolean), primaryKeys array is empty
          // manyToMany early returns without populating, so 'tags' key won't exist
          expect(result).toEqual({
            data: [
              {
                content: 'Important meeting notes',
                user_id: 1
                // Note: No 'tags' property at all due to early return
              }
            ]
          })
        })
      })
    })

    describe('advanced features', () => {
      const testCases = [
        {
          name: 'query with nested each relations',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'short',
              where: { id: 1 },
              each: {
                user: {
                  projection: 'short'
                },
                folder: {
                  projection: 'short',
                  each: {
                    user: {
                      projection: 'short'
                    }
                  }
                },
                tags: {
                  projection: 'short'
                }
              }
            }
          },
          expected: {
            data: [
              {
                id: 1,
                content: 'Important meeting notes',
                user_id: 1,
                folder_id: 1,
                user: {
                  data: {
                    id: 1,
                    name: 'Alice',
                    email: 'alice@example.com'
                  }
                },
                folder: {
                  data: {
                    id: 1,
                    name: 'Work',
                    user_id: 1,
                    parent_id: null,
                    user: {
                      data: {
                        id: 1,
                        name: 'Alice',
                        email: 'alice@example.com'
                      }
                    }
                  }
                },
                tags: {
                  data: [
                    { id: 1, name: 'urgent' },
                    { id: 3, name: 'work' }
                  ]
                }
              }
            ]
          }
        },
        {
          name: 'query with filtered relations',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'short',
              where: { user_id: 1 },
              each: {
                tags: {
                  projection: 'short',
                  where: { name: 'urgent' }
                }
              }
            }
          },
          expected: {
            data: [
              {
                id: 1,
                content: 'Important meeting notes',
                user_id: 1,
                folder_id: 1,
                tags: {
                  data: [{ id: 1, name: 'urgent' }]
                }
              },
              {
                id: 2,
                content: 'Shopping list',
                user_id: 1,
                folder_id: 2,
                tags: {
                  data: []
                }
              }
            ]
          }
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expected }) => {
        const result = await knexTools.buildQuery(
          db,
          parameters.model,
          parameters.queryConfig
        )
        expect(result).toEqual(expected)
      })

      test('empty relations maintain consistent format with metadata', async () => {
        const result = await knexTools.buildQuery(db, userModel, {
          projection: 'short',
          where: { id: 3 }, // Target Charlie who intentionally has no memos
          each: {
            memos: {
              projection: 'short',
              metadata: {
                counts: {
                  total: true
                }
              }
            }
          }
        })

        expect(result).toEqual({
          data: [
            {
              id: 3,
              name: 'Charlie',
              email: 'charlie@example.com',
              memos: {
                data: [],
                metadata: {
                  counts: {
                    total: 5
                  }
                }
              }
            }
          ]
        })
      })
    })

    describe('error handling', () => {
      const testCases = [
        {
          name: 'buildQuery throws error for invalid projection',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'nonexistent'
            }
          },
          expectedError: "Projection 'nonexistent' not found in model"
        },
        {
          name: 'buildQuery throws error for invalid relation',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'details',
              each: {
                nonexistent: {
                  projection: 'short'
                }
              }
            }
          },
          expectedError: "Relation 'nonexistent' not found in model"
        },
        {
          name: 'buildQuery throws error for invalid modifier',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'details',
              modifiers: {
                nonexistent: {}
              }
            }
          },
          expectedError: "Modifier 'nonexistent' not found in model"
        },
        {
          name: 'buildQuery throws error for belongsTo relation without foreign key in projection',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'basic', // basic projection doesn't include user_id
              where: { id: 1 },
              each: {
                user: {
                  projection: 'short'
                }
              }
            }
          },
          expectedError:
            "Cannot populate 'user' relation: projection must include 'user_id' field"
        },
        {
          name: 'buildQuery throws error for belongsTo relation without foreign key in projection (folder relation)',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'basic', // basic projection doesn't include folder_id
              where: { id: 1 },
              each: {
                folder: {
                  projection: 'short'
                }
              }
            }
          },
          expectedError:
            "Cannot populate 'folder' relation: projection must include 'folder_id' field"
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expectedError }) => {
        await expect(
          knexTools.buildQuery(db, parameters.model, parameters.queryConfig)
        ).rejects.toThrow(expectedError)
      })
    })

    describe('modifiers', () => {
      const testCases = [
        {
          name: 'applies default modifier automatically',
          parameters: {
            model: longMemoModel,
            queryConfig: {
              projection: 'basic'
            }
          },
          expected: {
            data: [{ id: 1, content: 'Important meeting notes' }]
          }
        },
        {
          name: 'applies parameterized modifier with object parameters',
          parameters: {
            model: memoModel,
            queryConfig: {
              projection: 'short',
              modifiers: {
                forUser: { userId: 1 }
              }
            }
          },
          expected: {
            data: [
              {
                id: 1,
                content: 'Important meeting notes',
                user_id: 1,
                folder_id: 1
              },
              { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 }
            ]
          }
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expected }) => {
        const result = await knexTools.buildQuery(
          db,
          parameters.model,
          parameters.queryConfig
        )
        expect(result).toEqual(expected)
      })
    })

    describe('metadata counts', () => {
      describe('basic count metadata', () => {
        const testCases = [
          {
            name: 'basic total count',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'details',
                metadata: {
                  counts: {
                    total: true
                  }
                }
              }
            },
            expected: {
              metadata: {
                counts: {
                  total: 5
                }
              }
            }
          },
          {
            name: 'basic filtered count',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'details',
                where: { user_id: 1 },
                metadata: {
                  counts: {
                    filtered: true
                  }
                }
              }
            },
            expected: {
              metadata: {
                counts: {
                  filtered: 2
                }
              }
            }
          },
          {
            name: 'both total and filtered counts',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'details',
                where: { user_id: 1 },
                metadata: {
                  counts: {
                    total: true,
                    filtered: true
                  }
                }
              }
            },
            expected: {
              metadata: {
                counts: {
                  total: 5,
                  filtered: 2
                }
              }
            }
          }
        ]

        test.each(testCases)('$name', async ({ parameters, expected }) => {
          const result = await knexTools.buildQuery(
            db,
            parameters.model,
            parameters.queryConfig
          )

          // Check metadata structure matches expected
          expect(result).toMatchObject(expected)
        })
      })

      describe('relation metadata', () => {
        const testCases = [
          {
            name: 'metadata on belongsTo relation',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'details',
                where: { id: 1 },
                each: {
                  user: {
                    projection: 'short',
                    metadata: {
                      counts: {
                        total: true
                      }
                    }
                  }
                },
                metadata: {
                  counts: { total: true } // Add main metadata for consistent format
                }
              }
            },
            expected: {
              data: [
                {
                  user: {
                    metadata: {
                      counts: {
                        total: 3
                      }
                    }
                  }
                }
              ]
            }
          },
          {
            name: 'metadata on hasMany relation',
            parameters: {
              model: userModel,
              queryConfig: {
                projection: 'details',
                where: { id: 1 },
                each: {
                  folders: {
                    projection: 'short',
                    metadata: {
                      counts: {
                        filtered: true
                      }
                    }
                  }
                },
                metadata: {
                  counts: { total: true } // Add main metadata for consistent format
                }
              }
            },
            expected: {
              data: [
                {
                  folders: {
                    metadata: {
                      counts: {
                        filtered: 2
                      }
                    }
                  }
                }
              ]
            }
          },
          {
            name: 'metadata on manyToMany relation',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'details',
                where: { id: 1 },
                each: {
                  tags: {
                    projection: 'short',
                    metadata: {
                      counts: {
                        total: true,
                        filtered: true
                      }
                    }
                  }
                },
                metadata: {
                  counts: { total: true } // Add main metadata for consistent format
                }
              }
            },
            expected: {
              data: [
                {
                  tags: {
                    metadata: {
                      counts: {
                        total: 3,
                        filtered: 2
                      }
                    }
                  }
                }
              ]
            }
          }
        ]

        test.each(testCases)('$name', async ({ parameters, expected }) => {
          const result = await knexTools.buildQuery(
            db,
            parameters.model,
            parameters.queryConfig
          )

          // Check relation metadata structure matches expected
          expect(result).toMatchObject(expected)
        })
      })

      describe('withRelatedCounts', () => {
        const testCases = [
          {
            name: 'gets multiple hasMany and one manyToMany related counts',
            parameters: {
              model: userModel,
              queryConfig: {
                projection: 'short',
                where: { id: 1 },
                withRelatedCounts: {
                  memos: true,
                  folders: true
                },
                each: {
                  memos: {
                    projection: 'short',
                    withRelatedCounts: {
                      tags: true
                    }
                  }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  name: 'Alice',
                  email: 'alice@example.com',
                  _counts: { memos: 2, folders: 2 },
                  memos: {
                    data: [
                      {
                        id: 1,
                        content: 'Important meeting notes',
                        user_id: 1,
                        folder_id: 1,
                        _counts: { tags: 2 }
                      },
                      {
                        id: 2,
                        content: 'Shopping list',
                        user_id: 1,
                        folder_id: 2,
                        _counts: { tags: 1 }
                      }
                    ]
                  }
                }
              ]
            }
          },
          {
            name: 'applies filters to related counts',
            parameters: {
              model: userModel,
              queryConfig: {
                projection: 'short',
                where: { id: { in: [1, 2, 3] } },
                orderBy: { id: 'asc' },
                withRelatedCounts: {
                  memos: {
                    where: {
                      id: { gte: 2 }
                    }
                  }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  name: 'Alice',
                  email: 'alice@example.com',
                  _counts: { memos: 1 }
                },
                {
                  id: 2,
                  name: 'Bob',
                  email: 'bob@example.com',
                  _counts: { memos: 2 }
                },
                {
                  id: 3,
                  name: 'Charlie',
                  email: 'charlie@example.com',
                  _counts: { memos: 0 }
                }
              ]
            }
          },
          {
            name: 'counts manyToMany relations with where filter',
            parameters: {
              model: memoModel,
              queryConfig: {
                projection: 'short',
                where: { id: { in: [1, 2] } },
                orderBy: { id: 'asc' },
                withRelatedCounts: {
                  tags: {
                    where: {
                      name: 'urgent'
                    }
                  }
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  content: 'Important meeting notes',
                  user_id: 1,
                  folder_id: 1,
                  _counts: { tags: 1 }
                },
                {
                  id: 2,
                  content: 'Shopping list',
                  user_id: 1,
                  folder_id: 2,
                  _counts: { tags: 0 }
                }
              ]
            }
          },
          {
            name: 'skips relations with falsy config values',
            parameters: {
              model: userModel,
              queryConfig: {
                projection: 'short',
                where: { id: 1 },
                withRelatedCounts: {
                  memos: true,
                  folders: null
                }
              }
            },
            expected: {
              data: [
                {
                  id: 1,
                  name: 'Alice',
                  email: 'alice@example.com',
                  _counts: { memos: 2 }
                }
              ]
            }
          }
        ]

        test.each(testCases)('$name', async ({ parameters, expected }) => {
          const result = await knexTools.buildQuery(
            db,
            parameters.model,
            parameters.queryConfig
          )
          expect(result).toEqual(expected)
        })

        test('throws error for invalid relation in withRelatedCounts', async () => {
          await expect(
            knexTools.buildQuery(db, userModel, {
              projection: 'short',
              where: { id: 1 },
              withRelatedCounts: {
                nonexistent: true
              }
            })
          ).rejects.toThrow(
            "Relation 'nonexistent' not found in model for related counts."
          )
        })

        test('returns empty data when no records match query with withRelatedCounts', async () => {
          const result = await knexTools.buildQuery(db, userModel, {
            projection: 'short',
            where: { id: 999 }, // No user with ID 999 exists
            withRelatedCounts: {
              memos: true,
              folders: true
            }
          })

          expect(result).toEqual({
            data: []
          })
        })

        test('withRelatedCounts does not populate _counts when projection excludes primary key', async () => {
          // Create a temporary model with a projection that doesn't include the primary key
          const userModelWithoutPK = {
            ...userModel,
            projections: {
              ...userModel.projections,
              nameOnly: (_, alias) => [`${alias}.name`, `${alias}.email`]
            }
          }

          const result = await knexTools.buildQuery(db, userModelWithoutPK, {
            projection: 'nameOnly', // This projection doesn't include 'id'
            where: { id: 1 },
            withRelatedCounts: {
              memos: true,
              folders: true
            }
          })

          // Since primary key 'id' is not in projection, all records will have undefined id
          // After filtering with .filter(Boolean), recordIds array is empty
          // populateRelatedCounts early returns without adding _counts property
          expect(result).toEqual({
            data: [
              {
                name: 'Alice',
                email: 'alice@example.com'
                // Note: No _counts property at all due to early return
              }
            ]
          })
        })
      })
    })
  })

  describe('counts', () => {
    beforeEach(async () => {
      // Insert test data
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' },
        { id: 3, name: 'Charlie', email: 'charlie@example.com', role: 'user' }
      ])

      await db('folder').insert([
        { id: 1, name: 'Work', user_id: 1 },
        { id: 2, name: 'Personal', user_id: 1 },
        { id: 3, name: 'Projects', user_id: 2 }
      ])

      await db('memo').insert([
        { id: 1, content: 'Important meeting notes', user_id: 1, folder_id: 1 },
        { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 },
        { id: 3, content: 'Project ideas', user_id: 2, folder_id: 3 },
        { id: 4, content: 'Untagged memo', user_id: 2, folder_id: 3 },
        { id: 5, content: 'Orphaned memo', user_id: null, folder_id: 1 }
      ])
    })

    describe('basic count functionality', () => {
      const testCases = [
        {
          name: 'returns total count only',
          parameters: {
            model: memoModel,
            queryConfig: {
              counts: {
                total: true
              }
            }
          },
          expected: {
            total: 5
          }
        },
        {
          name: 'returns filtered count only',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: { user_id: 1 },
              counts: {
                filtered: true
              }
            }
          },
          expected: {
            filtered: 2
          }
        },
        {
          name: 'returns both total and filtered counts',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: { user_id: 1 },
              counts: {
                total: true,
                filtered: true
              }
            }
          },
          expected: {
            total: 5,
            filtered: 2
          }
        },
        {
          name: 'returns total count for different model',
          parameters: {
            model: userModel,
            queryConfig: {
              counts: {
                total: true
              }
            }
          },
          expected: {
            total: 3
          }
        },
        {
          name: 'returns filtered count with complex where clause',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: {
                OR: [{ user_id: 1 }, { user_id: 2 }]
              },
              counts: {
                filtered: true
              }
            }
          },
          expected: {
            filtered: 4
          }
        },
        {
          name: 'returns filtered count with null values',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: { user_id: null },
              counts: {
                filtered: true
              }
            }
          },
          expected: {
            filtered: 1
          }
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expected }) => {
        const result = await knexTools.counts(
          db,
          parameters.model,
          parameters.queryConfig
        )
        expect(result).toEqual(expected)
      })
    })

    describe('modifier-based counts', () => {
      const testCases = [
        {
          name: 'returns count using single modifier',
          parameters: {
            model: memoModel,
            queryConfig: {
              counts: {
                modifiers: {
                  forUser: { userId: 1 }
                }
              }
            }
          },
          expected: {
            forUser: 2
          }
        },
        {
          name: 'returns counts using modifier from long memo model',
          parameters: {
            model: longMemoModel,
            queryConfig: {
              counts: {
                modifiers: {
                  forUser: { userId: 1 }
                }
              }
            }
          },
          expected: {
            forUser: 2
          }
        },
        {
          name: 'combines total, filtered, and modifier counts',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: { user_id: 1 },
              counts: {
                total: true,
                filtered: true,
                modifiers: {
                  forUser: { userId: 2 }
                }
              }
            }
          },
          expected: {
            total: 5,
            filtered: 2,
            forUser: 2
          }
        },
        {
          name: 'returns total count without applying default modifier',
          parameters: {
            model: longMemoModel,
            queryConfig: {
              counts: {
                total: true
              }
            }
          },
          expected: {
            total: 5
          }
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expected }) => {
        const result = await knexTools.counts(
          db,
          parameters.model,
          parameters.queryConfig
        )
        expect(result).toEqual(expected)
      })
    })

    describe('error handling', () => {
      const testCases = [
        {
          name: 'throws error when counts config is missing',
          parameters: {
            model: memoModel,
            queryConfig: {}
          },
          expectedError:
            "'counts' configuration is required in counts. e.g., { counts: { total: true } }"
        },
        {
          name: 'throws error when counts config is undefined',
          parameters: {
            model: memoModel,
            queryConfig: {
              counts: undefined
            }
          },
          expectedError:
            "'counts' configuration is required in counts. e.g., { counts: { total: true } }"
        },
        {
          name: 'throws error for invalid modifier',
          parameters: {
            model: memoModel,
            queryConfig: {
              counts: {
                modifiers: {
                  nonexistent: {}
                }
              }
            }
          },
          expectedError: "Modifier 'nonexistent' not found in model"
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expectedError }) => {
        await expect(
          knexTools.counts(db, parameters.model, parameters.queryConfig)
        ).rejects.toThrow(expectedError)
      })
    })

    describe('edge cases', () => {
      const testCases = [
        {
          name: 'returns zero for filtered count when no records match',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: { user_id: 999 },
              counts: {
                filtered: true
              }
            }
          },
          expected: {
            filtered: 0
          }
        },
        {
          name: 'returns correct counts when where has _exists filter',
          parameters: {
            model: memoModel,
            queryConfig: {
              where: {
                _exists: {
                  user: { role: 'admin' }
                }
              },
              counts: {
                total: true,
                filtered: true
              }
            }
          },
          expected: {
            total: 5,
            filtered: 2
          }
        },
        {
          name: 'handles empty counts object',
          parameters: {
            model: memoModel,
            queryConfig: {
              counts: {}
            }
          },
          expected: {}
        },
        {
          name: 'handles filtered count without where clause',
          parameters: {
            model: memoModel,
            queryConfig: {
              counts: {
                filtered: true
              }
            }
          },
          expected: {}
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expected }) => {
        const result = await knexTools.counts(
          db,
          parameters.model,
          parameters.queryConfig
        )
        expect(result).toEqual(expected)
      })
    })
  })

  describe('exists', () => {
    beforeEach(async () => {
      // Insert test data
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' }
      ])

      await db('memo').insert([
        { id: 1, content: 'Important meeting notes', user_id: 1, folder_id: 1 },
        { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 }
      ])
    })

    const testCases = [
      {
        name: 'returns true when records exist',
        parameters: {
          model: userModel,
          queryConfig: {}
        },
        expected: true
      },
      {
        name: 'returns false when no records exist',
        parameters: {
          model: folderModel,
          queryConfig: {}
        },
        expected: false
      },
      {
        name: 'returns true with where clause when match exists',
        parameters: {
          model: userModel,
          queryConfig: {
            where: { role: 'admin' }
          }
        },
        expected: true
      },
      {
        name: 'returns false with where clause when no match exists',
        parameters: {
          model: userModel,
          queryConfig: {
            where: { role: 'superadmin' }
          }
        },
        expected: false
      },
      {
        name: 'returns true with modifier when match exists',
        parameters: {
          model: memoModel,
          queryConfig: {
            modifiers: {
              forUser: { userId: 1 }
            }
          }
        },
        expected: true
      },
      {
        name: 'returns false with modifier when no match exists',
        parameters: {
          model: memoModel,
          queryConfig: {
            modifiers: {
              forUser: { userId: 999 }
            }
          }
        },
        expected: false
      },
      {
        name: 'returns true with both where and modifier when match exists',
        parameters: {
          model: memoModel,
          queryConfig: {
            where: { id: 1 },
            modifiers: {
              forUser: { userId: 1 }
            }
          }
        },
        expected: true
      },
      {
        name: 'returns false with both where and modifier when no match exists',
        parameters: {
          model: memoModel,
          queryConfig: {
            where: { id: 999 },
            modifiers: {
              forUser: { userId: 1 }
            }
          }
        },
        expected: false
      },
      {
        name: 'applies default modifier when present in model',
        parameters: {
          model: longMemoModel,
          queryConfig: {}
        },
        expected: true
      }
    ]

    test.each(testCases)('$name', async ({ parameters, expected }) => {
      const result = await knexTools.exists(
        db,
        parameters.model,
        parameters.queryConfig
      )
      expect(result).toBe(expected)
    })

    describe('error handling', () => {
      const testCases = [
        {
          name: 'throws error for invalid modifier',
          parameters: {
            model: memoModel,
            queryConfig: {
              modifiers: {
                nonexistent: {}
              }
            }
          },
          expectedError: "Modifier 'nonexistent' not found in model"
        }
      ]

      test.each(testCases)('$name', async ({ parameters, expectedError }) => {
        await expect(
          knexTools.exists(db, parameters.model, parameters.queryConfig)
        ).rejects.toThrow(expectedError)
      })
    })
  })
})
