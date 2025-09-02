const { createInMemoryEmptyDatabase } = require('./setup')
const knexTools = require('../src/knex-tools')

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

    describe('_exists operator for RLS', () => {
      const userModel = require('./models/user.model')
      const memoModel = require('./models/memo.model')
      const folderModel = require('./models/folder.model')

      describe('belongsTo relationships', () => {
        const testCases = [
          {
            name: 'belongsTo with simple condition',
            model: memoModel,
            parameters: {
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
            model: memoModel,
            parameters: {
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

        test.each(testCases)('$name', ({ model, parameters, expected }) => {
          const query = db(`${model.tableName} as ${model.alias}`).select('*')
          knexTools.applyWhereClauses(
            query,
            model.alias,
            parameters,
            model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('hasMany relationships', () => {
        const testCases = [
          {
            name: 'hasMany with simple condition',
            model: userModel,
            parameters: {
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

        test.each(testCases)('$name', ({ model, parameters, expected }) => {
          const query = db(`${model.tableName} as ${model.alias}`).select('*')
          knexTools.applyWhereClauses(
            query,
            model.alias,
            parameters,
            model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('manyToMany relationships', () => {
        const testCases = [
          {
            name: 'manyToMany with junction table',
            model: memoModel,
            parameters: {
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

        test.each(testCases)('$name', ({ model, parameters, expected }) => {
          const query = db(`${model.tableName} as ${model.alias}`).select('*')
          knexTools.applyWhereClauses(
            query,
            model.alias,
            parameters,
            model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('self-referencing relationships', () => {
        const testCases = [
          {
            name: 'self-referencing hasMany with aliases',
            model: folderModel,
            parameters: {
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

        test.each(testCases)('$name', ({ model, parameters, expected }) => {
          const query = db(`${model.tableName} as ${model.alias}`).select('*')
          knexTools.applyWhereClauses(
            query,
            model.alias,
            parameters,
            model.relations
          )

          expect(query.toSQL().sql).toMatch(expected.sql)
          expect(query.toSQL().bindings).toEqual(expected.bindings)
        })
      })

      describe('error handling', () => {
        const testCases = [
          {
            name: '_exists throws error when relations not provided',
            setup: () => db('memo').select('*'),
            parameters: {
              where: {
                _exists: {
                  user: { id: 1 }
                }
              }
            },
            expectError:
              'Relations must be provided to use _exists functionality'
          },
          {
            name: '_exists throws error when relation not found',
            setup: () => db('memo').select('*'),
            parameters: {
              where: {
                _exists: {
                  nonexistent: { id: 1 }
                }
              }
            },
            relations: memoModel.relations,
            expectError: "Relation 'nonexistent' not found in relations config"
          }
        ]

        test.each(testCases)(
          '$name',
          ({ setup, parameters, relations, expectError }) => {
            const query = setup()
            expect(() => {
              knexTools.applyWhereClauses(query, 'memo', parameters, relations)
            }).toThrow(expectError)
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
    describe('belongsTo relationships', () => {
      const testCases = [
        {
          name: 'belongsTo join without conditions',
          model: 'folder',
          parameters: {
            join: {
              user: {}
            }
          },
          expected: {
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ model, parameters, expected }) => {
        const modelDef = require(`./models/${model}.model`)
        const query = db(`${modelDef.tableName} as ${modelDef.alias}`).select(
          '*'
        )
        knexTools.processJoins(
          query,
          modelDef,
          parameters.join,
          modelDef.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('hasMany relationships', () => {
      const testCases = [
        {
          name: 'hasMany join without conditions',
          model: 'folder',
          parameters: {
            join: {
              memos: {}
            }
          },
          expected: {
            sql: 'select *, `m`.`id` as `memos_id`, `m`.`user_id` as `memos_user_id`, `m`.`folder_id` as `memos_folder_id`, `m`.`content` as `memos_content`, `m`.`created_at` as `memos_created_at`, `m`.`updated_at` as `memos_updated_at` from `folder` as `f` left join `memo` as `m` on `m`.`folder_id` = `f`.`id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ model, parameters, expected }) => {
        const modelDef = require(`./models/${model}.model`)
        const query = db(`${modelDef.tableName} as ${modelDef.alias}`).select(
          '*'
        )
        knexTools.processJoins(
          query,
          modelDef,
          parameters.join,
          modelDef.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('manyToMany relationships', () => {
      const testCases = [
        {
          name: 'manyToMany join without conditions',
          model: 'memo',
          parameters: {
            join: {
              tags: {}
            }
          },
          expected: {
            sql: 'select *, `t`.`id` as `tags_id`, `t`.`name` as `tags_name`, `t`.`created_at` as `tags_created_at`, `t`.`updated_at` as `tags_updated_at` from `memo` as `m` left join `memo_tag` on `m`.`id` = `memo_tag`.`memo_id` left join `tag` as `t` on `memo_tag`.`tag_id` = `t`.`id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ model, parameters, expected }) => {
        const modelDef = require(`./models/${model}.model`)
        const query = db(`${modelDef.tableName} as ${modelDef.alias}`).select(
          '*'
        )
        knexTools.processJoins(
          query,
          modelDef,
          parameters.join,
          modelDef.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('self-referencing relationships', () => {
      const testCases = [
        {
          name: 'self-referencing hasMany join without conditions (children)',
          model: 'folder',
          parameters: {
            join: {
              children: {}
            }
          },
          expected: {
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` as `f` left join `folder` as `children` on `children`.`parent_id` = `f`.`id`',
            bindings: []
          }
        },
        {
          name: 'self-referencing belongsTo join without conditions (parent)',
          model: 'folder',
          parameters: {
            join: {
              parent: {}
            }
          },
          expected: {
            sql: 'select *, `parent`.`id` as `parent_id`, `parent`.`user_id` as `parent_user_id`, `parent`.`parent_id` as `parent_parent_id`, `parent`.`name` as `parent_name`, `parent`.`created_at` as `parent_created_at`, `parent`.`updated_at` as `parent_updated_at` from `folder` as `f` left join `folder` as `parent` on `parent`.`id` = `f`.`parent_id`',
            bindings: []
          }
        }
      ]

      test.each(testCases)('$name', ({ model, parameters, expected }) => {
        const modelDef = require(`./models/${model}.model`)
        const query = db(`${modelDef.tableName} as ${modelDef.alias}`).select(
          '*'
        )
        knexTools.processJoins(
          query,
          modelDef,
          parameters.join,
          modelDef.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })

    describe('joins with conditions', () => {
      const testCases = [
        {
          name: 'belongsTo join with both on and where conditions',
          model: 'folder',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id` and `u`.`active` = ? where `u`.`role` = ?',
            bindings: [true, 'admin']
          }
        },
        {
          name: 'hasMany join with complex on and where conditions',
          model: 'folder',
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
            sql: 'select *, `children`.`id` as `children_id`, `children`.`user_id` as `children_user_id`, `children`.`parent_id` as `children_parent_id`, `children`.`name` as `children_name`, `children`.`created_at` as `children_created_at`, `children`.`updated_at` as `children_updated_at` from `folder` as `f` left join `folder` as `children` on `children`.`parent_id` = `f`.`id` and `children`.`user_id` > ? and `children`.`name` like ? where `children`.`created_at` >= ? and `children`.`updated_at` is not null',
            bindings: [0, '%test%', '2023-01-01']
          }
        },
        {
          name: 'manyToMany join with both on and where conditions',
          model: 'memo',
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
            sql: 'select *, `t`.`id` as `tags_id`, `t`.`name` as `tags_name`, `t`.`created_at` as `tags_created_at`, `t`.`updated_at` as `tags_updated_at` from `memo` as `m` left join `memo_tag` on `m`.`id` = `memo_tag`.`memo_id` left join `tag` as `t` on `memo_tag`.`tag_id` = `t`.`id` and `t`.`name` like ? where `t`.`created_at` is not null and `t`.`updated_at` >= ?',
            bindings: ['tag_%', '2023-06-01']
          }
        },
        {
          name: 'join with logical operators in both on and where',
          model: 'folder',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id` or (`u`.`active` = ?) or (`u`.`verified` = ?) where (`u`.`role` in (?, ?)) and (`u`.`premium` = ?)',
            bindings: [true, true, 'admin', 'user', true]
          }
        },
        {
          name: 'enforce join type with both on and where conditions',
          model: 'folder',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` inner join `user` as `u` on `u`.`id` = `f`.`user_id` and `u`.`active` = ? and `u`.`role` != ? where `u`.`verified` = ? and `u`.`premium` is not null',
            bindings: [true, 'guest', true]
          }
        },
        {
          name: 'join with conditional flags in both on and where',
          model: 'folder',
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
            sql: 'select *, `u`.`id` as `user_id`, `u`.`name` as `user_name`, `u`.`email` as `user_email`, `u`.`role` as `user_role`, `u`.`active` as `user_active`, `u`.`premium` as `user_premium`, `u`.`verified` as `user_verified`, `u`.`created_at` as `user_created_at`, `u`.`updated_at` as `user_updated_at` from `folder` as `f` left join `user` as `u` on `u`.`id` = `f`.`user_id` and `u`.`active` = ? where `u`.`verified` = ?',
            bindings: [true, true]
          }
        },
        {
          name: 'manyToMany enforce join with logical operators',
          model: 'memo',
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
            sql: 'select *, `t`.`id` as `tags_id`, `t`.`name` as `tags_name`, `t`.`created_at` as `tags_created_at`, `t`.`updated_at` as `tags_updated_at` from `memo` as `m` inner join `memo_tag` on `m`.`id` = `memo_tag`.`memo_id` inner join `tag` as `t` on `memo_tag`.`tag_id` = `t`.`id` or (`t`.`name` = ?) or (`t`.`name` = ?) where (`t`.`created_at` >= ?) and (`t`.`updated_at` is not null)',
            bindings: ['urgent', 'priority', '2023-01-01']
          }
        }
      ]

      test.each(testCases)('$name', ({ model, parameters, expected }) => {
        const modelDef = require(`./models/${model}.model`)
        const query = db(`${modelDef.tableName} as ${modelDef.alias}`).select(
          '*'
        )
        knexTools.processJoins(
          query,
          modelDef,
          parameters.join,
          modelDef.relations
        )
        expect(query.toSQL().sql).toMatch(expected.sql)
        expect(query.toSQL().bindings).toEqual(expected.bindings)
      })
    })
  })
  describe('buildQuery', () => {
    const memoModel = require('./models/memo.model')
    const userModel = require('./models/user.model')

    beforeEach(async () => {
      // Insert test data
      await db('user').insert([
        { id: 1, name: 'Alice', email: 'alice@example.com', role: 'admin' },
        { id: 2, name: 'Bob', email: 'bob@example.com', role: 'user' }
      ])

      await db('folder').insert([
        { id: 1, name: 'Work', user_id: 1 },
        { id: 2, name: 'Personal', user_id: 1 },
        { id: 3, name: 'Projects', user_id: 2 }
      ])

      await db('memo').insert([
        { id: 1, content: 'Important meeting notes', user_id: 1, folder_id: 1 },
        { id: 2, content: 'Shopping list', user_id: 1, folder_id: 2 },
        { id: 3, content: 'Project ideas', user_id: 2, folder_id: 3 }
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
      ])
    })

    describe('core functionality', () => {
      const testCases = [
        {
          name: 'basic query with projection',
          model: memoModel,
          queryConfig: {
            projection: 'short',
            where: { user_id: 1 }
          },
          expected: {
            length: 2,
            properties: ['id', 'content'],
            contentMatch: /Important meeting notes/
          }
        },
        {
          name: 'query with custom projection function',
          model: memoModel,
          queryConfig: {
            projection: 'basic',
            where: { id: 1 }
          },
          expected: {
            length: 1,
            properties: ['id', 'content'],
            missingProperties: ['user_id', 'folder_id']
          }
        },
        {
          name: 'query with sorting and paging',
          model: memoModel,
          queryConfig: {
            projection: 'details',
            orderBy: { id: 'desc' },
            take: 2,
            skip: 0
          },
          expected: {
            length: 2,
            firstId: 3,
            secondId: 2
          }
        },
        {
          name: 'buildQuery with _exists filter for RLS',
          model: memoModel,
          queryConfig: {
            projection: 'details',
            where: {
              _exists: {
                user: {
                  role: 'admin'
                }
              }
            }
          },
          expected: {
            length: 2,
            userIds: [1, 1]
          }
        }
      ]

      test.each(testCases)(
        '$name',
        async ({ model, queryConfig, expected }) => {
          const result = await knexTools.buildQuery(db, model, queryConfig)

          expect(result).toHaveLength(expected.length)

          if (expected.properties) {
            expected.properties.forEach(prop => {
              expect(result[0]).toHaveProperty(prop)
            })
          }

          if (expected.missingProperties) {
            expected.missingProperties.forEach(prop => {
              expect(result[0]).not.toHaveProperty(prop)
            })
          }

          if (expected.contentMatch) {
            expect(result[0].content).toMatch(expected.contentMatch)
          }

          if (expected.firstId) {
            expect(result[0].id).toBe(expected.firstId)
          }

          if (expected.secondId) {
            expect(result[1].id).toBe(expected.secondId)
          }

          if (expected.userIds) {
            expect(result.map(r => r.user_id)).toEqual(expected.userIds)
          }
        }
      )
    })

    describe('each relations', () => {
      describe('belongsTo relationships', () => {
        const testCases = [
          {
            name: 'query with belongsTo relation',
            model: memoModel,
            queryConfig: {
              projection: 'details',
              where: { id: 1 },
              each: {
                user: {
                  projection: 'short'
                }
              }
            },
            expected: {
              length: 1,
              userProperties: {
                id: 1,
                name: 'Alice',
                email: 'alice@example.com'
              }
            }
          }
        ]

        test.each(testCases)(
          '$name',
          async ({ model, queryConfig, expected }) => {
            const result = await knexTools.buildQuery(db, model, queryConfig)

            expect(result).toHaveLength(expected.length)

            if (expected.userProperties) {
              expect(result[0].user).toEqual(expected.userProperties)
            }
          }
        )
      })

      describe('hasMany relationships', () => {
        const testCases = [
          {
            name: 'query with hasMany relation',
            model: userModel,
            queryConfig: {
              projection: 'details',
              where: { id: 1 },
              each: {
                folders: {
                  projection: 'short'
                }
              }
            },
            expected: {
              length: 1,
              foldersLength: 2,
              folderProperties: ['id', 'name']
            }
          }
        ]

        test.each(testCases)(
          '$name',
          async ({ model, queryConfig, expected }) => {
            const result = await knexTools.buildQuery(db, model, queryConfig)

            expect(result).toHaveLength(expected.length)
            expect(result[0].folders).toBeDefined()
            expect(result[0].folders).toHaveLength(expected.foldersLength)

            if (expected.folderProperties) {
              expected.folderProperties.forEach(prop => {
                expect(result[0].folders[0]).toHaveProperty(prop)
              })
            }
          }
        )
      })

      describe('manyToMany relationships', () => {
        const testCases = [
          {
            name: 'query with manyToMany relation',
            model: memoModel,
            queryConfig: {
              projection: 'details',
              where: { id: 1 },
              each: {
                tags: {
                  projection: 'short'
                }
              }
            },
            expected: {
              length: 1,
              tagsLength: 2,
              tagNames: ['urgent', 'work']
            }
          }
        ]

        test.each(testCases)(
          '$name',
          async ({ model, queryConfig, expected }) => {
            const result = await knexTools.buildQuery(db, model, queryConfig)

            expect(result).toHaveLength(expected.length)
            expect(result[0].tags).toHaveLength(expected.tagsLength)

            if (expected.tagNames) {
              const resultTagNames = result[0].tags.map(t => t.name)
              expected.tagNames.forEach(tagName => {
                expect(resultTagNames).toContain(tagName)
              })
            }
          }
        )
      })
    })

    describe('advanced features', () => {
      const testCases = [
        {
          name: 'query with nested each relations',
          model: memoModel,
          queryConfig: {
            projection: 'details',
            where: { user_id: 1 },
            each: {
              user: {
                projection: 'short'
              },
              folder: {
                projection: 'details',
                each: {
                  user: {
                    projection: 'short'
                  }
                }
              },
              tags: {
                projection: 'short',
                orderBy: { name: 'asc' }
              }
            }
          },
          expected: {
            length: 2,
            userName: 'Alice',
            folderName: 'Work',
            nestedUserName: 'Alice',
            firstTagName: 'urgent'
          }
        },
        {
          name: 'query with filtered relations',
          model: memoModel,
          queryConfig: {
            projection: 'details',
            where: { user_id: 1 },
            each: {
              tags: {
                projection: 'short',
                where: { name: 'urgent' }
              }
            }
          },
          expected: {
            length: 2,
            firstMemoTagsLength: 1,
            firstMemoTagName: 'urgent',
            secondMemoTagsLength: 0
          }
        }
      ]

      test.each(testCases)(
        '$name',
        async ({ model, queryConfig, expected }) => {
          const result = await knexTools.buildQuery(db, model, queryConfig)

          expect(result).toHaveLength(expected.length)

          if (expected.userName) {
            expect(result[0].user.name).toBe(expected.userName)
          }

          if (expected.folderName) {
            expect(result[0].folder.name).toBe(expected.folderName)
          }

          if (expected.nestedUserName) {
            expect(result[0].folder).toHaveProperty('user')
            if (result[0].folder.user) {
              expect(result[0].folder.user.name).toBe(expected.nestedUserName)
            }
          }

          if (expected.firstTagName) {
            expect(result[0].tags[0].name).toBe(expected.firstTagName)
          }

          if (expected.firstMemoTagsLength !== undefined) {
            expect(result[0].tags).toHaveLength(expected.firstMemoTagsLength)
          }

          if (expected.firstMemoTagName) {
            expect(result[0].tags[0].name).toBe(expected.firstMemoTagName)
          }

          if (expected.secondMemoTagsLength !== undefined) {
            expect(result[1].tags).toHaveLength(expected.secondMemoTagsLength)
          }
        }
      )
    })

    describe('error handling', () => {
      const testCases = [
        {
          name: 'error handling for invalid projection',
          model: memoModel,
          queryConfig: {
            projection: 'nonexistent'
          },
          expectedError: "Projection 'nonexistent' not found in model"
        },
        {
          name: 'error handling for invalid relation',
          model: memoModel,
          queryConfig: {
            projection: 'details',
            each: {
              nonexistent: {
                projection: 'short'
              }
            }
          },
          expectedError: "Relation 'nonexistent' not found in model"
        }
      ]

      test.each(testCases)(
        '$name',
        async ({ model, queryConfig, expectedError }) => {
          await expect(
            knexTools.buildQuery(db, model, queryConfig)
          ).rejects.toThrow(expectedError)
        }
      )
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
