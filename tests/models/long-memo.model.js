// Example of horizontal table partitioning - long content memos only
const memoModel = require('./memo.model')

module.exports = {
  ...memoModel,
  alias: 'lm', // Different alias to distinguish from regular memo model
  modifiers: {
    // Reserved key - automatically applied by buildQuery
    default: (query, knexInstance, tableAlias) => {
      // Filter for memos with long content (> 15 characters)
      query.whereRaw(`LENGTH(${tableAlias}.content) > ?`, [15])
    },

    // Manual modifiers for future use
    forUser: (query, knexInstance, tableAlias, { userId }) => {
      query.where(`${tableAlias}.user_id`, userId)
    }
  }
}
