async function executeUnitOfWork(knexInstance, callback) {
  const trx = await knexInstance.transaction()
  try {
    const result = await callback(trx)
    await trx.commit()
    return result
  } catch (error) {
    await trx.rollback()
    throw error
  }
}

function buildMakeTransaction(knexInstance) {
  return async function (callback) {
    return executeUnitOfWork(knexInstance, callback)
  }
}

module.exports = {
  executeUnitOfWork,
  buildMakeTransaction
}
