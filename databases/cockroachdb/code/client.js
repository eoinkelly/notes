const log = (...args) => {
  console.log((new Date()).toISOString(), ...args);
};

log('Start script eval');

import pg from 'pg';
const { Client, Pool } = pg;

const timedQuery = async (label, client, sql, params) => {
  console.time(label);
  const res = await client.query(sql, params)
  console.timeEnd(label, label);
  return res;
};

const client = new Client({
  connectionString: process.env.DATABASE_URL
})

const MAX_ITERATIONS = 1;

try {
  console.time('db-client-connect');
  client.connect();
  console.timeEnd('db-client-connect');

  try {
    for (let n = 0; n < MAX_ITERATIONS; n++) {
      log('Iteration', n);

      const _res = await timedQuery('select-now-query', client, 'SELECT now()', [])
      // log(res.rows)

      const sql = `INSERT INTO things (id, name) VALUES (123, 'Name ${Date.now()}')`;
      const _res2 = await timedQuery('insert-query', client, sql, [])
      // log(res2.rows)

      const res3 = await timedQuery('select-all-things-query', client, 'SELECT * FROM things', [])
      log(res3.rows.length, 'rows found')
    }
  } finally {
    console.time('db-client-release');
    await client.end();
    console.timeEnd('db-client-release');
  }
} catch (err) {
  log(err.stack)
}

log('End script eval')
