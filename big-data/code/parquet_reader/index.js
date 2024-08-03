import parquet from "@dsnp/parquetjs"

// Docs
// https://www.npmjs.com/package/@dsnp/parquetjs

// Data format
// https://github.com/apache/parquet-format/
// https://github.com/apache/parquet-format/blob/master/BloomFilter.md
// Write data
// var schema = new parquet.ParquetSchema({
//   name: { type: 'UTF8' },
//   quantity: { type: 'INT64' },
//   price: { type: 'DOUBLE' },
//   date: { type: 'TIMESTAMP_MILLIS' },
//   in_stock: { type: 'BOOLEAN' }
// });
//
// var writer = await parquet.ParquetWriter.openFile(schema, 'fruits.parquet');
// await writer.appendRow({name: 'apples', quantity: 10, price: 2.5, date: new Date(), in_stock: true});
// await writer.appendRow({name: 'oranges', quantity: 10, price: 2.5, date: new Date(), in_stock: true});
// await writer.close();

// Read data
let reader = await parquet.ParquetReader.openFile('fruits.parquet');
let cursor = reader.getCursor();
let record = null;

while (record = await cursor.next()) {
  console.log(record);
}

await reader.close();