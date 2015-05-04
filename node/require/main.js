
console.log("requiring for first time:");
console.log(__filename);
console.log(__dirname);
var ret_a = require('./a');
var ret_b = require('./b');

console.log("requiring for second time:");
var ret_a2 = require('./a');
var ret_b2 = require('./b');

// you can also load json files as modules
var stuff = require('./stuff');
console.log('stuff', stuff);
