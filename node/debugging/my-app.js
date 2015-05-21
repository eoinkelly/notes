var debug = require('debug')('basic');
var libdebug = require('debug')('somelib');

// $ DEBUG=basic,somelib node my-app.js # debug output for basic,somelib
// $ node my-app.js # no debug output

console.log('some stuff', Date.now());
console.error('some stuff', Date.now());
debug('some stuff', Date.now());
libdebug('some stuff', Date.now());
