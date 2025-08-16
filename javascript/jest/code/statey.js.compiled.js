'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});
exports.inc = inc;
exports.dec = dec;
exports.counter = void 0;
let counter = 0;
exports.counter = counter;

function inc() {
  exports.counter = counter = counter + 1;
}

function dec() {
  exports.counter = counter = counter - 1;
}
