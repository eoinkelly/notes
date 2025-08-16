require('http');
function foo(date) {
  debugger;
  console.log('hi there', date);
}

console.log('starting');
setInterval(function () {
  foo(Date.now());
}, 1000);
console.log('ending');
