const fs = require('fs');
// const sys = require('sys');
const util = require('util');

var filename = process.argv[2];

if (! filename) {
  throw Error('You forgot filename');
}

var dump = function (thing) {
  console.log(util.inspect(thing, false, 7, true));
};

fs.watch(filename, function () {
  // dump(arguments);
  dump(filename + ' changed');
});

// QUESTION: what is diff between sys.puts and console.log ???

// http://www.joyent.com/developers/node/debug
/*

* A lot of logging stuff seems to be in `util`
* `console.dir` is a wrapper around `util.inspect`

process.argv
  0: "/full/path/to/binary/node"
  1: "/full/path/to/current/script.js"
  ... any command line args ...

  example:
  ["/usr/local/Cellar/node/0.10.31/bin/node", "/Users/eoinkelly/Dropbox/Notes/on-github/node/watcher.js", "target.txt"]


*/

console.log('watching for changes...');
