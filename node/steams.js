

// import the readable stream constructor
var Readable = require( 'stream' ).Readable;

var rs = new Readable();
rs.push("hi");
rs.push("there\n");
rs.push(null); // null indicates end of stream ???

// * The data we pushed into `rs` has now been buffered and is waiting for
//   somewhere to go

rs.pipe(process.stdout);
