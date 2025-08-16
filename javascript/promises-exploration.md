# Q docs

then(successFunc, errorFunc)

- _always_ returns a promise
- the promise ALWAYS fulfills or rejects with the VALUE of the input function
- if that VALUE is not a promise, it is turned into one
- only one of the "input function" (successFunc, errorFunc) is ever called

input function (either successFunc or errorFunc)

if the input function ...

- throws an error => then() creates a new rejected promise from that error
- returns a value => then returns a promise which fulfills to that value
- returns a promise => then returns that exact promise

> A promise is an object that represents the return value or the thrown
> exception that the function may eventually provide

Callbacks are an "inversion of control"

- A function that accepts a callback is saying:
    - "Don't call me I'll call you"
    - "give me the args I need and tell me who to tell next - I won't give _you_
      back anything"

Promises "uninvert" this - they cleanly separate input arguments from "control
flow arguments" _ this makes it easier to create _ functions that take a
variable num of args _ but doesn't node manage this by always having hte error
first? _ implement rest and spread parameters

- Q can be used to exchange promises with a bunch of the other implementations

Other promise implementations:

- Dojo
- jQuery
- When.js
- WinJS
- RSVP

```js

function doLongThing(successCBs, errorCBs) {
  $.ajax('...',
    function (data) {
      _.each(successCBs, function(cb) {
        cb(data);
      });
    },
    function(error) {
      _.each(errorCBs, function(cb) {
        cb(error);
      });
    }
  );
}

// Option 1:
// register a bunch of success and error callbacks
// identified by position
doLongThing([cb1, cb2, cb3], [err1, err2]);
// pros/cons
// - positional args
// - all callbacks have to be known before hand - no way to add them one by one
// - all callbacks have to be available in one function
// = it is assumed that all success callbacks will get the value from doLongThing
// = it is assumed that all error callbacks will get the error thrown by doLongThing
// + can register multiple callbacks


// Option 2:
// I could return an object that contains a "register callbacks" function that would add a callback to doLongThing's list

function doLongThing() {
  var successCBs = [],
      errorCBs = [];

  $.ajax('...',
    function blahblah (data) {
      _.each(successCBs, function(cb) {
        cb(data);
      });
    },
    function(error) {
      _.each(errorCBs, function(cb) {
        cb(error);
      });
    }
  );

  return {
    register: function(cb, errcb) {
      successCBs.push(cb);
      errorCBs.push(errcb);
    }
  };
}


var x = doLongThing();

x.register(function(val) {
  // this is run by doLongThing when it has a successful outcome
}, function () {});

// Pros/cons
// - if register() is called after doLongThing is finished (has had its "resolution event") then the callback it provides will not be run
//
// Option 3:
// * run the callbacks that register() provides immediately if the resolution event has happened
//
/*
  next it would be good to generalize this pattern so it could be used for more than just doLongThing()
 */

//oneOneSecondLater is a function that takes a func as arg - it will call that func with the param '1' after one second
var oneOneSecondLater = function (callback) {
    setTimeout(function () {
        callback(1);
    }, 1000);
};

// call the supplied callback only after both invocations of oneOneSecondLater have called their callbacks
// this makes our callback be dependant on the finishing of the first two
var twoOneSecondLater = function (callback) {
    var a, b;
    var consider = function () {
        if (a === undefined || b === undefined)
            return;
        callback(a + b);
    };
    oneOneSecondLater(function (_a) {
        a = _a;
        consider();
    });
    oneOneSecondLater(function (_b) {
        b = _b;
        consider();
    });
};

twoOneSecondLater(function (c) {
    // c === 2
});


/*
If I have a promises for the sum of two numbers that were calculated async then I don't have the value

if then() returns a promise, it is wrapping the value returned by the callback you give it
  it is a promise that the system will run whatever function you gave then and its value will wrapped by this new promise


p2 = p1.then(function foofoo(p1Value) {
  return p1Value + 1; // <-- this will be the "value" of p2.
})

p3 = p2.then(function barbar(p2Val) {
  return p2Val + 10;
})


p1.then returns immediately (on the same turn) and puts a promise object in p2
at some stage in the future
  p1 will resolve (fill in its value and call foofoo() with the p1 value as the arg)

p2 is a promise that will resolve when foofoo completed and returned a value

then() returns new  promise for the callback you give it
then() makes a new promise that will not resolve until the callback you give it has been run.
then(): create a new promise that will resolve when you finish this callback

pY =pX.then(blah) is like saying "run blah when pX resolves and return me a promise that will be resolved when blah has been run (pY)"

in this way pY's resolving will happen after pX resolves
the resolution of pY depends on pX
 */

/*
Node uses callbacks

* first param to an async function is always the err

 */

// The idiom is as follows
function readJSON(filename, callback){
  fs.readFile(filename, 'utf8', function (err, res){
    if (err) return callback(err); // <-- if we got an err we handle it immediately
    callback(null, JSON.parse(res));
  });
}


// Extracting a value out of a promise
// *****************************
p1 = new Promise(function (resolve, reject) {

});
p2 = p1.then(function foofoo(p1Value) {
  return p1Value + 1; // <-- this will be the "value" of p2.
});

p3 = p2.then(function barbar(p2Val) {
  return p2Val + 10;
})

// say we want to get the value out of p3
var x;
p3.then(function(val) {
  x = val;
});

// Should I store a promise as a way of storing a value? Probably not - prob makes more sense to extract it and store it directly
//
//
// The return value of a then() callback is a bit magic. If it is
// * a normal value: it will be passed as the argument to the next then()
// * a thennable: the following then() is only evaluated when the return value resolves
//
// Promise rejections skip forward to the next "then" with a rejection callback (or "catch", since it's equivalent).
// Rejections happen when a promise is explicitly rejected, but also implicitly if an error is thrown in the constructor callback:


// Imagine a complex synchronous flow:

if (var data = attemptToGetData()) {
  if (validateTheData(data)) {
    if (hitAnotherApiForExtraData()) {
      if (normalizeAllData()) {
        showDataToUser();
      }
    }
  }
} else {
  if (attemptToHandleErr()) {
    if (tryToHandleAgain()) {
      giveUpAndTellUser();
    }
  }
}

// ****************

if(doThing()) {
  b();
} else {
  c();
}

var p = new Promise(doB, doC) {
  var val = doThing();
  if (val) {
    doB();
  } else {
    doC();
  }
}


var promise = new Promise(function (resolve, reject) {

  // you would only really wrap a promise if the thing you are doing is aync
  // would you use a promise if you are doing something sync?
  // + handy if other things in the flow are async
  // + a sync thing is just a promise that resolves immediately
  //
  //
  // The purpose of this funciton is to create the value or error
  // without promises you could embed the logic that comes after each of those conditions directly in here
  // the promise decouples the "doing of the async thing" from the actions you take in respone to its outcome
  //
  // in order to do something useful with the promise you need at least one then()
  $.get('/blah/blah', function (val) {
    resolve(val);
  },
  function (msg) {
    reject(new Error(msg));
  });
});

// Event aggregator example
// ************************

// oracle is an event aggregator ala Backbone
var oracle = {
  // ...
};

oracle.listen('blah-success', function (val) {
  console.log(val);
});

oracle.listen('blah-error', function(err) {
  console.log(err);
});


// Using an event aggregator I do the async thing and then just notify the oracle with what happened and the value I made
$.get('/blah/blah', function (val) {
  oracle.notify('blah-success', val);
},
function (msg) {
  oracle.notify('blah-fail', new Error(msg));
});

// so a promise is a way to have events without a global
// and you can register listeners in pairs
// * in both cases you can be explicity about what order to register listeners

// * event aggregators get very complex as you have more complex logic
// * (listeners have to emit different events that themselves have listeners)

// a better name for then() might be andThenRegister(successListener, rejectListener)
// the return value of the listeners you pass to then()
//    is ???

```
