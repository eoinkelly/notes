
currying

wu.js

foo(a)(b)(c) // this is what currying looks like if you want to call curried functions all together

lo-dash has .autoCurry which will automatically curry the argus

var foo = function (a, b, c) {
}.autoCurry();


if you have a func that takes 3 args and a func that takes 2, you can make up to 6 new functions
    really? doesnt order matter

    in funcitonal style, the order of args really mattes because it decides how you can curry

    underscores api prevents you from currying becasue the args are backwards

currying pros:
* you can make generic functions - the data is gone
    * I guess they don't depend on any particular data set
* You can build new functions by applying args
* You get much more concise definitions
* You can make types "line up" for composition

underscore has both chain() and compose()

composition of functions f, g, h is f(g(h(x)))
so for example
var last = compose(first, reverse) // find the last element in an array
composition reads right to left

last([1,2,3]) // [3,2,1]

pros of composition:
* you don't need to mention args at all - makes the funciton very generic, less extra detail when you read it
  * this is also known as a "data generic" way
* compositon is more declarative
  * it lets the details of how the composition is implemented to something else
* you don't have to wrap/unwrap data like  you do with chain() - compose has a lot less ceremony
* build new functions from other functions
* helps build generic programs without args!!!!
* very high coding level
* mathmatically backed


