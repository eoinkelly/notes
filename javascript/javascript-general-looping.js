// General Looping
// ===============

// Looping X Times
// ---------------

// This loop is syntactically concise if we don't care what the value of the iterator is, we just want to do something X times
// remember that repeat still exists after the loop has finished
// In this formulation, repeat takes on values from 9 -> 0 within the loop and the final test sets its value to -1 for the rest of the function.

var repeat = 10;
while (repeat--) {
  // test truthyness, then subtract 1
  console.log('repeat: ' + repeat);
}
// repeat = -1 from here on

// In this formulation, repeat takes on values from 9 --> 1 - WATCH OUT this does not loop 10 times
var repeat = 10;
while (--repeat) {
  // subtract 1, then test truthyness
  console.log('repeat: ' + repeat);
}
// repeat = 0 from here on

// This behaves identically to --repeat, only loops 9 times !!!
var repeat = 10;
while ((repeat -= 1)) {
  // subtract 1, then test for truthyness
  console.log('repeat: ' + repeat);
}
// repeat = 0 from here on

/*
for ([initialization]; [condition]; [final-expression])
	statement

initialization = an expression run before the loop begins
	* the result of this expression is thrown away
	* variables in the expression are not local to the loop!

condition
	* an *expression* to be evaluated before each loop iteration
	* if the expression evaluates to true, statement is executed
	* if the expression evaluates to false the loop is exited

final-expression
	* An expression to be evaluated at the end of each loop iteration *before* the next evaluation of condition
	* you can imagine this being pasted in at the end of the code in the loop

the execution path for a loop that goes through 3 iterations is
[initialization]

[condition]
[statement]
[final-expression]

[condition]
[statement]
[final-expression]

[condition]
[statement]
[final-expression]

[condition] // evaluates false so we exit the loop
*/

// 1 based counting down
// i takes on values from 10 -> 1 (10 iterations)
var repeat = 10;
var i;
for (i = repeat; i > 0; i -= 1) {
  console.log('i: ' + i);
}
// i = 0 for the rest of the code in this function

// 0 based counting down
// maybe handy for arrays
// i takes on values from 9 -> 0 (10 iterations)
var repeat = 9;
var i;
for (i = repeat; i >= 0; i -= 1) {
  console.log('i: ' + i);
}
// i = 0 for the rest of the code in this function

// Looping & Counting (looping X times & having a variable to know which iteration you are on, either 0 or 1 based counting)
// ------------------------------------------------------------

// Looping with an index from 1 -> maxvalue. In this case the last vlaue of j
// will be maxValue and this is the value that j will have after the loop has
// finished.
var j, maxValue;
for (j = 1; j <= maxValue; j += 1) {
  console.log('hello');
}
// j = maxValue for rest of code in this function
