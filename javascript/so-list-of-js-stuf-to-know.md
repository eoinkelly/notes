That object.prop and object['prop'] are the same thing (so can you please stop using eval, thanks);
that object properties are always strings (even for arrays);
what for...in is for (and what it isn't).

Property-sniffing;
what undefined is (and why it smells)
 why the seemingly-little-known in operator is beneficial and different from typeof/undefined checks
 hasOwnProperty
 the purpose of delete.

That the Number datatype is really a float
 the language-independent difficulties of using floats
 avoiding the parseInt octal trap.

Nested function scoping
 the necessity of using var in the scope you want to avoid accidental globals
 how scopes can be used for closures
 the closure loop problem.

How global variables and window properties collide
 how global variables and document elements shouldn't collide but do in IE
 the necessity of using var in global scope too to avoid this.

How the function statement acts to ‘hoist’ a definition before code preceding it
 the difference between function statements and function expressions
 why named function expressions should not be used.

How constructor functions, the prototype property and the new operator really work
 methods of exploiting this to create the normal class/subclass/instance system you actually wanted
 when you might want to use closure-based objects instead of prototyping. (Most JS tutorial material is absolutely terrible on this
 it took me years to get it straight in my head.)

How this is determined at call-time, not bound
 how consequently method-passing doesn't work like you expect from other languages
 how closures or Function#bind may be used to get around that.

Other ECMAScript Fifth Edition features like indexOf, forEach and the functional-programming methods on Array
 how to fix up older browsers to ensure you can use them
 using them with inline anonymous function expressions to get compact, readable code.

The flow of control between the browser and user code
 synchronous and asynchronous execution
 events that fire inside the flow of control (eg. focus) vs. events and timeouts that occur when control returns
 how calling a supposedly-synchronous builtin like alert can end up causing potentially-disastrous re-entrancy.

How cross-window scripting affects instanceof
 how cross-window scripting affects the control flow across different documents
 how postMessage will hopefully fix this.