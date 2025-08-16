/*
# Essential Javascript Design Patterns

http://addyosmani.com/resources/essentialjsdesignpatterns/book/

*	A pattern is a template for solving problems
*	A pattern is a reusable solution that can be applied to commonly occuring problems in software design


Creating objects in JS

3 common ways:
	var newObject = {};
	var newObject = Object.create(null); // NOt in IE 6-8, can be polyfilled
	var newObject = new Object();

What are the differences?

*/

/*
Module Pattern
--------------
*	http://www.joezimjs.com/javascript/javascript-closures-and-the-module-pattern/#more-826
*/
var Module = (function () {
  function privateFunc() {
    // I have no access to the public functions
    console.log('I am private func');
  }

  // The return statement is very long & a bit unwieldy
  return {
    publicFunc: function () {
      console.log('I am public func');
    },
    publicFunc2: function () {
      // I can access privateFunc() because of closure
      privateFunc();
      // Public functions have to use this to find each other
      this.publicFunc();
    }
  };
})();

/*
Revealing Module Pattern
------------------------
*	http://www.joezimjs.com/javascript/javascript-closures-and-the-module-pattern/#more-826
*/

var Module = (function () {
  /*
	Pros:
	*	All functions are declared and implemented in the same way
	*	Private functions now have access to public funcitons if they need it
	*	Public functions can call each other using publicFunc1() rather than
		this.publicFunc1() which
		*	saves a few chars
		*	protects you from errors if 'this' is not what you expected
	Cons:
	*	You have to write a bit more code
	*/
  var privateFunc1 = function () {};
  var publicFunc1 = function () {
    console.log('I am public');
  };

  // Notice how clean and easy to read the return object is
  return {
    publicFunc1: publicFunc1,
    publicFunc2: publicFunc2
  };
})();

/*
	Extending existing objects in JS
*/

// optino 1
// - we return the whole (huge) jquery object
var jQuery = (function ($) {
  $.myPluginFunc = function () {};

  return $;
})(jQuery);

// Option 2:
/*
 * Augments the jquery object
 */
(function ($) {
  $.myPluginFunc = function () {};
})(jQuery);
