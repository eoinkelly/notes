Clean Code
==========

Intro
=====

2 parts to learning craft: knowledge and work
	knowledge: learn the principles, practices, patterns etc.
	work: grid that knowledge into your fingers eyes and gut

we may create languages that are closer to the requirementts and tools that help us parse anc assemble those rquirements into formal structures but we will never elminate the necessary precision - so there will always be code

LeBlanc's law: Later = never

The only way to go fast is to write clean code
bad code tempts a mess to grow


clean code
	should clearly expose the tensions in the problem to be solved. it should build those tensions to a climax  and then give the reader an "aha of course" moment
	should be decisive
	exhibits close attention to detail
	provides one way rather than many of doing the same thing
	has nothing obvious you can do to make it better

when the same thing is repreated over and over it's a sign that there is an idea in our minds that is not well represented in code

Ron Jeffries:
	* objects should only do one thing too!,
	* also mentions an "extract method" refactoring which takes a big method and breaks into one method that says what should be done and a bunch of submethods that say how it should be done.

	* when he finds himeslef doing a "find things in a collection" task (be it from a database, a hash, an array etc)  he often wraps the implementation in a more abstract method or class. This allows him to implement the functionaility simply to begin with (e.g. with a hash) but since all references in the code are to his abstract implemnetation he can change to something more complex later. In addition the collection abstraction calls his attention to what is really going on and keeps hime from running down the path of implementing aribrary collection behaviour when all he needs is a few simple ways of finding what he wants

reduced duplication, high expressiveness and early building of simple abstractions makes good code
note: *simple abstractions*

ward cunninghame: great code makes it look like the language was made for the problem

the ratio of time spent reading and writing code is approx 10:1

check in the code a little cleaner than you checked it out - boyscout rule of code

2. Meaningful Names
===================

* Use intention revealing names
Aside:
	it seems a lot of this advice rotates round revealing my intentions to the future programmer, communicating with another human being through code
	could you think of the code as just a very detailed specification of a solution for a future human that can also be understood by machine?
* Don't be afraid to chnage names if you find better ones.
* naming helps provide "context" for our solution.
* Don't use names that have another, unrelated meaning to the solution e.g. hp for hypotenuse is bad as it might be confused with HP the printer company? really???
* Becareful when using accepted computer science jargon in names e.g. list as this might imply a particular thing that has nothing to do with you solution to another programmer.
* Remember that you are naming things for other humans, don't use nonsense prefixes or postfixes to just get uniqueness to satisfy the compiler. i.e. names should mean something!
* Don't use noise words e.g. what is the difference between:
	Product
	ProductInfo
	ProductObject
	ProductData
	it's hard to tell from the names eh?
* there is an old C convention of using aFoo for local variables and theFoo for arguments to a function - BM doesn't do this anymore as modern IDEs make it unnecessary. how???
* Don't encode the variables type into it's name e.g. NameString because what happens if the type changes to a custom class? Then this name becomes disinformation.
	* Hungarian notation is now considered harmful as it encodes the type in the name (note hungarian notation is not just camel case!)
* Don't use prefixes on names to indicate stuff either e.g. m_* for member variables
* Use pronounceable names as they are easier to discuss with people
* don't use single or double letter names for anything that might have a large scope as you might have to grep for it (imagine grepping for i in a codebase!).
	* The length of a name should correspond to its scope.
	* If you use a single letter name, you force the reader to mentally map your a, b, c to whatever it actually represents in the code. This makes it harder for them.
		* The exception to this is looping variables i, j, k (never l as it's hard to distinguish from 1)
* In the case of interfaces, the convention currently is to call the interface IShapeFactory and the concrete class ShapeFactory. BM prefers to leave the interface along (call it ShapeFactory) and prefix the implementation (concrete class) lik ImpShapeFactory
* Classes and objects should have noun phrase names. Avoid names that imply a particular comp-sci pattern (e.g. factory, manager) unless you are actally implementing that pattern
* Method names should have verb phrase names e.g. postPayment, doSetup, makePage
* The javabean standard has standards for prefixing method names that are handy (in Java at least)
	* accessors = get*
	* mutators = set*
	* predicates = is*
* When constructors are overloaded BM recommends using static factory methods to create the new object e.g.
	Complex fulcrumPoint = Complex.fromRealNumber(23.0);
	Complex fulcrumPoint = new Complex(23.0); // not as clear whats going on. If you use the line above, you can make the constructor private

* Don't be cute/slang-ish wiht names e.g. eatMyShorts() where you mean kill(). Choose clarity over entertainment value.
* Use a consistent lexicon. Pick one word per abstract concept e.g. don't use
	fetch
	get
	retrieve
	as equivalent methods of different classes
	Don't use
		fooController
		barManager
	as names for objects if both are implementing the same abstract concept
	Aside: so there should be an application wide set of verbs that describe abstract concepts that apply system wide.
* Don't pun (using the same word for two purposes)
	if add() concatenates or does addition in one part of your system, don't reuse it to append stuff to an array in another.
* Write for your audience i.e. programmers. Express the solution in terms they are likely to understand (patterns, algorithms etc.) rather than those of the problem domain (which the business folk will understand but possibly not the programmers). Except if there is no "programmer-ese" for what you are doing - in that case use the problem domain as at least the future programmer can go ask someone.
* Add meaningful context
	Most names are not meaningful in and of themselves. Instead we place the names in context by enclosing them in well named classes, functions and namespaces.
* Don't add more context than is helpful as it can prevent re-use. If customerAddress contains all the stuff you need for a supplier address you can't reuse it but if it was Address then you could.
* Naming requires good descriptive skills and a shared cultural background. Naming is hard. Don't be afraid to rename stuff as you clarify your thoughts.

3. Functions
============

1. Functions should be very small
	* 4-5 lines is great!
	* hardly ever longer than 20 lines
	* Implication 1:
		The blocks within if/else/while statements should be 1 line! that line should probably be a function call
			+ keeps the enclosing function small
			+ adds documentary value because the function within the block can have a name that describes what the block does

	* Implication 2: functions should not be large enough to hold nested structures - the indent level should not be greater than one or two

2. Functions should Do One Thing. They should do it well. They should do it only.

	It can be hard to figure out what the "one thing" is.

	The steps of the function should be one level of abstraction below the stated name of the function

	Consider using a TO paragraph to describe the function

	TO {name of func}
		* task 1 of func
		* task 2 of func
		* ...

	Note that the tasks should be one level of abstraction down from the stated name of the function. For example:

		TO RenderPageWithSetupsAndTearDowns
		* we check to see whether the page is a test page and
		* if so, we include the setups and teardowns
		* In either case we render the page as HTML

	If a function does only those steps that are one level below the stated name of the funciton, then the function is doing one thing.
	After all the reason we write functions is to decompose a larger concept (the name of the function) into a set of steps at the next level of abstraction

	function largerConcept () {
		concept1LevelDown();
		anotherConceptat1LevelDown();
	}

	* If you can extact another function from it witough it's name being merely a restatement of its implementation the the function is NOT doing one  thing!
	* If a function is broken up  into sections it is obviously not doing just one thing

3. One level of abstraction per function
	* All statements in a function should be at the same level of abstraction
	* Mixing levels of abstraction in a function makes it hard for the reader to distinguish between essential concepts and details

Code should read like a top-down narrative. We want every funciton to be followed by those at the enxt level of abstraction so that we can read the program descending one level of abstraction at a time as we read down the list of functions

* So i guess I can group functions together according to their level of abstraction?


Example from creating a wordpress page
	TO create the homepage we include the header and the main content, a sidebar if one exists and the footer
	TO include the sidebar we check to see if it exists and if so include it in

Switch Statements
-----------------

* By their nature they do N things - hard to make them small
* make sure each switch is buried in a low level class and never repeated

3. Use Descriptive Names
	* The smaller and more focused a function is, the easier it is to choose a name.
		* so is it the case that if i have trouble naming a function the it is probably doing too much?
	* Don't be afraid to try many different names
	* Choosing descriptive names clarifies the design of the module in my mind and will help me improve it.

4. Be consistent in naming
	Use the same phrases, nouns and verbs in the function names you choose for your modules. For example, reading these:
		includeSetupAndTeardownPages()
		includeSetupPages()
		includeSuiteSetupPage()
		includeSetupPage()

		implies that these also exist

		includeTeardownPage()
		includeSuiteTeardownPage()
		includeTeardownPages()

		If you are able to call functions based on what you expect to exist then the code is pretty good


Function Arguments
------------------

* niladic = 0 arguments
* monoadic = 1 argument
* diadic = 2 arguments
* triadic = 3 arguments
* polyadic = 4+ arguments

* The ideal number of arguments for a function is 0.
* Try to have as few arguments as possible to a function - any more than 3 should be an extreme special case (and should probably not happen anyway!)
* Obviously you can't eliminate arguments completely but you should strive to keep their numbers down

Arguments are hard because the reader has to interpret it every time they see the function called.
Argumetns are at a different level of abstraction than the function call and using them forces you to know a detail that isn't very important at that point.

Compare:
	includeSetupPage(); // Easier to read
	includeSetupPage(newPageContent);

* Arguments are harder to test. You ahve to write test cases to ensure that the various combinations of arguments work correctly - this gets very difficult as the number of args increases.
* Output arguments are harder to understand than input arguments. When we read a functkion we are used to the idea of input coming in through the arguments and output leaving through the return value. If you have output arguments it forces the reader to do a double-take.

* Try to use an instance variable instead of arguments

Common Monadic Forms
--------------------

Why might we pass a single argument to a function?

1. We might be asking a question about that argument
2. We might be operating on that argument, transforming it into something else and returning it (note that we return the transformed value rather than just operating on our argument as that would make it an output argument which is confusing)
3. An event. This is less common than 1 or 2. There is an input arguemnt but no output. it is understood that the function will use the input arg to change the state of the system somehow. This form should be used with care and make it clear that it is an event.

These 3 forms are 3 different contexts for using a monadic function - don't mix them in a single function and make sure my functions are named to indicate what context it is in
* I guess ruby predicate naming covers 1. nicely

* These 2 forms are what readers expect when they see a function with a single argument.
* You should name my functions so that it is clear which of these actions is being done
* I should not have a monadic function that does both of these i.e. a function that answers a question about the argument but also morphs it into something else.

Flag Arguments
--------------

Flag arguments are ugly!
Passing a boolean to a function proclaims that this function does more than one thign (depending on whether the boolean is true or false)

Dyadic function forms
---------------------

* A function with 2 arguments is always harder to understand than a function with 1
* Sometimes 2 arguments are appropriate e.g. new Point(0,0); but in this case the 2 args are ordered components of the same value whereas in many cases there isn't any relationship between the args nor any natural ordering.
* Generally we have to learn to ignore some of the args to read what the function is doing e.g. writeField(outputStream, name) we ignore outputStream

* Stuff like assertEqual(expected, actual) natually has 2 args but doesn't help us remember what order they are in assertExpectedEqualsActual(expected, actual)
	* this is the "keyword" form of function naming where we encode the names of the arguments into the function name - this helps us remember the order of the args

Strategies for reducing no. of arguments of a funciton
------------------------------------------------------

* can you make the function a member of a class or object that would allow you to call it on the object rather than with the arg e.g.
		doFoo(myObj, val); could become myObj.doFoo(val);
* Use an argument object to group args together. This adds some descriptive power as that group of args becomes a named concept
* Argument lists
	e.g. ruby splat args or javascript args
	String.format("format spec %s %s %d", str1, str2, str3) // this is actually dyadic as we
	String.format("format spec", listofstrings)

Naming Functions
With our names we are trying to communicate:
1. the *intent* of the funciton
2. the order and intent of the arguments
Monad function name and argument should go together in a verb - noun pair
	write(name) // implies that a "name" is being "written" somehow
	writeField(name) // implies that a "name" is being "written" to a "field" somehow.

Functions should not have side-effects
--------------------------------------

Side effects are lies. Your function is promising to do one thing but then does other "hidden" things that are not part of the "one thing" e.g.
* change variales in it's own class
* change it's argumemtns (output arguments)
* change global variables

Note that these things are not necessairly bad as long as they are the "one thing" that the function does.

Side effects result in temporal coupling. THis is:
* Different things will happen depending on *when* you call the function

If there is a temporal coupling in the code the name of the function should reflect this

Output Arguments
----------------

appendFooter(s);

* Does this mean:
	* take s and append it to the footer of something (s is an input)
	* append a footer to s (s is an output)

* In OO languages 'this' is *intended* to be an output argument so we shouldn't need to create any more of them.
* In general output arguments shoiuld be avoided. If your function must change the state of something, have it change the state of it's owning object.

Aside:
	in front-end the DOM is a big global object. jQuery is also a global that wraps the DOM. Rather than talking directly to the DOM elements I talk to jQuery
	if functions are to do one-thing only, then my functions that change the DOM (via jQuery) should be it's "one thing"
	The "one thing" that any functions I have that change the DOM is that they "chagne the DOM"

Command Query Separation
------------------------

* Functions should either *do something* or *answer something*, but not both.
* The name of the function should indicate which action it will do.
* We will refer to these functions as command functions and query functions

Returning error codes from command functions is a sublte violation of command query separation as it encourages commands being used as predicates in if statements e.g.
	if (deletePage() == E_OK)
Also when you return an error code, you force the calling functoin to deal with the error immediately. Contrast

// Approach 1:
	if (deletePage(page) == E_OK) {
		// do success stuff
	} else {
		// deal with failure
	}

try {
	// first we try all the happy-path code without having to check the return value of each of these functions
	deletePage(page1);
	deletePage(page2);
	deletePage(page3);
	deletePage(page4);
	deletePage(page5);
} catch (Excetion e) {
	// handle the error now
}

* try/catch blocks are considered "ugly" because the confuse the structure of the code and mix error processing with normal processing --??? really?
* BM recommends extracting try/catch blocks into functions of their own
* In functions that do exactly one thing, error handling is a thing. A function that handles errors should do nothing else.

The more classes that import and use a class the harder it can be to change unless you have abstracted it behind an interface. Consider this error code examples

public enum Error {
	OK,
	INVALID,
	NO_SUCH
}

It's likely that most of the classes in the system will depend on this.
So if you change this enum you have to recompile all those classes too
This makes folks not want to change this class
So they reuse the error codes for new purposes
Hurtsville, population you.

D.R.Y. Don't Repeat Yourself
----------------------------

Duplication may be the root of all evil in software.

Structured Programming advocates that every function and every block within a function should only have one entry and exit. This means one return statement per function and no continue/break in loops. BM reckons this doesn't really matter if you keep your functions small.

Chap 4. Comemnts
================

"Don't comment bad code - rewrite it" - Brian Kerrigan and PJ Plaugher

* The proper use of comments is when we fail to express ourselves via code. BM believes that to use a comment is to admit defeat
* Comments are hard to maintain and get ignored when the code is updated and moved around.
* comments become another thing that has to be maintained and kept in good repair
aside: bifurcate := divide into two branches or forks
* Comments do not make up for bad code
* When you find yourself reaching for a comment, first ask "how can I refactor this to make it obvious enough with out the comment", then use the comment only if you absolutely need it.
* Sometimes comments can be handy to amplify the importance of some bit of code that mgiht be considered trivial e.g. trim() a string might really matter in some cases.
* Comments should be local to the code they refer to.
* Don't put in verbose comments for no reason - they are just visual clutter (PHPdoc encourages this)
* Don't have a rule that every function and/or variable must have a comment (e.g. javadoc/PHPdoc) - this creates confusion and clutter adn creates potential for misinformation and lies.
* Don't use comments that restate the obvious
* If comments are noisy we learn to ignore them (our eyes don't see them after a while). this makes it easier for errors to hide in them!
Aside: so there is a cost to every comment! Don't spend your "user attention" money on foolish comments.
* BM is not a fan of comment banners. At best he suggests not using them often so that they are startling and obvious when they do turn up. Otherwise they fall into background noise.
* Closing brace comments only make sens on functions with deeply nested structures.
* Don't leave commented out source code in files - let version control take care of keeping old code.
* Keep to the point - don't put irrelevant details or historical discussions in your comments.
* Short functions don't need much description so function header comments aren't all that useful
* Adding javadocs to non public code is not generally  useful and adds verboseness and extra formality without much benefit.
Aside: can think of comments I write as a first pass and grist for my refactoring mill.

(end chap 4)



Functions should either do something or answer something, but not both. Either your function should change the state of an object, or it should return some information about that object. Doing both often leads to confusion.

When you return an error code, you create the problem that the caller must deal with the error immediately.

If a function does only those steps that are one level below the stated name of the function, then the function is doing one thing. After all, the reason we write functions is to decompose a larger concept (in other words, the name of the function) into a set of steps at the next level of abstraction.

Hiding implementation is not just a matter of putting a layer of functions between the variables. Hiding implementation is about abstractions
