Regular Expressions
===================

Inbox
======

@codinghorror recommends this book http://www.codinghorror.com/blog/2009/06/regular-expressions-for-regular-programmers.html
Good reference: https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/RegExp#Special_characters_in_regular_expressions
Online tester: http://regexpal.com/
Javascript lib to remove x-browser diffs in syntax: http://xregexp.com/
Windows tool for creating them: http://www.regexbuddy.com/


http://www.regular-expressions.info/lookaround.html
negative lookaround
	matches any 'yz' that does not have 'x' right before it
	(?!x)yz

Positive lookaround
	matches any 'yz' that has 'x' right before it
	(?=x)yz