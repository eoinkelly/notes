-module(day_1).
-export([mirror/1]).
-export([number/1]).
-export([fact/1]).
-export([fib/1]).
-export([counter/1]).
-export([matcher/1]).
-export([count_words/1]).

% Syntax
% ======
% * all executable statements end in .
% * all clauses in a function except the last one end in ;
% * _ is the "I don't care about this" variable name - erlang will not complain about it being unused.
% * the methods are exported with their arity

% Questions
% * how to do multi-line methods?


% Thing variable's type is decided at runtime.
mirror(Thing) -> Thing.

% notice that when defining cases for a function you use ; as the delimiter for all lines except the last
number(one) -> 1;
number(two) -> 2;
number(three) -> 3;
number(_) -> "sorry not recognised".


fact(0) -> 1;
fact(N) -> N * fact(N-1).


fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).


% Exercise 1
% ----------
% want to split a string into words and count the words
count_words(Str) -> string:words(Str).

% Exercise 2
% ----------
% write a function that uses recursion to count to ten

% how to guard against input bigger than 10?
% Passing an arg that is not an Integer:
%   * it will blow up on the N+1 line
%   * are there type classes like haskell?
counter(10) -> 10;
counter(N) -> counter(N+1).


% Exercise 3
% ----------
% write a function that uses matching to print "success" or "error: message"
% given inputs of either success or {error, Message}
matcher(success) -> "success";
matcher({error, Message}) -> "error: " ++ Message.
                             
