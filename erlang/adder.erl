-module(adder).
-export([add/2, hello/0, greet_and_add_magic/1]).
%% -import(io, [format/1]).
-define(MAGIC, 2).

add(A, B) ->
  A + B.

hello() ->
  io:format("hello there~n").

greet_and_add_magic(A) ->
  hello(),
  add(A, ?MAGIC).
