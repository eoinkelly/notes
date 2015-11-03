-module(greeter).
-export([greet/2]).

greet(irish, Name) ->
  io:format("Cead mile failte ~s~n", [Name]);
greet(kiwi, Name) ->
  io:format("Kia ora ~s~n", [Name]);
greet(_, Name) ->
  io:format("Hello there ~s~n", [Name]).

