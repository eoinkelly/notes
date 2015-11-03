-module(strange_if).
-compile(export_all).

basic_if(A, B) ->
  %% note the ; at end of each branch except the last
  RetVal = if A =:= B -> io:format("are equal~n");
              A > B -> io:format("first is bigger~n");
              true -> io:format("fell into true branch~n")
           end,
  %% A contrived example to show that if expressions return a value
  {all_well, RetVal}.

other_if(A, B) ->
  %% a contrived and broken example to show usage of , and ; in else clause
  if A =:= B, A > 5; B < 20 -> io:format("are big and equal~n");
     true -> io:format("NOT big and equal~n")
  end.
