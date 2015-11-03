-module(patterns).
-compile(export_all).

%% the first clause will match any 2 args
%% the second clause has a diff no. of args so erlang doesn't consider it part
%% of same clause
valid_date_1(Date, Time) ->
  {Y, M, D} = Date,
  {H, Min, S} = Time,
  io:format("Raw date: ~p~n", [Date]),
  io:format("Raw time: ~p~n", [Time]),
  io:format("Timestamp is: ~p/~p/~p ~p:~p:~p~n", [Y, M, D, H, Min, S]).
valid_date_1(_) ->
  io:format("You gave me garbage date and time~n").

valid_date_2(Date = {Y, M, D}, Time = {H, Min, S}) ->
  %% using = in the pattern match allows us to access original vars and their
  %% destructured parts
  io:format("Raw date: ~p~n", [Date]),
  io:format("Raw time: ~p~n", [Time]),
  io:format("Timestamp is: ~p/~p/~p ~p:~p:~p~n", [Y, M, D, H, Min, S]);
valid_date_2(_,_) ->
  io:format("You gave me garbage date and time~n").
