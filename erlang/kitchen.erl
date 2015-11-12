-module(kitchen).
-compile(export_all).

%% when the shell (maybe all processes) sends a message it goes immediately into receive mode

%% Spawing a process with spawn/3
%%
%% * spawn does not actually run the function
%% * ?MODULE returns the current module's name
start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg;
  Unexpected ->
    io:format("fridge2/1 got an unexpected message: ~p~n", Unexpected)
  after 3000 ->
    timeout
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg;
  Unexpected ->
    io:format("fridge2/1 got an unexpected message: ~p~n", Unexpected)
  after 3000 ->
    timeout
  end.

stop(Pid) ->
  Pid ! terminate.

%% Private functions
%%
%% * To make a function private you just don't export it
%% * TIP: you can use `-ifdef(TEST)` macro to conditionally export extra
%%   functions in dev and test.

%% * the state of a function can be held in the parameters passed to it
%% * since the function invokes itself it can do so with new params when it
%%   wants to change state!
%% * there is no easy way to inspect the state of variables maintained by
%%   recursively calling function (e.g. FoodList in example below)
%% * only messages that match a pattern in `recieve` are removed from the inbox
%%   - it it does not match it is left in there.
%%
fridge2(FoodList) ->
  io:format("Fridge contents: ~p~n", [FoodList]),
  receive
    {From, {store, Food}} ->
      From ! {self(), stored},
      fridge2([Food | FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), taken},
          fridge2(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2(FoodList)
      end;
    terminate -> ok;
    Unexpected ->
      %% guard against getting a message we do not understand
      io:format("fridge2/1 got an unexpected message: ~p~n", Unexpected)
  end.

