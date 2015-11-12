-module(processes).
-compile(export_all).

%% a funciton (process) will only execute once by default
%% there is no restriction on the types of message you can sent it
%% there is no automatic syntax for sending the sender pid
%% receive keyword starts a process
%% somehow when a function does recursion receive is smart enough not to make a new process
%% a receive block will only receive and process *one* message
dolphin() ->
  receive
    hello -> io:format("Hello human I am dolphin~n");
    boo -> io:format("boo to you too~n");
    _ -> io:format("meep meep~n")
  end.

dolphin2() ->
  receive
    {Sender, hello} ->
      Sender ! "Hello human I am dolphin~n",
      dolphin2();
    {Sender, boo} ->
      Sender ! "boo to you too~n",
      dolphin2();
    {Sender, _} ->
      Sender ! "meep meep~n",
      dolphin2()
  end.
