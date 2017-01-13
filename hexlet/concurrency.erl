-module(concurrency).

-compile(export_all).

say_smth(_, 0) ->
  io:format("Done ~n");
say_smth(Val, Count) ->
  io:format("~s ~n", [Val]),
  say_smth(Val, Count - 1).

start(Val1, Val2) ->
  spawn(concurrency, say_smth, [Val1, 3]),
  spawn(concurrency, say_smth, [Val2, 3]).