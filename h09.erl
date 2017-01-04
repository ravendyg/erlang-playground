-module(h09).

-compile(export_all).


start() ->
  io:format("start ~p~n", [self()]),
  spawn(fun loop/0).

loop() ->
  io:format("~p enters loop ~n", [self()]),
  receive
    stop -> io:format("~p stops now ~n", [self()]);
    Msg  ->
      io:format("~p receive ~p~n", [self(), Msg]),
      loop()
  end.