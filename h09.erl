-module(h09).

-compile(export_all).


start() ->
  InitialState = [],
  spawn(?MODULE, loop, [InitialState]).

loop(State) ->
  receive
    {add, Item} ->
      io:format("~p adds ~p to its state ~n", [self(), Item]),
      NewState = [Item | State],
      ?MODULE:loop(NewState);
    {remove, Item}  ->
      NewState =
        case lists:member(Item, State) of
          true  -> lists:delete(Item, State);
          false ->
            io:format("Have no ~p~n", [Item]),
            State
        end,
        ?MODULE:loop(NewState);
    show_items ->
      io:format("my items are ~p~n", [State]),
      ?MODULE:loop(State);
    stop -> io:format("~p stoped ~n", [self()]);
    _    -> ?MODULE:loop(State)
  end.