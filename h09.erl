-module(h09).

-compile(export_all).


start() ->
  InitialState = [],
  spawn(?MODULE, loop, [InitialState]).

loop(State) ->
  receive
    {add, From, Item} ->
      NewState = [Item | State],
      From ! {reply, ok},
      ?MODULE:loop(NewState);
    {remove, From, Item}  ->
      {Reply, NewState} =
        case lists:member(Item, State) of
          true  -> {ok, lists:delete(Item, State)};
          false -> {{error, not_exists}, State}
        end,
      From ! {reply, Reply},
      ?MODULE:loop(NewState);
    {show_items, From} ->
      From ! {reply, State},
      ?MODULE:loop(State);
    stop -> io:format("~p stoped ~n", [self()]);
    _    -> ?MODULE:loop(State)
  end.

add_item(Pid, Item) ->
  Pid ! {add, self(), Item},
  handle_reply().

remove_item(Pid, Item) ->
  Pid ! {remove, self(), Item},
  handle_reply().

show_items(Pid) ->
  Pid ! {show_items, self()},
  handle_reply().

stop(Pid) ->
  Pid ! stop,
  ok.

handle_reply() ->
  receive
    {reply, Reply} -> Reply
  after 5000 -> timeout
  end.
