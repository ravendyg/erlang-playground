-module(worker).

-export([start_file_reader/1, add_count/3]).

start_file_reader(Pid) ->
  Ref = erlang:monitor(process, Pid),
  receive
    {read, FileName} ->
      {ok, Str} = file:read_file(FileName),
      Pid ! {data, parse_file(Str)};
    {'DOWN', Ref, process, Pid, _Info} -> ok
  end.

parse_file(Binary) ->
  Items = binary:split(Binary, [<<" ">>, <<"\n">>, <<"\r">>], [global, trim]),
  put_item_to_map(maps:new(), Items).

put_item_to_map(State, []) ->
  State;
put_item_to_map(State, [Item | Items]) ->
  [_, Key, Count, _] = string:tokens(unicode:characters_to_list(Item), ","),
  {IntCount, []} = string:to_integer(Count),
  NewState = add_count(State, Key, IntCount),
  put_item_to_map(NewState, Items).

add_count(State, Key, Val) ->
  NewValue =
    case maps:find(Key, State) of
      {ok, Value} -> Val + Value;
      error -> Val
    end,
  maps:put(Key, NewValue, State).