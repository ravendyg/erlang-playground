-module(worker).

-export([start_file_reader/2, add_count/3]).

start_file_reader(FileName, Pid) ->
  {ok, Str} = file:read_file(FileName),
  Pid ! {data, parse_file(Str)}.

parse_file(Binary) ->
  Items = binary:split(Binary, [<<"\n">>, <<"\n\r">>], [global, trim]),
  put_item_to_map(maps:new(), Items).

put_item_to_map(State, []) ->
  State;
put_item_to_map(State, [Item | Items]) ->
  [_, Key, BinaryCount, _] = binary:split(Item, [<<",">>], [global, trim]),
  {IntCount, []} = string:to_integer(unicode:characters_to_list(BinaryCount)),
  NewState = add_count(State, Key, IntCount),
  put_item_to_map(NewState, Items).

add_count(State, Key, Val) ->
  NewValue =
    case maps:find(Key, State) of
      {ok, Value} -> Val + Value;
      error -> Val
    end,
  maps:put(Key, NewValue, State).