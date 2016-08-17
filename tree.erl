-module(tree).

-export([empty/0, insert/3, lookup/2, emptyLat/0, emptyLon/0, insert2/4, lookup2/2]).

empty() -> {node, 'nil'}.

%node: {node, {Key, Value, Smaller, Larger}}

insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, empty(), empty()}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}})
  when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}})
  when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

lookup(_, {node, 'nil'}) -> undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {NodeKey, _, _, Larger}}) when Key > NodeKey ->
    lookup(Key, Larger).

emptyLat () -> {node, lat, 'nil'}.
emptyLon () -> {node, lon, 'nil'}.

insert2 (Lat, Lon, Val, {node, lat, 'nil'}) ->
  {node, lat, {Lat, Lon, Val, emptyLon(), emptyLon()} };
insert2 (Lat, Lon, Val, {node, lon, 'nil'}) ->
  {node, lon, {Lat, Lon, Val, emptyLat(), emptyLat()} };
insert2 (NewLat, NewLon, NewVal, {node, lat, {Lat, Lon, Val, Smaller, Larger} })
  when NewLat < Lat ->
    {node, lat, {Lat, Lon, Val, insert2(NewLat, NewLon, NewVal, Smaller), Larger} };
insert2 (NewLat, NewLon, NewVal, {node, lat, {Lat, Lon, Val, Smaller, Larger} })
  when NewLat >= Lat ->
    {node, lat, {Lat, Lon, Val, Smaller, insert2(NewLat, NewLon, NewVal, Larger)} };
insert2 (NewLat, NewLon, NewVal, {node, lon, {Lat, Lon, Val, Smaller, Larger} })
  when NewLon < Lon ->
    {node, lon, {Lat, Lon, Val, insert2(NewLat, NewLon, NewVal, Smaller), Larger} };
insert2 (NewLat, NewLon, NewVal, {node, lon, {Lat, Lon, Val, Smaller, Larger} })
  when NewLon >= Lon ->
    {node, lon, {Lat, Lon, Val, Smaller, insert2(NewLat, NewLon, NewVal, Larger)} }.

lookup2 (X, Y) -> {X, Y}.