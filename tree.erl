%https://en.wikipedia.org/wiki/K-d_tree

-module(tree).

%-import(us).

-export([empty/0, insert/3, lookup/2, emptyLat/0, emptyLon/0, insert2/4,
    lookup2/2, test2d/0, testList2d/1, testWiki2d/1, getTestPoints/0,
    getCenter/1, calculateDistanceToCenter/1, map/2]).

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

test2d () ->
  Q  = insert2(7,2,1,emptyLat()),
  Q2 = insert2(5,4,2,Q),
  Q3 = insert2(2,3,3,Q2),
  Q4 = insert2(9,6,4,Q3),
  Q5 = insert2(4,7,5,Q4),
  insert2(8,1,6,Q5).

% create a 2D tree from a list [{X, Y, N}] of points
testList2d (L) -> testList2dTail(L, emptyLat()).

testList2dTail ([], Acc) -> Acc;
testList2dTail ([{Lat, Lon, Val}|T], Acc) ->
  testList2dTail(T, insert2(Lat, Lon, Val, Acc)).

% order point by their distance from the center of gravity
% then create a 2D tree
% accepts list of point [{X,Y,N}]
% or 'nil' if you want to use internal test set
testWiki2d ('nil') ->
  {_, _, _, L} = calculateDistanceToCenter( getTestPoints() ),
  Vals = qs(L),
  Points = [{X,Y,N} || {point, {X,Y,N}, distance, _} <- Vals],
  testList2d(Points);
testWiki2d (RandomList) ->
  {_, _, _, L} = calculateDistanceToCenter(RandomList),
  Vals = qs(L),
  Points = [{X,Y,N} || {point, {X,Y,N}, distance, _} <- Vals],
  testList2d(Points).

getTestPoints () -> [{2,3,1},{5,4,2},{9,6,3},{4,7,4},{8,1,5},{7,2,6}].

% calculate center of gravity in 2D space
getCenter (L) -> getCenterAcc(L, 0, 0, 0).
getCenterAcc ([], _, _, 0) -> {0, 0};
getCenterAcc ([], X, Y, N) -> {X/N, Y/N};
getCenterAcc ([{Xc, Yc, _}|T], X, Y, N) -> getCenterAcc(T, X+Xc, Y+Yc, N+1).

% calculate center of given points and their distances from that center
% [{lat, lon, anything}] -> {a:center, {Xc, Yc}, a:points, [{a:point, {lat, lon, anything}, a:distance, distance}]
calculateDistanceToCenter (L) ->
  Center = getCenter(L),
  {center, Center, points, addDistance(L, Center, [])}.

% calculate distance on a plane between 2 points
get2DDistance ({X1, Y1}, {X2, Y2}) -> math:sqrt( math:pow(X1-X2,2) + math:pow(Y1-Y2,2) ).

% add distance to some center to all points in the lists
% [{lat, lon, anything}, {Xc, Yc}, []] -> [{a:point, {lat, lon, anything}, a:distance, distance}]
addDistance ([], _, Acc) -> Acc;
addDistance ([{X, Y, N}|T], {Xc, Yc}, Acc) ->
  addDistance(T, {Xc, Yc}, [{point, {X,Y,N}, distance, get2DDistance({X, Y}, {Xc, Yc})} | Acc]).

% map helper function
map (F, L) -> lists:reverse( mapTail(F, L, []) ).
mapTail (_, [], Acc) -> Acc;
mapTail (F, [H|T], Acc) -> mapTail(F, T, [F(H) | Acc]).

% quick sort for addDistance output
qs([])    -> [];
qs([H|T]) ->
    {Ls, Eq, Gt} = part(H,T),
    qs(Ls) ++ [H|Eq] ++ qs(Gt).

compPr(_,[],R) -> R;
compPr({Q,W,E,A},[{R,T,Y,H}|L],{Ls,Eq,Gt}) when A > H  -> compPr({Q,W,E,A}, L, {[{R,T,Y,H}|Ls], Eq, Gt});
compPr({Q,W,E,A},[{R,T,Y,H}|L],{Ls,Eq,Gt}) when A < H  -> compPr({Q,W,E,A}, L, {Ls, Eq, [{R,T,Y,H}|Gt]});
compPr({Q,W,E,A},[{R,T,Y,H}|L],{Ls,Eq,Gt}) when A == H -> compPr({Q,W,E,A}, L, {Ls, [{R,T,Y,H}|Eq], Gt}).

part(A,L) -> compPr(A,lists:reverse(L),{[],[],[]}).