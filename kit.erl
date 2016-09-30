-module(kit).
-compile(export_all).

fridge1() ->
  receive
    {From, {store, _Food}} ->
      From ! {self(), ok},
      fridge1();
    {From, {take, _Food}} ->
      %% to be ...
      From ! {self(), not_found},
      fridge1();
    terminate ->
      ok
  end.

fridge2( FoodList ) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      putIntoFridge( Food, FoodList );
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), Food},
          fridge2( lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2( FoodList )
      end;
    {From, list} ->
      From ! FoodList,
      fridge2( FoodList );
    terminate ->
      ok
  end.

putIntoFridge( Food, FoodList )
  when is_list( Food ) == true -> fridge2( Food ++ FoodList );

putIntoFridge( Food, FoodList ) -> fridge2( [ Food | FoodList ] ).


store( Pid, Item ) ->
  Pid ! { self(), {store, Item} },
  receive
    {Pid, Msg} -> Msg
  end.

take( Pid, Item ) ->
  Pid ! { self(), {take, Item} },
  receive
    {Pid, Msg} -> Msg
  end.

list( Pid ) ->
  Pid ! { self(), list },
  receive
    Msg -> Msg
  end.

createFridge( FoodList ) -> spawn( ?MODULE, fridge2, [FoodList] ).