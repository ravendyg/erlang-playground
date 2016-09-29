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
      fridge2( [Food | FoodList] );
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