-module(ks2).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> sr:start_link(?MODULE, []).

%% Sync call
order_cat(Pid, Name, Color, Description) ->
  sr:call(Pid, {order, Name, Color, Description}).

%% Async
return_cat(Pid, Cat = #cat{}) ->
  sr:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  sr:call(Pid, terminate).




init([]) -> [].

handle_call({order, Name, Color, Description}, From, Cats) ->
  if Cats =:= [] ->
      sr:reply(From, make_cat(Name, Color, Description)),
      Cats;
    Cats =/= [] ->
      sr:reply(From, hd(Cats)),
      tl(Cats)
  end;

handle_call(terminate, From, Cats) ->
  sr:reply(From, ok),
  terminate(Cats).

handle_cast({return, Cat=#cat{}}, Cats) ->
  [Cat | Cats].


make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.