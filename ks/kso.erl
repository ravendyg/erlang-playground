%%% otp based implementation

-module(kso).
-behaviour(gen_server).

-export([start_link/0, order_cat/4, close_shop/1, return_cat/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% -behaviour(ks_behaviour).

-record(cat, {name, color=green, description}).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% Sync calls
% order_cat
order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).


% close_shop
close_shop(Pid) ->
  gen_server:call(Pid, terminate).



%% Async cals
return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).



%%% Server functions
init([]) -> {ok, []}.

% handle_call
handle_call({order, Name, Color, Description}, _From, Cats) ->
  if
    Cats =:= [] ->
      {reply, make_cat(Name, Color, Description), Cats};
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;

handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

% handle_cast
handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

% handle_info
handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, Cats}.

% terminate
terminate(normal, Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.

% code_change
code_change(_OldVsn, State, _Extra) ->
  %% Not used now
  {ok, State}.


%%% Private funcitons
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.