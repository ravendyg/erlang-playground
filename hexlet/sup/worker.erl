-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
  {ok, Id}.


ping(Pid) ->
  gen_server:call(Pid, get_id).


handle_call(get_id, _From, Id) ->
  {reply, {Id, self()}, Id}.


%%  plugs
handle_cast(_, Id) ->
  {noreply, Id}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) -> ok.
