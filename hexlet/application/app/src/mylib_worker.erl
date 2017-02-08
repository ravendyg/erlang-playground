-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, []}.


%% API
get_version() ->
  {ok, Version} = application:get_key(mylib, vsn),
  Version.

get_modules() ->
  {ok, Modules} = application:get_key(mylib, modules),
  Modules.

get_min_val() ->
  {ok, MinVal} = application:get_env(mylib, min_val),
  MinVal.

get_connection_timeout() ->
  {ok, Timeout} = application:get_env(mylib, connection_timeout),
  Timeout.

all_apps() ->
  lists:foldl(fun conver_app_info/2, maps:new(), application:which_applications()).

conver_app_info({Name, Desc, Version}, Map) ->
  maps:put(Name, #{description => Desc, version => Version}, Map).


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