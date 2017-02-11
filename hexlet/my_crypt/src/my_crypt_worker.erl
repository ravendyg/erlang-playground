-module(my_crypt_worker).
-behavior(gen_server).

-export([start_link/0, encode/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, no_state}.

%% API
encode(Input) ->
  gen_server:call(?MODULE, {encode, Input}).


%% Helpers
encode(<<>>, _, Acc) -> Acc;
encode(Input, <<>>, Acc) -> encode(Input, get_key(), Acc);

encode(<<In:8, RestIn/binary>>, <<Key:8, RestKey/binary>>, Acc) ->
  encode(RestIn, RestKey, <<Acc/bytes, (In bxor Key):8>>).

get_key() ->
  {ok, Key} = application:get_env(my_crypt, crypt_key),
  Key.


%% sync
handle_call({encode, Input}, _From, State) ->
  {reply, encode(Input, get_key(), <<>>), State}.


%%  plugs
handle_cast(_, Id) ->
  {noreply, Id}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) -> ok.