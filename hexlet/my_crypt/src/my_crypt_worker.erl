-module(my_crypt_worker).
-behavior(gen_server).

-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% -export([encode/3, grow_binary_not_shorter/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, no_state}.

%% API
encode(Input) ->
  gen_server:call(?MODULE, {encode, Input}).

get_key() ->
  {ok, Key} = application:get_env(my_crypt, crypt_key),
  Key.

set_key(NewKey) ->
  application:set_env(my_crypt, crypt_key, NewKey),
  ok.

hash(Binary) ->
  gen_server:call(?MODULE, {hash, Binary}).


%% Helpers
encode(<<>>, _, Acc) -> Acc;
encode(Input, <<>>, Acc) -> encode(Input, get_key(), Acc);

encode(<<In:8, RestIn/binary>>, <<Key:8, RestKey/binary>>, Acc) ->
  encode(RestIn, RestKey, <<Acc/bytes, (In bxor Key):8>>).

get_hash_size() ->
  {ok, Size} = application:get_env(my_crypt, hash_size),
  Size.

grow_binary_not_shorter(Binary, Length) when
  byte_size(Binary) < Length ->
    Size = byte_size(Binary),
    NextBinary =
      case Length - Size > Size of
        true  -> Binary;
        false -> binary:part(Binary, {0, Length - Size})
      end,
    grow_binary_not_shorter(<<Binary/binary, NextBinary/binary>>, Length);
grow_binary_not_shorter(Binary, _Length) -> Binary.

% fold_pearson(<<>>, Acc) -> Acc;
% fold_pearson(<<In:8, Rest/binary>>, <<>>) ->
%   fold_pearson(Rest, <<In:8>>);
% fold_pearson(<<In:8, Rest/binary>>, <<Acc:8>>) ->
%   fold_pearson(Rest, <<(Acc bxor In):8>>).

hash_pearson(<<>>, Acc, _Size) -> Acc;
hash_pearson(Rest, Acc, Size) ->
  First = binary:part(Rest, {0, Size}),
  Length = byte_size(Rest),
  Next =
    case Length > 2 * Size of
      true -> binary:part(Rest, {Size, Length-Size});
      false -> <<>>
    end,
  NextAcc =
    case Acc of
      <<>> -> First;
      _    -> encode(Acc, First, <<>>)
    end,
  hash_pearson(Next, NextAcc, Size).


%% sync
handle_call({encode, Input}, _From, State) ->
  {reply, encode(Input, get_key(), <<>>), State};

handle_call({hash, Binary}, _From, State) ->
  Size = get_hash_size(),
  NewBinary = grow_binary_not_shorter(Binary, 2 * Size),
  {reply, hash_pearson(NewBinary, <<>>, Size), State}.

%% async
handle_cast(_, State) ->
  {noreply, State}.


%%  plugs
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) -> ok.