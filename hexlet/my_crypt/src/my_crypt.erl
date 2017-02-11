-module(my_crypt).

-behaviour(application).

-export([start/2, stop/1]).
-export([encode/1, get_key/0, set_key/1, hash/1]).


start(_StartType, _StartArgs) ->
  my_crypt_sup:start_link().

stop(_) -> ok.


encode(Input) ->
  my_crypt_worker:encode(Input).

get_key() ->
  my_crypt_worker:get_key().

set_key(NewKey) ->
  my_crypt_worker:set_key(NewKey).

hash(Binary) ->
  my_crypt_worker:hash(Binary).