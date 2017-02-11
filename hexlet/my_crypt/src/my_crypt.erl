-module(my_crypt).

-behaviour(application).

-export([start/2, stop/1]).
-export([encode/1]).


start(_StartType, _StartArgs) ->
  my_crypt_sup:start_link().

stop(_) -> ok.


encode(Input) ->
  my_crypt_worker:encode(Input).

