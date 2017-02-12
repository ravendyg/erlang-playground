-module(my_crypt_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-include("otp_types.hrl").

start_link() ->
  supervisor:start_link({local, my_crypt_sup}, my_crypt_sup, []).

init(_Args) ->
  SupervisorSpecification = #{
      strategy => one_for_one,
      intensity => 10,
      period => 60},

  ChildSpecifications =
      [
        #{id => my_crypt_worker,
         start => {my_crypt_worker, start_link, []},
         restart => permanent,
         shutdown => 2000,
         type => worker,
         modules => [my_crypt_worker]}
      ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.