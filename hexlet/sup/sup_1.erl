-module(sup_1).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, sup_1}, sup_1, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => worker1,
           start => {worker, start_link, [worker_1]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [worker]},
          #{id => worker2,
           start => {worker, start_link, [worker_2]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [worker]}
        ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.