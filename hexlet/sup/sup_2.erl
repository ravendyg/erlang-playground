-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
    supervisor:start_link({local, sup_2}, sup_2, []).

add_worker(WorkerRef) ->
  {ok, Child} = supervisor:start_child(
    sup_2,
    {WorkerRef,
     {worker, start_link, [WorkerRef]},
      permanent,
      2000,
      worker,
    [worker]}),
  {ok, Child}.

remove_worker(WorkerRef) ->
  supervisor:terminate_child(sup_2, WorkerRef),
  supervisor:delete_child(sup_2, WorkerRef).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    Worker1 = spec(worker_1),
    Worker2 = spec(worker_2),
    {ok, {SupervisorSpecification, [Worker1, Worker2]}}.

spec(WorkerId) ->
  #{id => WorkerId,
    start => {worker, start_link, [WorkerId]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [worker]
  }.