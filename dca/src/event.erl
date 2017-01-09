-module(event).

-export([loop/1, start/2, start_link/2, init/3, cancel/1]).

-record(state, {
          server,
          name="",
          to_go = 0
        }).


loop(S = #state{server = Server}) ->
  receive
    {cancel, Server, Ref} ->
      Server ! {Ref, ok}
  after S#state.to_go * 1000 ->
    Server ! {done, S#state.name}
  end.


init(Name, Server, Delay) ->
  loop(#state{
        server=Server,
        name=Name,
        to_go=Delay
      }).


start(Name, Delay) ->
  spawn(?MODULE, init, [Name, self(), Delay]).

start_link(Name, Delay) ->
  spawn_link(?MODULE, init, [Name, self(), Delay]).


cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {cancel, self(), Ref},
    receive
      {Ref, ok} ->
        erlang:demonitor(Ref, [flush]),
        ok;
      {'DOWN', Ref, process, Pid, _Reason} ->
        ok
    end.