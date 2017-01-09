-module(evserv).

-export([loop/1, start/0, start_link/0, init/0, subscribe/1, terminate/0,
        add_event/3, cancel/1, listen/1
]).

-record(state, {events, clients}).

-record(event, {
          pid,
          name = "",
          description="",
          timeout = 0
        }).


loop(State = #state{}) ->
  receive
    {{subscribe, Client}, Pid, MsgRef} ->
      Ref = erlang:monitor(process, Client),
      NewClients = maps:put(Ref, Client, State#state.clients),
      Pid ! {MsgRef, ok},
      loop(State#state{clients = NewClients});

    {{add, Name, Description, Delay}, Pid, Ref} ->
      EventPid = event:start_link(Name, Delay),
  io:format("~p~n", [EventPid]),
      NewEvents = maps:put(Name, #event{
                                  pid=EventPid,
                                  name=Name,
                                  description=Description,
                                  timeout=Delay
                                }, State#state.events),
      Pid ! {Ref, ok},
      loop(State#state{events=NewEvents});

    {{cancel, Name}, Pid, Ref} ->
      NewEvents = case maps:find(Name, State#state.events) of
        {ok, Event} ->
          event:cancel(Event#event.pid),
          maps:remove(Name, State#state.events);
        error -> State#state.events
      end,
      Pid ! {Ref, ok},
      loop(State#state{events=NewEvents});

    {done, Name} ->
      NewEvents = case maps:find(Name, State#state.events) of
        {ok, E} ->
io:format("~p~n", [E]),
          send_to_clients({done, E#event.name, E#event.description}, State#state.clients),
          maps:remove(Name, State#state.events);
        error -> State#state.events
      end,
      loop(State#state{events=NewEvents});

    shutdown ->
      exit(shutdown);

    {'DOWN', Ref, process, _Pid, _Reason} ->
      NewClients = maps:remove(Ref, State#state.clients),
      loop(State#state{clients=NewClients});

    code_change ->
      ?MODULE:loop(State);

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(State)
  end.


init() ->
  loop(#state{
        events = maps:new(),
        clients = maps:new()
      }).


start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.

terminate() ->
  ?MODULE ! shutdown.


send_to_clients(Msg, Clients) ->
  [Pid ! Msg || Pid <- maps:values(Clients)].

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
%   Q = ?MODULE,
% io:format("~p~n", [Pid, Ref, Q]),
  ?MODULE ! {{subscribe, Pid}, self(), Ref},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, Timeout) ->
  Ref = make_ref(),
  ?MODULE ! {{add, Name, Description, Timeout}, self(), Ref},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE !{{cancel, Name}, self(), Ref},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.


listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay * 1000 ->
    []
  end.