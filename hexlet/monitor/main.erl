-module(main).

-export([parse/1, start/1]).


parse(Files) ->
  Pids = [start(FileName) || FileName <- Files],
  % [start({ReaderPid, FileName}) || {ReaderPid, FileName} <- Readers],
  listen_readers(maps:new(), maps:new(), Pids).

start(FileName) ->
  ReaderPid = spawn(worker, start_file_reader, [FileName, self()]),
  erlang:monitor(process, ReaderPid),
  ReaderPid ! {read, FileName},
  {ReaderPid, FileName}.

listen_readers(Data, Errors, []) -> {Data, Errors};
listen_readers(Data, Errors, Pids) ->
  receive
    {data, NewData} ->
      UpdatedData = update_map(Data, maps:to_list(NewData)),
      listen_readers(UpdatedData, Errors, Pids);
    {'DOWN', Reference, process, Pid, Info} ->
      erlang:demonitor(Reference, [flush]),
      NewPids = lists:keydelete(Pid, 1, Pids),
      case Info of
        normal ->
          listen_readers(Data, Errors, NewPids);
        _ ->
          {_, FileName} = lists:keyfind(Pid, 1, Pids),
          listen_readers(Data, maps:put(FileName, Info, Errors), NewPids)
      end;
    Any ->
      % don't expect anything else, but who knows
      io:format("any~p~n", [Any])
  after 5000 ->
    io:format("timeout~n")
  end.

% main:parse(["data_1.csv", "data_2.csv", "data_3.csv"]).
% main:parse(["data_1.csv"]).


update_map(State, []) -> State;
update_map(State, [{Key, Value} | Items]) ->
  update_map(
    worker:add_count(State, Key, Value),
    Items
  ).