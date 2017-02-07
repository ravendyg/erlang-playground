-module(main).

-export([parse/1, start/1]).


parse(Files) ->
  Pids = [start(FileName) || FileName <- Files],
  % [start({ReaderPid, FileName}) || {ReaderPid, FileName} <- Readers],
  listen_readers(maps:new(), maps:new(), Pids, length(Files)).

start(FileName) ->
  ReaderPid = spawn(worker, start_file_reader, [self()]),
  erlang:monitor(process, ReaderPid),
  ReaderPid ! {read, FileName},
  {ReaderPid, FileName}.

listen_readers(Data, Errors, Pids, ReadersLeft) when ReadersLeft > 0 ->
  % io:format("listen ~p~n", [ReadersLeft]),
  receive
    {data, NewData} ->
      UpdatedData = update_map(Data, maps:to_list(NewData)),
      % io:format("~p~n", [[ReadersLeft, NewData]]),
      listen_readers(UpdatedData, Errors, Pids, ReadersLeft - 1);
    {'DOWN', Reference, process, Pid, Info} ->
      % io:format("here~n~p~n", [[Info, _Pid, Reference]]),
      erlang:demonitor(Reference, [flush]),
      case Info of
        normal -> listen_readers(Data, Errors, Pids, ReadersLeft);
        _ ->
          {_, FileName} = lists:keyfind(Pid, 1, Pids),
          listen_readers(Data, maps:put(FileName, Info, Errors), Pids, ReadersLeft - 1)
      end;
    Any -> io:format("any~p~n", [Any])
  after 5000 ->
    io:format("timeout~n")
  end;
listen_readers(Data, Errors, _Pids, ReadersLeft) when ReadersLeft =< 0 -> {Data, Errors}.

% main:parse(["data_1.csv", "data_2.csv", "data_3.csv"]).
% main:parse(["data_1.csv"]).


update_map(State, []) -> State;
update_map(State, [{Key, Value} | Items]) ->
  update_map(
    worker:add_count(State, Key, Value),
    Items
  ).