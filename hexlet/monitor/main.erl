-module(main).

-export([parse/1, start/1]).


parse(Files) ->
  [start(FileName) || FileName <- Files],
  % [start({ReaderPid, FileName}) || {ReaderPid, FileName} <- Readers],
  listen_readers(maps:new(), length(Files)).

start(FileName) ->
  ReaderPid = spawn(worker, start_file_reader, [self()]),
  erlang:monitor(process, ReaderPid),
  ReaderPid ! {read, FileName}.

listen_readers(Data, ReadersLeft) when ReadersLeft > 0 ->
  io:format("listen ~p~n", [ReadersLeft]),
  receive
    {data, NewData} ->
      UpdatedData = update_map(Data, maps:to_list(NewData)),
      io:format("~p~n", [[ReadersLeft, NewData]]),
      listen_readers(UpdatedData, ReadersLeft - 1);
    {'DOWN', Reference, process, _Pid, _Info} ->
      io:format("here~n~p~n", [[_Info, _Pid, Reference]]),
      erlang:demonitor(Reference, [flush]),
      case _Info of
        normal -> listen_readers(Data, ReadersLeft);
        _ -> listen_readers(Data, ReadersLeft - 1)
      end;
    Any -> io:format("any~p~n", [Any])
  after 5000 ->
    io:format("timeout~n")
  end;
listen_readers(Data, ReadersLeft) when ReadersLeft =< 0 -> Data.

% main:parse(["data_1.csv", "data_2.csv", "data_3.csv"]).
% main:parse(["data_1.csv"]).


update_map(State, []) -> State;
update_map(State, [{Key, Value} | Items]) ->
  update_map(
    worker:add_count(State, Key, Value),
    Items
  ).