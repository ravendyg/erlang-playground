-module(t_u).

-compile(export_all).

start() -> start(1234).

start(Port) ->
  spawn(?MODULE, server, [Port])
  ,
  ok
  .

server(Port) ->
  io:format("server on port ~p~n", [Port]),
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
  [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
  receive
    stop -> gen_tcp:close(ListenSocket)
  end,
  ok.

accept(Id, ListenSocket) ->
  io:format("Socket #~p wait for client~n", [Id]),
  {ok, _Socket} = gen_tcp:accept(ListenSocket),
  io:format("Socket #~p, session started~n", [Id]),
  handle_connection(Id, ListenSocket).

handle_connection(Id, ListenSocket) ->
  handle_connection(Id, ListenSocket, null).

handle_connection(Id, ListenSocket, Connection)->
  receive
    {tcp, Socket, Msg} ->
      io:format("Socket #~p got message: ~p~n", [Id, binary:split(Msg, [<<"\r\n">>], [global])]),
      gen_tcp:send(Socket, Msg),
      handle_connection(Id, ListenSocket, Socket);
    {tcp_closed, _Socket} ->
      io:format("Socket #~p, session closed ~n", [Id]),
      accept(Id, ListenSocket)
    after 5000 ->
      case Connection of
        null ->
          handle_connection(Id, ListenSocket, null);
        _    ->
          gen_tcp:send(Connection, ""),
          gen_tcp:close(Connection),
          handle_connection(Id, ListenSocket, null)
      end
    end.

% stop(Pid) ->
%   Pid ! close.