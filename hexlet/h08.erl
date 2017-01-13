-module(h08).
-compile(export_all).

% -export([test_map/0, listen/2]). % need to export listen/2 explicitly to be able to spawn

test() ->
  test_messages("test1, empty", []),

  test_messages("test2, одно сообщение, матчится",
                [{msg, 1}]),

  test_messages("test3, одно сообщение, не матчится",
                [msg1]),

  test_messages("test4, 3 сообщения, все матчатся",
                [{msg, 1}, {msg, 2}, {msg, 3}]),

  test_messages("test5, 3 сообщения, все не матчатся",
                [msg1, msg2, msg3]),

  test_messages("test6, 4 сообщения, часть матчится, часть не матчится",
                [{msg, 1}, msg2, {msg, 3}, msg4]),

  test_messages("test7, 4 сообщения, часть матчится, часть не матчится",
                [msg1, {msg, 2}, msg3, {msg, 4}]),

  ok.


test_messages(TestName, Messages) ->
  io:format("~n### ~ts~ntest_messages: ~p~n", [TestName, Messages]),
  flush(),
  [self() ! Msg || Msg <-Messages],

  io:format("call receive~n"),
  Res =
    receive
      {msg, M} -> {msg, M}
    after 100 -> timeout
    end,
  io:format("after receive: ~p~n", [Res]),
  [{messages, Left}] = process_info(self(), [messages]),
  io:format("left in mailbox: ~p~n", [Left]),
  ok.

flush() ->
  receive
    _ -> flush()
  after 100 -> ok
  end.





listen(Pid, FileNames) ->
  Words = lists:foldl(fun get_words/2, [], FileNames),
  Pid ! {ok, Words}.


get_words(File, Acc) ->
  Str = file:read_file(File),
  case Str of
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      Acc;
    {ok, Binary} ->
      Words =
        lists:map(
          fun unicode:characters_to_list/1,
          binary:split(Binary, [<<" ">>, <<"\n">>, <<"\r">>], [global, trim])
        ),
      Acc ++ Words
  end.


count_words(Word, Acc) ->
  case maps:find(Word, Acc) of
    {ok, Value} ->
      maps:put(Word, Value+1, Acc);
    error ->
      maps:put(Word, 1, Acc)
  end.


map_reduce(FileList) ->
  spawn(?MODULE, listen, [self(), FileList]),
  receive
    {ok, Words} ->
    % io:format("~p~n", [Words]),
      lists:foldl(fun count_words/2, maps:new(), Words)
  after 5000 -> {timeout, maps:new()}

  end.




test_map() -> map_reduce(["data1.txt"
  , "data2.txt", "data3.txt"
  ]).