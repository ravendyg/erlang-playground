-module(h11).

% -export([run/0, run_and_crash/0, work/1, work_and_crash_one/1]).
-compile(export_all).

%% sample 1

run() ->
    [spawn_link(?MODULE, work, [Id]) || Id <- lists:seq(0, 5)],
    ok.

work(Id) ->
    io:format("~p ~p started~n", [Id, self()]),
    timer:sleep(1000),
    io:format("~p ~p stopped~n", [Id, self()]),
    ok.

run_and_crash() ->
    [spawn(?MODULE, work_and_crash_one, [Id]) || Id <- lists:seq(0, 5)],
    ok.

work_and_crash_one(Id) ->
    io:format("~p ~p started~n", [Id, self()]),
    if
        Id == 3 ->
            io:format("~p ~p exiting~n", [Id, self()]),
            exit(for_some_reason);
            % ok;
        true -> ok
    end,
    timer:sleep(1000),
    io:format("~p ~p stopped~n", [Id, self()]),
    ok.


%% sample 2

run2() ->
    spawn(fun system_process/0),
    ok.


system_process() ->
    io:format("~p system process started~n", [self()]),
    process_flag(trap_exit, true),
    spawn_link(fun worker/0),
    receive
        Msg -> io:format("~p system process got message ~p~n", [self(), Msg])
    after 2000 -> ok
    end,
    ok.


worker() ->
    io:format("~p worker started~n", [self()]),
    timer:sleep(500),
    % exit(some_reason),
    ok.