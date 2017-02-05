-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% BEGIN (write your solution here)
start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) -> {ok, {[], []}}.


%% API
%% sync
remove_user(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid) ->
    gen_server:call(RoomPid, get_users).

get_history(RoomPid) ->
    gen_server:call(RoomPid, get_history).


%% async
add_user(RoomPid, UserName, UserPid) ->
    gen_server:cast(RoomPid, {add_user, UserName, UserPid}),
    ok.

add_message(RoomPid, Author, Text) ->
    gen_server:cast(RoomPid, {add_message, Author, Text}),
    ok.


%% callbacks
handle_call({remove_user, UserPid}, _From, {Users, Messages}) ->
    case lists:keyfind(UserPid, 2, Users) of
    % case maps:is_key(UserPid, Users) of
        {_,_} ->
            {reply, ok, {
                [{Name, Pid} || {Name, Pid} <- Users, Pid =/= UserPid],
                Messages
            }};
        false ->
            {reply, {error, user_not_found}, {Users, Messages}}
    end;

handle_call(get_users, _From, {Users, Messages}) ->
    {reply, Users, {Users, Messages}};

handle_call(get_history, _From, {Users, Messages}) ->
    {reply, lists:reverse(Messages), {Users, Messages}}.



handle_cast({add_user, UserName, UserPid}, {Users, Messages}) ->
    {noreply, {[{UserName, UserPid} | Users], Messages}};

handle_cast({add_message, Author, Message}, {Users, Messages}) ->
    [chat_user:add_message(UserPid, Author, Message) || {_, UserPid} <- Users],
    {noreply, {Users, [{Author, Message} | Messages]}}.



handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) -> ok.

%% END
