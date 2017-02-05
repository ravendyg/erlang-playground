-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% BEGIN (write your solution here)
start_link() ->
  gen_server:start_link({local, manager}, ?MODULE, [], []).

init([]) -> {ok, []}.



%%% API
create_room(RoomName) ->
    gen_server:call(manager, {create_room, RoomName}).

get_rooms() ->
    gen_server:call(manager, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(manager, {add_user, RoomPid, UserName, UserPid}).

get_history(RoomPid) ->
    gen_server:call(manager, {get_history, RoomPid}).

get_users(RoomPid) ->
    gen_server:call(manager, {get_users, RoomPid}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(manager, {remove_user, RoomPid, UserPid}).

send_message(RoomPid, UserName, Message) ->
    gen_server:call(manager, {send_message, RoomPid, UserName, Message}).



handle_call({create_room, RoomName}, _From, Rooms) ->
    {ok, RoomPid} = chat_room:start_link(),
    Room = {RoomName, RoomPid},
    {reply, Room, [Room | Rooms]};

handle_call(get_rooms, _From, Rooms) ->
    {reply, Rooms, Rooms};

handle_call({get_history, RoomPid}, _From, Rooms) ->
    Reply =
        case lists:keyfind(RoomPid, 2, Rooms) of
            {_, _} -> {ok, chat_room:get_history(RoomPid)};
            false  -> {error, room_not_found}
        end,
    {reply, Reply, Rooms};

handle_call({get_users, RoomPid}, _From, Rooms) ->
    Reply =
        case lists:keyfind(RoomPid, 2, Rooms) of
            {_, _} -> {ok, chat_room:get_users(RoomPid)};
            false  -> {error, room_not_found}
        end,
    {reply, Reply, Rooms};

handle_call({remove_user, RoomPid, UserPid}, _From, Rooms) ->
    Reply =
        case lists:keyfind(RoomPid, 2, Rooms) of
            {_, _} -> chat_room:remove_user(RoomPid, UserPid);
            false  -> {error, room_not_found}
        end,
    {reply, Reply, Rooms};


handle_call({add_user, RoomPid, UserName, UserPid}, _From, Rooms) ->
    Reply =
        case lists:keyfind(RoomPid, 2, Rooms) of
            {_RoomName, RoomPid} ->
                chat_room:add_user(RoomPid, UserName, UserPid),
                ok;
            false -> {error, room_not_found}
        end,
    {reply, Reply, Rooms};


handle_call({send_message, RoomPid, Author, Message}, _From, Rooms) ->
    Reply =
        case lists:keyfind(RoomPid, 2, Rooms) of
            {_, _} ->
                chat_room:add_message(RoomPid, Author, Message);
            false -> {error, room_not_found}
        end,
    {reply, Reply, Rooms}.
    % [chat_user:add_message(UserPid, Author, Text) || UserPid <- maps:keys(Users)],
    % {noreply, {Users, [{Author, Text} | Messages]}}.

handle_cast(_, State) -> {noreply, State}.




handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) -> ok.















