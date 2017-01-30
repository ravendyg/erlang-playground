-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% BEGIN (write your solution here)
start_link() ->
  gen_server:start_link({manager, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.



%%% API
create_room(RoomName) ->
    gen_server:call(?MODULE, {create_room, RoomName}).

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).




handle_call({create_room, RoomName}, _From, Rooms) ->
    {ok, RoomPid} = chat_room:start_link(),
    Room = {RoomName, RoomPid},
    {reply, Room, [Room | Rooms]};

handle_call(get_room, _From, Rooms) ->
    {reply, Rooms, Rooms};

handle_call({add_user, RoomPid, UserName, UserPid}, _From, Rooms) ->
    % find room by pid
    Names = [RoomName || {RoomName, RPid} <- Rooms, RPid =:= RoomPid],
    if  length(Names) =:= 1 ->
            chat_room:add_user(RoomPid, UserName, UserPid);
        true -> {error, room_not_found}
    end.

%% END















