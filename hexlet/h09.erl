-module(h09).

-compile(export_all).


% start_test() ->
%   InitialState = [],
%   spawn(?MODULE, loop, [InitialState]).

% loop_test(State) ->
%   receive
%     {{add, Item}, From, Ref} ->
%       NewState = [Item | State],
%       From ! {reply, Ref, ok},
%       ?MODULE:loop(NewState);
%     {{remove, Item}, From, Ref} ->
%       {Reply, NewState} =
%         case lists:member(Item, State) of
%           true  -> {ok, lists:delete(Item, State)};
%           false -> {{error, not_exists}, State}
%         end,
%       From ! {reply, Ref, Reply},
%       ?MODULE:loop(NewState);
%     {show_items, From, Ref} ->
%       From ! {reply, Ref, State},
%       ?MODULE:loop(State);
%     stop -> io:format("~p stoped ~n", [self()]);
%     _    -> ?MODULE:loop(State)
%   end.

% add_item(Pid, Item) ->
%   call(Pid, {add, Item}).

% remove_item(Pid, Item) ->
%   call(Pid, {remove, Item}).

% show_items(Pid) ->
%   call(Pid, show_items).

% stop(Pid) ->
%   Pid ! stop,
%   ok.

% call(Pid, Msg) ->
%   MRef = erlang:monitor(process, Pid),
%   Pid ! {Msg, self(), MRef},
%   handle_reply(MRef).

% handle_reply(MRef) ->
%   receive
%     {reply, MRef, Reply} ->
%       erlang:demonitor(MRef, [flush]),
%       Reply;
%     {'DOWN', MRef, _, _, Reason} -> {error, Reason}
%   after 5000 ->
%     erlang:demonitor(MRef, [flush]),
%     timeout
%   end.





-type(server() :: pid()).
-type(room_id() :: reference()).
-type(name() :: binary()).
-type(message() :: {name(), binary()}).


-spec start() -> server().
start() ->
  InitialState = maps:new(),
  spawn(?MODULE, loop, [InitialState]).

call(Pid, Msg) ->
  MRef = erlang:monitor(process, Pid),
  Pid ! {Msg, self(), MRef},
  handle_reply(MRef).

handle_reply(MRef) ->
  receive
    {reply, MRef, Reply} ->
      erlang:demonitor(MRef, [flush]),
      Reply;
    {'DOWN', MRef, _, _, Reason} -> {error, Reason};
    Rep -> Rep
  after 5000 ->
    erlang:demonitor(MRef, [flush]),
    timeout
  end.

loop(Rooms) ->
  receive
    {{create_room, Name}, From, Ref} ->
      {Reply, NewState} = insert_room(Name, Rooms),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(NewState);
    {{remove_room, Id}, From, Ref} ->
      {Reply, NewState} = delete_room(Id, Rooms),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(NewState);
    {get_rooms, From, Ref} ->
      From ! {reply, Ref,
        lists:map(fun({Key, {Name, _, _}}) -> {Key, Name} end, maps:to_list(Rooms))
      },
      ?MODULE:loop(Rooms);
    {{add_user, RoomId, UserName}, From, Ref} ->
      {Reply, NewState} = create_user(RoomId, UserName, Rooms),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(NewState);
    {{get_users, RoomId}, From, Ref} ->
      Reply = case maps:find(RoomId, Rooms) of
        {ok, {_, Users, _}} -> {ok, lists:reverse(maps:keys(Users))};
        error -> {error, room_not_found}
      end,
      From ! {reply, Ref, Reply},
      ?MODULE:loop(Rooms);
    {{remove_user, RoomId, UserName}, From, Ref} ->
      {Reply, NewState} = delete_user(RoomId, UserName, Rooms),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(NewState);
    {{send_message, RoomId, UserName, Message}, From, Ref} ->
      {Reply, NewState} = post_message(RoomId, UserName, Message, Rooms),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(NewState);
    {{get_messages, RoomId}, From, Ref} ->
      Reply = get_messages(RoomId, Rooms),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(Rooms);
    Er    ->
      io:format("~p~n", [Er]),
      ?MODULE:loop(Rooms)
  end.

get_messages(RoomId, Rooms) ->
  case maps:find(RoomId, Rooms) of
    {ok, {_, _, Messages}} -> {ok, Messages};
    error -> {error, room_not_found}
  end.

post_message(RoomId, UserName, Message, Rooms) ->
  case maps:find(RoomId, Rooms) of
    {ok, {RoomName, Users, Messages}} ->
      case maps:find(UserName, Users) of
        {ok, ok} ->
          {ok, maps:put(RoomId, {RoomName, Users, [{UserName, Message} |Messages]}, Rooms)};
        error -> {{error, user_not_in_room}, Rooms}
      end;
    error -> {{error, room_not_found}, Rooms}
  end.

delete_user(RoomId, UserName, Rooms) ->
  case maps:find(RoomId, Rooms) of
    {ok, {RoomName, Users, Messages}} ->
      case maps:find(UserName, Users) of
        {ok, ok} ->
          {ok, maps:put(RoomId, {RoomName, maps:remove(UserName, Users), Messages}, Rooms)};
        error -> {{error, user_not_in_room}, Rooms}
      end;
    error -> {{error, room_not_found}, Rooms}
  end.


insert_room(Name, Rooms) ->
  RoomNames = maps:values(Rooms),
  if length(RoomNames) < 5 ->
    Id = make_ref(),
    {{ok, Id}, maps:put(Id, {Name, maps:new(), []}, Rooms)};
  true -> {{error, room_limit}, Rooms}
  end.

delete_room(Id, Rooms) ->
  case maps:find(Id, Rooms) of
    {ok, _} -> {ok, maps:remove(Id, Rooms)};
    error -> {{error, room_not_found}, Rooms}
  end.

create_user(RoomId, UserName, Rooms) ->
  case maps:find(RoomId, Rooms) of
    {ok, {Name, Users, Messages}} ->
      {ok, maps:put(RoomId, {Name, maps:put(UserName, ok, Users), Messages}, Rooms)};
    error   -> {{error, room_not_found}, Rooms}
  end.


-spec create_room(server(), name()) -> {ok, room_id()} | {error, term()}.
create_room(Server, RoomName) ->
  call(Server, {create_room, RoomName}).


-spec remove_room(server(), room_id()) -> ok | {error, term()}.
remove_room(Server, RoomId) ->
  call(Server, {remove_room, RoomId}).


-spec get_rooms(server()) -> [{room_id(), name()}].
get_rooms(Server) ->
  call(Server, get_rooms).


-spec add_user(server(), room_id(), name()) -> ok | {error, term()}.
add_user(Server, RoomId, UserName) ->
  call(Server, {add_user, RoomId, UserName}).


-spec remove_user(server(), room_id(), name()) -> ok | {error, term()}.
remove_user(Server, RoomId, UserName) ->
  call(Server, {remove_user, RoomId, UserName}).


-spec get_users_list(server(), room_id()) -> {ok, [name()]} | {error, term()}.
get_users_list(Server, RoomId) ->
  call(Server, {get_users, RoomId}).


-spec send_message(server(), room_id(), name(), binary()) -> ok.
send_message(Server, RoomId, UserName, Message) ->
  call(Server, {send_message, RoomId, UserName, Message}).


-spec get_messages_history(server(), room_id()) -> [message()].
get_messages_history(Server, RoomId) ->
  call(Server, {get_messages, RoomId}).

