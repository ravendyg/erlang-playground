-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% BEGIN (write your solution here)
start_link() -> gen_server:start_link(?MODULE, [], []).


%% API
add_message(UserPid, Author, Text) ->
    gen_server:cast(UserPid, {add_message, Author, Text}),
    ok.


handle_cast({add_message, Author, Text}, Messages) ->
    {no_reply, [{Author, Text}, Messages]}.
%% END
