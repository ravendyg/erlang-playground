-module(h10).

-compile(export_all).


-record(state, {
          apns_host :: string(),
          apns_port :: integer(),
          connections = orddict:new() :: orddict:orddict(file:name_all(), port())
        }).


% not really understand right now how it works


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  {ok, #state{
        apns_host = application:get_env(wg_push, apns_host, "gateway.sandbox.push.apple.com"),
        apns_port = application:get_env(wg_push, apns_port, 2196)
      }}.


send_messages(Messages, SSL_Options, State) ->
  gen_server:call(?MODULE, {send_messages, Messages, SSL_Options}).


handle_call({send_messages, Messages, SSL_Options}, _From, State) ->
  {Reply, State3} = send_messages(Messages, SSL_Options, State),
  {reply, Reply, State3}.




api_1(A) ->
  gen_server:call(?MODULE, {msg1, A}).

handle_call({msg1, A}, _From, State) ->
  ...


do_smth(A, B) ->
  gen_server:cast(?MODULE, {do_smth, A, B}),
  ok.

handle_cast({do_smth, A, B}, State) ->
  NewState = ...,
  {noreply, NewState}.