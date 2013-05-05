-module(chat_client).
-behaviour(gen_server).

% Public exports
-export([sup_spec/0, start_link/0, send/2]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
		socket = undefined,
		read = []
	}).

%% Public functions

sup_spec() ->
	{
		client,                       % ID
		{?MODULE, start_link, []},    % Start spec MFA
		temporary,                    % Restart type
		1000,                         % Shutdown timeout
		worker,                       % Child type
		[?MODULE]                     % Child modules
	}.

start_link() ->
	gen_server:start_link(?MODULE, [], []).

send(Pid, Message) ->
	gen_server:cast(Pid, {send, Message}).

%% gen_server callbacks

init(_) ->
	{ok, #state{}}.

handle_call(_Msg, _From, State) -> {reply, {error, badcall}, State}.

handle_cast({send, Message}, State = #state{socket = Socket}) ->
	gen_tcp:send(Socket, Message),
	{noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp_error, Socket, Reason}, State = #state{socket = Socket}) ->
	io:fwrite("~p Socket error: ~p\n", [self(), Reason]),
	{stop, normal, State};

handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
	io:fwrite("~p Socket closed\n", [self()]),
	{stop, normal, State};

handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
	chat:broadcast(Data),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State};

handle_info({transfer_socket, Socket}, State = #state{socket = undefined}) ->
	io:fwrite("~p New connection established\n", [self()]),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State#state{socket = Socket}};

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
