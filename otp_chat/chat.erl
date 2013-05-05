-module(chat).
-behaviour(gen_server).

% Public exports
-export([sup_spec/0, start_link/0, broadcast/1]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
		messages_sent = 0 :: non_neg_integer()
	}).

%% Public functions

sup_spec() ->
	{
		?MODULE,                                              % ID
		{?MODULE, start_link, []},                       % Start spec MFA
		permanent,                                       % Restart type
		1000,                                            % Shutdown timeout
		worker,                                          % Child type
		[?MODULE]                                        % Child modules
	}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, [], []).

-spec broadcast(string()) -> ok.
broadcast(Message) ->
	gen_server:cast(?MODULE, {broadcast, Message, self()}).

%% gen_server callbacks

init(_) -> {ok, #state{}}.

handle_call(_Msg, _From, State) -> {reply, {error, badcall}, State}.

handle_cast({broadcast, Message, From}, State = #state{messages_sent = Sent}) ->
	Children = supervisor:which_children(client_sup),
	[chat_client:send(P, Message) || {_, P, _, _} <- Children, is_pid(P), P =/= From],
	{noreply, State#state{messages_sent = Sent+1}};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
