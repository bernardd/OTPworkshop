-module(blank_gen_server).
-behaviour(gen_server).

% Public exports
-export([start_link/0]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

% Public functions

start_link() ->
	gen_server:start_link({local, ?MODULE}, [], []).

% gen_server callbacks

init(_) -> {ok, #state{}}.

handle_call(_Msg, _From, State) -> {reply, {error, badcall}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
