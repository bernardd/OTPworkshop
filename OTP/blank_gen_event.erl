-module(blank_gen_event).
-behaviour(gen_event).

% gen_event exports
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

% gen_event callbacks

init(_) -> {ok, #state{}}.

handle_event(_Event, State) -> {ok, State}.

handle_call(_Msg, State) -> {ok, {error, badcall}, State}.

handle_info(_Msg, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
