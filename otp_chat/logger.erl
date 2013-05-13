-module(logger).
-behaviour(gen_event).

% public exports
-export([start/1]).

% gen_event exports
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
		file :: file:io_device()
	}).

start(Filename) ->
	gen_event:add_handler(chat_evt_mgr, logger, Filename).

% gen_event callbacks

init(Filename) ->
	{ok, File} = file:open(Filename, [append]),
	{ok, #state{file = File}}.

handle_event({chat_msg, Msg}, State = #state{file = File}) ->
	{{Y,Mo,D}, {H,Mi,S}} = erlang:localtime(),
	Timestamp = io_lib:fwrite("~-4..0B-~-2..0B-~-2..0B ~-2..0B:~-2..0B:~-2..0B ", [Y, Mo, D, H, Mi, S]),
	file:write(File, Timestamp ++ Msg),
	{ok, State};

handle_event(_Event, State) -> {ok, State}.

handle_call(_Msg, State) -> {ok, {error, badcall}, State}.

handle_info(_Msg, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
