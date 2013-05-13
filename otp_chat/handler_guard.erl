%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc This module is responsible for installing event handler, and reinstalling it should it fail.
% Since we can't directly supervise a gen_event module (since it doesn't have its own process)
% we instead create this watcher process and supervise it.  When it starts up, it tells the
% gen_event handler to install itself in 'supervised' mode, meaning it receives a message if
% the handler is ever removed.  When this process is informed that the event handler has been
% removed due to an error, <i>this</i> process terminates.  By in turn attaching this process to a 
% supervisor, we get all the nice supervisor features on an event handler.
%
% The gen_event module must implement a function `start' which is called to add it as a handler.
% The arguments to `start' will be the `Args' passed to `handler_guard_spec'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(handler_guard).

-export([sup_spec/3]).
-export([start_link/3]).

% gen_server's callbacks
-export([init/1, handle_info/2, code_change/3, handle_call/3, handle_cast/2, terminate/2]).

-behaviour(gen_server).

% @doc Convenience function to generate a {@module} supervisor spec. Returns a supervisor spec that can be used
% to insert a {@module} instance into a supervisor.
% @param Handler The `gen_event' event handler module.
% @param RegisteredName The registered name to give this instance
% @param Args Arguments to be passed to `Handler' upon initialisation
sup_spec(Handler, RegisteredName, Args) ->
	{ 	handler_guard,									% Id       = internal id
		{handler_guard, start_link, [Handler, RegisteredName, Args]},% StartFun = {M, F, A}
		permanent,                                      % Restart  = permanent | transient | temporary
		2000,                                       	% Shutdown = brutal_kill | int() >= 0 | infinity
		worker,                                        	% Type     = worker | supervisor
		[handler_guard]                                 % Modules  = [Module] | dynamic
	}.

-record(state, {
		handler_module,
		reg_name
	}).

% @doc Start up a {@module} instance.
% @param Handler The `gen_event' event handler module.
% @param RegisteredName The registered name to give this instance
% @param Args Arguments to be passed to `Handler' upon initialisation
% @return `{ok, Pid}'
start_link(Handler, RegisteredName, Args) ->
	gen_server:start_link({local, RegisteredName}, ?MODULE, [Handler, RegisteredName, Args], []).

%% gen_event callbacks
% @private
% Initialisation - ask the handler module to install itself.
init([HandlerModule, RegisteredName, Args]) ->
	% Call the new event handler's startup procedure
	case catch apply(HandlerModule, start, Args) of
		ok ->
			process_flag(trap_exit, true), %% required for terminate handler
			{ok, #state{handler_module = HandlerModule, reg_name = RegisteredName}};
		already_started ->
			{stop, {already_started, HandlerModule}};
		Error ->
			{stop, Error}
	end.

% @private
% gen_event manager sends this message if a handler was added using
% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
% When we receive it, kill off this server process so that our supervisor
% restarts us (provided we haven't hit any restart limits) and causes
% the handler to be re-installed.
handle_info({gen_event_EXIT, HandlerModule, Reason}, 
		State = #state{handler_module = HandlerModule, reg_name = RegisteredName}) ->
		%% Unregister ASAP to keep the number of lost messages to a minimum.
		unregister(RegisteredName),
		io:fwrite("Handler gone, exiting for restart.\n"),
		HandlerModule:handle_crash(Reason),
		io:fwrite("~w: detected handler ~p shutdown:~n~p~n", [?MODULE, HandlerModule, Reason]),
		{stop, {handler_died, HandlerModule, Reason}, State};

	% We don't handle any other messages
	handle_info(_Msg, State) -> {noreply, State}.

	% @private
	% We don't handle any calls
	handle_call(_Msg, _From, State) -> {reply, {error, badcall}, State}.

	% @private
	% We don't handle any casts
	handle_cast(_Msg, State) -> {noreply, State}.

	% @private
	% Shutdown callback
	terminate(Reason, #state{handler_module = HandlerModule, reg_name = RegisteredName}) ->
		catch unregister(RegisteredName),
		case Reason of
			shutdown -> ok;
			_ -> io:fwrite("~w: Terminated with reason: ~p\n", [?MODULE, Reason])
		end,
		HandlerModule:stop().

	% @private
	% Code update callback
	code_change(_Old, State, _Extra) ->
		{ok, State}.
