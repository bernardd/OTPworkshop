%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc An asynchronous TCP server, implemented as a `gen_server'.
% I've ripped this code verbatim from <a href="http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles">
% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles</a>
% (where you can also find a complete writeup of it) because it pretty much does exactly what we want in terms of a 
% non-blocking TCP server.
%
% The callback function supplied to `start_link' must return `{ok, Pid}' where `Pid' is the pid to
% which control of the socket shall be transferred. That process must accept a message of the form
% `{transfer_socket, Socket}', which will be the indication that ownership of the new socket has
% been transferred to it.
%
% Also note that the socket is created with `{active, false}', meaning the receiving process 
% must call `inet:setopts(Socket, [{active, once}])' to start receiving data. (Alternatively, of course,
% it can simply use `gen_tcp:recv/[2,3]').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(async_tcp_listener).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% External API
-export([start_link/4, sup_spec/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-record(state, {
		listener,       % Listening socket
		acceptor,       % Asynchronous acceptor's internal reference
		module,         % Connection handling callback module
		function,       % Startup function for callback module
		args            % Arguments for callback function
	}).

%%--------------------------------------------------------------------
%% @doc Called by a supervisor to start the listening process. Upon establishment of a
%% new connection, this server will call
%%
%% `apply(Module, Function, Args ++ [Socket])'
%%
%% The expected return is `{ok, Pid}' where Pid is the process to which ownership of the
%% socket will be passed.
%%
%% where `Socket' is the socket reference for the connection.
%% @param Port The port on which to listen
%% @param Module The module to invoke when a new connection is established
%% @param Function The function in Module to call when a new connection is established
%% @param Args The args to provide to Function. Note that the socket will be appended
%% 		 to these arguments
%%
%% @end
%%----------------------------------------------------------------------
-spec start_link(non_neg_integer(), atom(), atom(), [term()]) -> {ok, pid()} | {error, term()}.
start_link(Port, Module, Function, Args) when is_integer(Port), is_atom(Module), is_atom(Function), is_list(Args) ->
	Name = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Port)),
	gen_server:start_link({local, Name}, ?MODULE, [Port, Module, Function, Args], []).

sup_spec(ID, Args) ->
	{
		ID,                                              % ID
		{?MODULE, start_link, Args},                     % Start spec MFA
		permanent,                                       % Restart type
		1000,                                            % Shutdown timeout
		worker,                                          % Child type
		[?MODULE]                                        % Child modules
	}.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Port, Module, Function, Args]) ->
	Opts = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opts) of
		{ok, Listen_socket} ->
			%%Create first accepting process
			{ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
			{ok, #state{listener = Listen_socket,
					acceptor = Ref,
					module   = Module,
					function = Function,
					args     = Args}};
		{error, Reason} ->
			{stop, Reason}
	end.

handle_call(_Request, _From, State) ->
	{reply, {error, badcall}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
	#state{listener=ListSock, acceptor=Ref, module=Module, function=Fun, args=Args} = State) ->
	try
		case set_sockopt(ListSock, CliSocket) of
			ok              -> ok;
			{error, Reason} -> exit({set_sockopt, Reason})
		end,

		%% New client connected - spawn a new process using the simple_one_for_one
		%% supervisor.
		{ok, Pid} = apply(Module, Fun, Args),
		gen_tcp:controlling_process(CliSocket, Pid),
		Pid ! {transfer_socket, CliSocket},

		%% Signal the network driver that we are ready to accept another connection
		case prim_inet:async_accept(ListSock, -1) of
			{ok,    NewRef} -> ok;
			{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
		end,

		{noreply, State#state{acceptor=NewRef}}
		catch exit:Why ->
			error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
			{stop, Why, State}
	end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
	error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
	{stop, Error, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok    -> ok;
				Error -> gen_tcp:close(CliSocket), Error
			end;
		Error ->
			gen_tcp:close(CliSocket), Error
	end.
