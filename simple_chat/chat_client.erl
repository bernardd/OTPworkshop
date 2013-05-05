-module(chat_client).

-export([start/0]).

start() ->
	Socket = receive
		{socket, S} -> S
	end,
	chat ! {new_client, self()},
	io:fwrite("~p New client started\n", [self()]),
	inet:setopts(Socket, [{active, once}]),
	loop(Socket, []).

loop(Socket, DataSoFar) ->
	receive
		{send, Msg} ->
			gen_tcp:send(Socket, Msg),
			loop(Socket, DataSoFar);
		{tcp_error, Socket, Reason} ->
			io:fwrite("~p Socket error: ~p\n", [self(), Reason]);
		{tcp_closed, Socket} ->
			io:fwrite("~p Socket closed\n", [self()]);
		{tcp, Socket, Data} ->
			Remaining = handle_data(DataSoFar ++ Data),
			inet:setopts(Socket, [{active, once}]),
			loop(Socket, Remaining);
		_ ->
			loop(Socket, DataSoFar)
	end.

handle_data(Str) ->
	case string:chr(Str, $\n) of
		0 -> Str; % No EOL found
		N ->
			{Line, Rest} = lists:split(N, Str),
			handle_line(Line),
			handle_data(Rest)
	end.

handle_line(Line) ->
	chat ! {broadcast, self(), Line}.
