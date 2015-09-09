-module(chat_hub).
-export([start/0]).

start() ->
	register(chat_hub, self()),
	loop([]).

loop(Clients) ->
	receive
		{new_client, Pid} ->
			erlang:monitor(process, Pid),
			loop([Pid | Clients]);
		{'DOWN', _Ref, process, Pid, _Info} ->
			loop(Clients -- [Pid]);
		{broadcast, From, "/local " ++ Msg} ->
			Msg = "LocalEcho: " ++ Msg, % Oops!
			From ! {send, Msg};
		{broadcast, From, Msg} ->
			[Pid ! {send, Msg} || Pid <- Clients, Pid =/= From],
			loop(Clients);
		shutdown ->
			[Pid ! shutdown || Pid <- Clients];
			% No loop, just exit
		_ ->
			% Eat other messages that might arrive
			loop(Clients)
	end.
