-module(chat).

-export([start/0, chat_hub/1]).

-define(DEFAULT_CHAT_PORT, 2000).

start() ->
	Handler = spawn(?MODULE, chat_hub, [[]]),
	register(chat_hub, Handler),
	{ok, Socket} = gen_tcp:listen(2000, [list, {active, false}, {reuseaddr, true}]),
	await_connection(Socket).

await_connection(Socket) ->
	{ok, Connection} = gen_tcp:accept(Socket),
	Client = spawn(chat_client, start, []),
	gen_tcp:controlling_process(Connection, Client),
	Client ! {socket, Connection},
	await_connection(Socket).

chat_hub(Clients) ->
	receive
		{new_client, Pid} ->
			erlang:monitor(process, Pid),
			chat_hub([Pid | Clients]);
		{'DOWN', _Ref, process, Pid, _Info} ->
			chat_hub(Clients -- [Pid]);
		{broadcast, From, Msg} ->
			[Pid ! {send, Msg} || Pid <- Clients, Pid =/= From],
			chat_hub(Clients);
		_ ->
			% Eat other messages that might arrive
			chat_hub(Clients)
	end.
