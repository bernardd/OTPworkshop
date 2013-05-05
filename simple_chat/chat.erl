-module(chat).

-export([start/0, chat_handler/1]).

-define(DEFAULT_CHAT_PORT, 2000).

start() ->
	Handler = spawn(?MODULE, chat_handler, [[]]),
	register(chat, Handler),
	{ok, Socket} = gen_tcp:listen(2000, [list, {active, false}, {reuseaddr, true}]),
	await_connection(Socket).

await_connection(Socket) ->
	{ok, Connection} = gen_tcp:accept(Socket),
	Client = spawn(chat_client, start, []),
	gen_tcp:controlling_process(Connection, Client),
	Client ! {socket, Connection},
	await_connection(Socket).

chat_handler(Clients) ->
	receive
		{new_client, Pid} ->
			erlang:monitor(process, Pid),
			chat_handler([Pid | Clients]);
		{'DOWN', _Ref, process, Pid, _Info} ->
			chat_handler(Clients -- [Pid]);
		{broadcast, From, Msg} ->
			[Pid ! {send, Msg} || Pid <- Clients, Pid =/= From],
			chat_handler(Clients);
		_ ->
			% Eat other messages that might arrive
			chat_handler(Clients)
	end.
