-module(chat_app).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, start_phase/3, stop/1, config_change/3]).

start(_StartType, _StartArgs) ->
	
start_phase(Phrase, StartType, PhaseArgs) -> ok.

stop(_State) -> ok.

config_change(_Changed, _New, _Removed) -> ok.

init(main_sup) ->
	{ok, {
			one_for_one, % Strategy
			4,           % Max retries
			1000         % Max time
		},
		[ % Children
			% Client supervisor:
			{
				client_sup,                                      % ID
				{supervisor, start_link, [?MODULE, client_sup]}, % Start spec MFA
				permanent,                                       % Restart type
				1000,                                            % Shutdown timeout
				supervisor,                                      % Child type
				[?MODULE]                                        % Child modules
			},
			% Main chat server:
			chat:sup_spec(),
			% Socket listener:
			async_tcp_listener:sup_spec(client_listener, [?DEFAULT_CHAT_PORT, supervisor, start_child, [client_sup, []]]),
			% Status server:
			status_listener:sup_spec()
		]
	}.

init(client_sup) ->
	{ok, {
			simple_one_for_one, % Strategy
			0,           % Max retries - unused
			0            % Max time - unused
		},

