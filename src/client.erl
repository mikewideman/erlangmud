%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Client module
% Outputs game messages to the console.
% Also watches for keyboard input, passes
% to the parser, and sends to the connection manager.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(client).
-include("defs.hrl").
-export( [start/0, outputloop/0, inputloop/3 ] ).


outputloop() ->
	receive 
	{fail, GameAction} ->
		io:format(" Your action ~s failed.", [GameAction#action.verb] ),
		outputloop();
	{event, Event} ->
		io:format("New event: ~n Verb:~p~nSubject:~p~nObject:~p~nPayload:~p~n",
		  [Event#event.verb, Event#event.subject, Event#event.object, Event#event.payload]),
		outputloop()
	end.

inputloop(Pid, Username, ConnectPid) ->
	String = string:strip(io:get_line( "$" ),both, $\n ),
	if
		String == "" ->
			inputloop(Pid, Username, ConnectPid);
		String == "quit" ->
			io:format("Exiting...");
		true ->
			Tokens = parser:parse(String),
			case Tokens of
				["say", DestUser | Message ] ->
					io:format("~p ~n", [Message]),
					ConnectPid ! {send_message, DestUser, string:join(Message, " ")},
					inputloop(Pid, Username, ConnectPid);
				[Verb | DirectObject] ->
					ConnectPid ! {send_input, { Username, {Verb, string:join(DirectObject, " ")} } },
					inputloop(Pid, Username, ConnectPid);
				{Verb} ->
					ConnectPid ! {send_input, { Username, {Verb} } },
					inputloop(Pid, Username, ConnectPid)
			end
	end.

getUserInfo() ->
	Uname = string:strip(io:get_line( "Enter username:" ), both, $\n ),
	Server = list_to_atom( string:strip( io:get_line( "Enter server node:"), both, $\n )  ),
	{Uname, Server}.

start() ->
	{Uname, Server} = getUserInfo(),
	io:format(" Connecting to server ~p ~n", [Server] ),
	Success = clientConnection:connect(Server, Uname),
	case Success of
	{ok, ConnectPid} ->
		Outpid = spawn( client, outputloop, [] ),
		ConnectPid ! {connect_ui, Outpid},
		inputloop(Outpid, Uname, ConnectPid);
	_ ->
		io:format("Could not connect to server")
	end.
