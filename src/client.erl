%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Client module
% Outputs game messages to the console.
% Also watches for keyboard input, passes
% to the parser, and sends to the connection manager.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(client).
-export( [start/0, outputloop/0, inputloop/3 ] ).


outputloop() ->
	receive 
		{Username, {Verb, DirectObj }}   ->
			io:fwrite( Verb ),
			io:fwrite( DirectObj ),
			outputloop();
		{Username, {Verb} } ->
			io:fwrite( Verb ),
			outputloop()
	end.

inputloop(Pid, Username, ConnectPid) ->
	String = io:get_line( "$" ),
	ConnectPid ! {send_input, { Username, parser:parse(String) } },
	inputloop(Pid, Username, ConnectPid).

getUserInfo() ->
	Uname = string:strip(io:get_line( "Enter username:" ) ),
	Server = list_to_atom( string:strip( io:get_line( "Enter server node:"), both, $\n )  ),
	{Uname, Server}.

start() ->
	{Uname, Server} = getUserInfo(),
	io:format(" Connecting to server ~p ~n", [Server] ),
	Success = clientConnection:connect(Server, Uname),
	case Success of
	{ok, ConnectPid} ->
		Outpid = spawn( ui, outputloop, [] ),
		inputloop(Outpid, Uname, ConnectPid);
	_ ->
		io:format("Could not connect to server")
	end.
