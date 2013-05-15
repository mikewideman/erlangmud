%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Client module
% Outputs game messages to the console.
% Also watches for keyboard input, passes
% to the parser, and sends to the connection manager.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(client).
-export( [start/0, outputloop/0, inputloop/2 ] ).


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

inputloop(Pid, Username) ->
	String = io:get_line( "$" ),
	Pid ! { Username, parser:parse(String) },
	inputloop(Pid, Username).

getUserInfo() ->
	Uname = string:strip(io:get_line( "Enter username:" ) ),
	Server = list_to_atom( string:strip( io:get_line( "Enter server node:"), both, $\n )  ),
	{Uname, Server}.

start() ->
	{Uname, Server} = getUserInfo(),
	io:format(" Connecting to server ~p ~n", [Server] ),
	Outpid = spawn( ui, outputloop, [] ),
	inputloop(Outpid, Uname).
