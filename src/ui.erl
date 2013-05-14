%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% UI module
% Outputs game messages to the console.
% Also watches for keyboard input, passes
% to the parser, and sends to the server.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ui).
-export( [start/0, outputloop/0, inputloop/2 ] ).


outputloop() ->
	receive { X } ->
		io:fwrite(X),
		outputloop()
	end.

inputloop(Pid, Username) ->
	String = io:get_line( "$" ),
	Pid ! { Username, parser:parse(String) },
	inputloop(Pid, Username).

start() ->
	Uname = io:get_line( "Enter username:\n$" ),
	Outpid = spawn( ui, outputloop, [] ),
	inputloop(Outpid, Uname).
