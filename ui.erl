%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% UI module
% Outputs game messages to the console.
% Also watches for keyboard input, passes
% to the parser, and sends to the server.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ui).
-export( [start/0, outputloop/0, inputloop/1] ).


outputloop() ->
	receive { X } ->
		io:fwrite(X),
		outputloop()
	end.

inputloop(Pid) ->
	String = io:get_line( "$" ),
	Pid ! parser:parse(String),
	inputloop(Pid).
	

start() ->
	Outpid = spawn( ui, outputloop, [] ),
	inputloop(Outpid).
