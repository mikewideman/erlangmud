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
	String = parser:parse(io:get_line("$")).
	

start() ->
	spawn( ui, outputloop, [] ),
	spawn( ui, inputloop, ["Fakepid"] ).

