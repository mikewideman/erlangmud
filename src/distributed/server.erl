-module(server).
-export([start/0, loop/1]).

start() ->
    ServerPid = spawn(server, loop, [ [] ]),
    register(server, ServerPid),
    {ok, ServerPid}.

loop(State) ->
    receive 
	{Source, Anything} -> 
	    %Source ! _Anything,
	    io:format("Received message: ~p from ~p~n", [Anything, Source]),
	    Source ! "Hi there! " ++ Anything,
	    loop(State)
    end.
	    
