-module(server).
-behaviour(application).
-export([start/2, stop/1, loop/1]).

start(StartType, StartArgs) ->
    serverPid = spawn(server, loop, [StartArgs]),
    register(server, serverPid),
    {ok, serverPid, []}.

    % TODO: Remove
    %register(server, self()),    
   
% TODO: Figure out if anything more needs to happen here
stop(State) ->
    {ok}.

loop(State) ->
    receive 
	{Source, _Anything} -> 
	    Source ! _Anything,
	    loop(State)
    end.
	    
