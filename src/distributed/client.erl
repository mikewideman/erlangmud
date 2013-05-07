-module(client).
%-import(net_kernel).
-export([startConnection/1, start/1, loop/1]).

start(Node) ->
    spawn(client, startConnection, [Node]).

startConnection(Node) ->
    net_kernel:connect_node(Node),
    loop({Node}).

loop(State) ->
    receive
	Anything ->
	    io:format("~p~n", [Anything]),
	    loop(State)
    end.
