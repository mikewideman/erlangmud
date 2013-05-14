-module(clientConnection).
-export([startConnection/1, connect/1, loop/1, sendMessage/1]).

connect(Server) ->
    ClientPid = spawn(client, startConnection, [Server]),
    register(client, ClientPid),
    {ok, ClientPid}.
    
startConnection(Server) ->
    Success = net_kernel:connect_node(Server),
    case Success of
	true -> 
	    loop([{server, Server}]),
	    {ok};
	_ -> 
	    {fail, "Unable to connect to server " ++ Server}
    end.

sendMessage(_Message) ->
    client ! {send_message, _Message}.

% The client process state has the form: [{server, ServerNode}]
% where {server, ServerNode} represents the server address to send
% messages to
loop([ServerAddr]) ->
    receive
	{send_message, _Message} ->
	    ServerAddr ! {self(), _Message},
	    loop([ServerAddr]);
	    
	_Anything ->
	    io:format("Received message: ~p~n", [_Anything]),
	    loop([ServerAddr])
    end.
