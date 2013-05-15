-module(clientConnection).
-export([startConnection/2, connect/2, loop/2, sendMessage/2]).

connect(Server, Username) ->
    ClientPid = spawn(clientConnection, startConnection, [Server, Username]),
    register(client, ClientPid),
    {ok, ClientPid}.
    
startConnection(ServerAddr, Username) ->
    Success = net_kernel:connect_node(ServerAddr),
    case Success of
	true -> 
	    Server = {server, ServerAddr},
	    Server ! {client, connect, self(), Username},
	    loop(Server, Username),
	    {ok};
	_ -> 
	    {fail, "Unable to connect to server " ++ ServerAddr}
    end.

sendMessage(_DestUsername, _Message) ->
    client ! {send_message, _DestUsername, _Message}.

loop(Server, Username) ->
    receive
	{send_message, DestUsername, _Message} ->
	    Server ! {client, send_message, Username, DestUsername, _Message},
	    loop(Server, Username);
	{ send_input, Message } ->
		Server ! {client, perform_action, self(), Message};
	_Anything ->
	    io:format("Received message: ~p~n", [_Anything]),
	    loop(Server, Username)
    end.
