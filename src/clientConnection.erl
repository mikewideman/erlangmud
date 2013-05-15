-module(clientConnection).
-export([startConnection/2, connect/2, loop/4, sendMessage/2]).

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
	    loop(Server, Username, 0, false),
	    {ok};
	_ -> 
	    {fail, "Unable to connect to server " ++ ServerAddr}
    end.

sendMessage(_DestUsername, _Message) ->
    client ! {send_message, _DestUsername, _Message}.

loop(Server, Username, ClientPid, ConnectedToOutput) ->
    receive
	{send_message, DestUsername, _Message} ->
	    Server ! {client, send_message, Username, DestUsername, _Message},
	    loop(Server, Username, ClientPid,ConnectedToOutput);
	{ send_input, Message } ->
		Server ! {client, perform_action, self(), Message},
		loop(Server, Username, ClientPid,ConnectedToOutput);
	{ connect_ui, Pid } ->
		loop(Server, Username, Pid, true);
	Any ->
		%TODO: only do this if ClientPid is real
		ClientPid ! Any,
		loop(Server, Username,ClientPid,ConnectedToOutput)
	end.
