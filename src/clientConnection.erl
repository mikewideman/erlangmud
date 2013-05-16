-module(clientConnection).
-export([startConnection/3, connect/2, loop/4, sendMessage/2]).

connect(Server, Username) ->
    ClientPid = spawn(clientConnection, startConnection, [Server, Username, self()]),
    receive 
	{connected} -> 
    	    register(client, ClientPid),
    	    {ok, ClientPid};
	_ ->
	    {failed}
    end.
    
startConnection(ServerAddr, Username, StartedFrom) ->
    Success = net_kernel:connect_node(ServerAddr),
    case Success of
	true -> 
	    StartedFrom ! {connected},
	    Server = {server, ServerAddr},
	    Server ! {client, connect, self(), Username},
	    loop(Server, Username, 0, false),
	    {ok};
	_ -> 
	    StartedFrom ! {failed},
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
		Msg = {client, perform_action, self(), Message},
		Server ! Msg,
		loop(Server, Username, ClientPid,ConnectedToOutput);
	{ connect_ui, Pid } ->
		loop(Server, Username, Pid, true);
	Any ->
		%TODO: only do this if ClientPid is real
		ClientPid ! Any,
		loop(Server, Username,ClientPid,ConnectedToOutput)
	end.
