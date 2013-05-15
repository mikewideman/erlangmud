-module(server).
-export([start/1, shutdown/0, startLoop/1, loop/2]).

start(_CallbackPid) ->
    ServerPid = spawn(server, startLoop, [_CallbackPid]),
    register(server, ServerPid),
    {ok, ServerPid}.

startLoop(CallbackPid) ->
    process_flag(trap_exit, true),

    % TODO: Use an actual configuration file name
    Result = dungeon:start("dungeon.conf"),
    case Result of
	{fail, Reason} ->
	    io:format("Could not build dungeon: ~p~n", [Reason]),
	    io:format("Server is shutting down~n");
	{ok, Dungeon} ->
	    io:format("Dungeon (~p) successfully created~n", [Dungeon]),

	    % Link to the dungeon so that the server will know if the
	    % dungeon dies
	    link(Dungeon),
	    CallbackPid ! {server_initialized},
	    loop(Dungeon, [])
    end.

shutdown() ->
    io:format("Server is shutting down~n"),
    Dungeon = whereis(dungeon),
    case Dungeon of
	undefined -> 
	     % Do nothing -the dungeon process has already stopped
	    io:format("Dungeon has already shut down~n");
	_Any ->
	    io:format("Shutting down the dungeon~n"),
	    Dungeon ! {shutdown}
    end,

    Server = whereis(server),
    case Server of
	undefined -> 
	    % Do nothing -the server process has already stopped
	    io:format("Server has already shutdown~n"); 
	_Anything ->
	    Server ! {shutdown, "Server has shut down"} 
    end.

% The server state has the following form: 
% 
% Dungeon, [{ClientPid_1, Username_1}, {ClientPid_2, Username_2}, ...]
%
% where Dungeon is the PID of the dungeon process
loop(Dungeon, Clients) ->
    receive 

% TODO: Remove this clause
%
%	{Source, Anything} -> 
%	    %Source ! _Anything,
%	    io:format("Received message: ~p from ~p~n", [Anything, Source]),
%	    Source ! "Hi there! " ++ Anything,
%	    loop(State);

	{client, send_message, SourceUsername, DestUsername, Message} ->
	    SourcePid = getPidForUser(SourceUsername, Clients),
	    case SourcePid of		
		username_not_found -> 
		    % Nothing can be done in this case
		    io:format("Unknown user ~p tried to send a message to ~p~n",
			     [SourceUsername, DestUsername]),
		    loop(Dungeon, Clients);
		_Any ->
		    DestPid = getPidForUser(DestUsername, Clients),
		    case DestPid of		
			username_not_found -> 
			    SourcePid ! {error, "Could not send message"},
			    loop(Dungeon, Clients);
			_Any -> 
			    DestPid ! {Message, SourceUsername},
			    loop(Dungeon, Clients)
		    end
	    end;

	{client, connect, ClientPid, Username} ->
	    Result = connectClient(ClientPid, Username, Clients),
	    case Result of
		{ok, UpdatedClients} -> 
		    loop(Dungeon, UpdatedClients);
		{error, Reason} ->
		    ClientPid ! {not_connected, Reason},
		    io:format("Could not connect client ~p because: ~p~n",
			     [ClientPid, Reason]),
		    loop(Dungeon, Clients);
		_Any -> 
		    ClientPid ! {not_connected, "Unknown reason"},
		    io:format("Could not connect client ~p for unknown reason",
			     [ClientPid]),
		    loop(Dungeon, Clients)					
	    end;

	{client, disconnect, ClientPid} ->
	    UpdatedClients = disconnectClient(ClientPid, Clients),
	    ClientPid ! {disconnected},
	    loop(Dungeon, UpdatedClients);

	{client, who_is_on, ClientPid} ->
	    ClientPid ! {users, getUsernames(Clients)},
	    loop(Dungeon, Clients);

	{client, perform_action, _ClientPid, GameAction} ->
	    Dungeon ! GameAction,
	    loop(Dungeon, Clients);
	
	{dungeon, ok, connected, Username} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Unknown user ~p was added to the dungeon~n",
			     [Username]),
		    io:format("Removing ~p from the dungeon~n", [Username]),
		    Dungeon ! {disconnected, Username},
		    loop(Dungeon, Clients);
		_Any ->
		    io:format("~p was successfully added to the dungeon~n",
			     [Username]),
		    ClientPid ! {connected},
		    loop(Dungeon, Clients)
	    end;	

	{dungeon, error, connected, Username, _Reason} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Unknown user ~p could was not added to dungeon~n",
			     [Username]),
		    loop(Dungeon, Clients);
		_Any ->
		    io:format("~p was not added to the dungeon~n", [Username]),
		    unlink(ClientPid),
		    UpdatedClients = removeClient(ClientPid, Clients),
		    ClientPid ! {not_connected},
		    loop(Dungeon, UpdatedClients)
	    end;
	
	{dungeon, ok, disconnected, Username} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Unknown user ~p was removed from the dungeon~n",
			     [Username]),
		    loop(Dungeon, Clients);
		_Any ->
		    io:format("~p was successfully removed from the dungeon~n",
			     [Username]),
		    loop(Dungeon, Clients)
	    end;

	{dungeon, error, disconnected, Username, _Reason} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Unknown user ~p could not be removed from the dungeon~n",
			     [Username]),
		    loop(Dungeon, Clients);
		_Any ->
		    io:format("~p could not be removed from the dungeon~n",
			      [Username]),
		    loop(Dungeon, Clients)
	    end;

	{dungeon, ok, input, Username, GameAction} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Unknown user ~p performed action ~p~n",
			     [Username, GameAction]),
		    loop(Dungeon, Clients);
		_Any ->
		    io:format("~p successfully performed action ~p~n",
			     [Username, GameAction]),
		    ClientPid ! {ok, GameAction}, % TODO: Make sure client handles this properly
		    loop(Dungeon, Clients)
	    end;
		
	{dungeon, error, input, Username, GameAction} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Unknown user ~p failed to perform action ~p~n",
			     [Username, GameAction]),
		    loop(Dungeon, Clients);
		_Any ->
		    io:format("~p failed to perform action ~p~n",
			     [Username, GameAction]),
		    ClientPid ! {fail, GameAction}, % TODO: Make sure client handles this properly
		    loop(Dungeon, Clients)
	    end;
		
	{dungeon, ok, Username, Event} ->
	    ClientPid = getPidForUser(Username, Clients),
	    case ClientPid of		
		username_not_found -> 
		    io:format("Event ~p could not reach unknown user ~p~n",
			     [Event, Username]),
		    loop(Dungeon, Clients);
		_Any ->
		    ClientPid ! {event, Event}, % TODO: Make sure client handles this properly
		    loop(Dungeon, Clients)
	    end;

	{'EXIT', Pid, Why} ->
	    case Pid of
		Dungeon ->
		    % TODO: Try to recover more gracefully
		    self() ! {shutdown, Why};
		_Any ->
		    UpdatedClients = disconnectClient(Pid, Clients),
		    loop(Dungeon, UpdatedClients)
	    end;

	{shutdown, _Reason} ->
	    closeConnections(Clients, _Reason);

	_Any ->
	    io:format("Unrecognized message ~p received~n", [_Any])
    end.	  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILITY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Attempts to establish a connection with the client with the given ClientPid.
% A connection includes linking to the connecting client process and adding a
% player with the given Username to the dungeon. If a connection is
% successfully established a tuple of the form {ok, UpdatedClients} will be 
% returned where UpdateClients is the passed in Clients list with the new 
% client entry ({ClientPid, Username}) added; otherwise a tuple of the form 
% {error, Reason} will be returned where reason is an explaination of why the 
% connection could not be established.
connectClient(ClientPid, Username, Clients) ->
    Success = link(ClientPid),    
    case Success of
	true ->
	    dungeon ! {connected, Username},
	    {ok, [{ClientPid, Username} | Clients]};
	_Any ->
	    {error, "Could not link to the given PID"}
    end.

disconnectClient(ClientPid, Clients) ->
    unlink(ClientPid),
    Username = getUsername(ClientPid, Clients),
    case Username of
	pid_not_found -> 
	    io:format("Could not disonnect ~p: PID not found~n", [ClientPid]),
	    Clients;
	_Any ->
	    dungeon ! {disconnected, Username},
	    ClientPid ! {disconnected},
	    removeClient(ClientPid, Clients)
    end.

removeClient(_ClientPid, []) ->
    [];
removeClient(ClientPid, [{ClientPid, _Username} | T]) ->
    T;
removeClient(ClientPid, [{_Pid, _Username} | T]) ->
    [{_Pid, _Username} | removeClient(ClientPid, T)].

closeConnections([], _Reason) ->
    {ok}; % Nothing left to do
closeConnections([{ClientPid, _Username} | T], _Reason) ->
    unlink(ClientPid),
    ClientPid ! {disconnected, _Reason},
    closeConnections(T, _Reason).

getUsernames([]) ->
    [];
getUsernames([{_ClientPid, Username} | T]) ->
    [Username | getUsernames(T)].

getPidForUser(_Username, []) ->
    username_not_found;
getPidForUser(Username, [{ClientPid, Username} | _T]) ->
    ClientPid;
getPidForUser(_Username, [_H | T]) ->
    getPidForUser(_Username, T).

getUsername(_ClientPid, []) ->
    pid_not_found;
getUsername(ClientPid, [{ClientPid, Username} | _T]) ->
    Username;
getUsername(_ClientPid, [_H | T]) ->
    getUsername(_ClientPid, T).
