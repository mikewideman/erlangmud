-module(server).
-import(dungeon, [build_dungeon/1]).
-export([start/0, shutdown/0, startLoop/0, loop/1]).

start(_CallbackPid) ->
    ServerPid = spawn(server, startLoop, [_CallbackPid]),
    register(server, ServerPid),
    {ok, ServerPid}.

startLoop(CallbackPid) ->
    process_flag(trap_exit, true),

    % TODO: Use an actual configuration file name
    Dungeon = dungeon:build_dungeon(config_file_name),
    
    % Link to the dungeon so that the server will know if the dungeon dies
    link(Dungeon),

    CallbackPid ! {server_initialized},
    loop(Dungeon, []).

shutdown() ->
    Dungeon = whereis(dungeon),
    case Dungeon
	undefined -> ; % Do nothing -the dungeon process has already stopped
	_Anything ->
	    Dungeon ! {shutdown}
    end,

    Server = whereis(server),
    case Server
	undefined -> ; % Do nothing -the server process has already stopped
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
		    io:format("Unknown username ~p tried to send '~p' to ~p~n",
			     SourceUsername, Message, DestUsername),
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
		    io:format("Could not connect client ~p because: ~p~n",
			     ClientPid, Reason),
		    loop(Dungeon, Clients);
		_Any -> 
		    io:format("Could not connect client ~p for unknown reason",
			     ClientPid),
		    loop(Dungeon, Clients)					
	    end;

	{client, disconnect, ClientPid} ->
	    UpdatedClients = disconnectClient(ClientPid, Clients),
	    loop(Dungeon, UpdatedClients);

	{client, who_is_on, ClientPid} ->
	    ClientPid ! {users, getUsernames(Clients)},
	    loop(Dungeon, Clients);

	% The RequestID is used in order to allow clients to differentiate
	% between responses to different game actions in case multiple game
	% action requests are sent from the same client in a short time frame.
	% The GameAction parameter is assumed to have the correct form.
	{client, perform_action, ClientPid, _RequestID, GameAction} ->
	    Dungeon ! GameAction,
	    loop(Dungeon, Clients);
	
	{dungeon, ok, connected, Username} ->
	    ;

	{dungeon, error, connected, Username, Reason} ->
	    ;
	
	{dungeon, ok, disconnected, Username} ->
	    ;

	{dungeon, error, disconnected, Username, Reason} ->
	    ;

	{dungeon, ok, input, Username, GameAction} ->
	    ;
		
	{dungeon, error, input, Username, GameAction} ->
	    ;
		
	{dungeon, ok, Username, Event} ->
	    ;

	{'EXIT', Pid, Why} ->
	    case Pid of
		Dungeon ->
		    % TODO: Try to recover more gracefully
		    self() ! {shutdown, "Dungeon has collapsed!"};
		_Any ->
		    UpdatedClients = disconnectClient(Pid, Clients),
		    loop({Dungeon, UpdatedClients}
	    end;

	{shutdown, _Reason} ->
	    closeConnections(Clients, _Reason)
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
    dungeon ! {disconnected, Username},
    removeClient(ClientPid, Clients).

removeClient(ClientPid, []) ->
    [];
removeClient(ClientPid, [{ClientPid, _Username} | T]) ->
    T;
removeClient(ClientPid, [{_Pid, _Username} | T]) ->
    [{_Pid, _Username} | removeClient(ClientPid, T)].

closeConnections([], _Reason) ->
    ; % Nothing left to do
closeConnections([{ClientPid, _Username} | T], _Reason) ->
    unlink(ClientPid),
    ClientPid ! {disconnected, _Reason}.

getPids([]) ->
    [];
getPids([{ClientPid, _Username} | T]) ->
    [ClientPid | getPids(T)];

getUsernames([]) ->
    [];
getUsernames([{_ClientPid, Username} | T]) ->
    [Username | getUsernames(T)];

getPidForUser(_Username, []) ->
    username_not_found;
getPidForUser(Username, [{ClientPid, Username} | T]) ->
    ClientPid;
getPidForUser(_Username, [_H | T]) ->
    getPidForUser(_Username, T).
