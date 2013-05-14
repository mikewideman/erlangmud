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
	    Dungeon ! {shutdown} % TODO: Make sure this message is handled 
		                 % properly by the dungeon 
    end,

    Server = whereis(server),
    case Server
	undefined -> ; % Do nothing -the server process has already stopped
	_Anything ->
	    Server ! {shutdown} 
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
	    DestPid = getPidForUser(DestUsername, Clients),
	    DestPid ! {Message, SourceUsername},
	    loop(Dungeon, Clients);

	{client, connect, ClientPid, Username} ->
	    % TODO: Add ClientPid and Username as a tuple to the list of
	    %       connected clients (the server state)
	    %
	    % TODO: Start player w/ given username on the Dungeon
	    %
	    % TODO: Send back acknoledgement or failure message; also make
	    %       sure that if anything goes wrong (such as if the client
	    %       process/node dies) after the player has been created in the
	    %       dungeon that the dungeon is immediately informed and the
	    %       player is removed from the game and that they are also 
	    %       removed from the server state (list of connected clients)
	    Result = connectClient(ClientPid, Username, Clients),
	    case Result of
		{ok, UpdatedClients} -> loop(Dungeon, UpdatedClients);
		_Any -> loop(Dungeon, Clients)					
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
	% The GameAction parameter is assumed to have the form 
	% {Username, Verb, Object, DirectObject}
	%
	% TODO: Determine if DirectObject is optional
	%
	% TODO: Make sure that the client is sending properly formatted
	%       GameActions and that improperly formatted actions are handled
	%       properly (i.e. an error message is sent back from the dungeon)
	{client, perform_action, ClientPid, _RequestID, GameAction} ->
	    Dungeon ! GameAction,
	    loop(Dungeon, Clients);

	%%
	%% TODO: Handle dungeon response messages here
	%%

	{'EXIT', Pid, Why} ->
	    case Pid of
		Dungeon ->
		    broadcast("The dungeon has collapsed!", Clients),
		    
		    % TODO: Try to recover more gracefully
		    self() ! {shutdown};
		_Any ->
		    UpdatedClients = disconnectClient(Pid, Clients),
		    loop({Dungeon, UpdatedClients}
	    end;

	{shutdown} ->
	    closeConnections(Clients)
    end.	  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILITY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Attempts to establish a connection with the client with the given ClientPid.
% A connection includes linking to the connecting client process and adding a
% player with the given Username to the dungeon. If that username is already
% in use the connection will fail. If a connection is successfully established
% a tuple of the form {ok, UpdatedClients} will be returned where UpdateClients
% is the passed in Clients list with the new client entry 
% ({ClientPid, Username}) added; otherwise a tuple of the form {fail, Reason}
% will be returned where reason is an explaination of why the connection could
% not be established.
% 
% TODO: Implement
connectClient(ClientPid, Username, Clients) ->
    link(ClientPid),
    dungeon ! {connected, Username}

disconnectClient(ClientPid, Clients) ->
    % TODO: Remove the client which died from the list of active
    % connections and tell the dungeon to remove that client's
    % player from the game

% TODO: Implement (send messages to all clients notifying them that the server
% is shutting down)
closeConnections(Clients) ->
    .

getPids(Clients) ->
    .

getUsernames(Clients) ->
    .

getPidForUser(Username, Clients) ->
    .

broadcast(Message, Clients) ->
    .
