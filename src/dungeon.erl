-module(dungeon).
-export([start/1, merge/1]).
-include("defs.hrl").

% start
%
% Start a dungeon process and register it to the atom 'dungeon'.
% The dungeon will read its configuration out of the file
% with the name specified.
start(ConfigFileName) ->
	Result = file:consult(ConfigFileName),
	case Result of
		{ok, RoomConf} ->
			Dungeon = spawn(fun() -> 
						process_flag(trap_exit, true),
						RoomProcs = [room:start(Description) || {room, Description} <- RoomConf],
						dungeon_loop(RoomProcs, dict:new()) end),
			register(dungeon, Dungeon),
			{ok, Dungeon};
		{error, {Line, Mod, Term}} ->
			Reason = file:format_error({Line, Mod, Term}),
			{fail, Reason};
		{error, Error} ->
			{fail, Error}
	end.		

% merge
%
% Add additional rooms onto a running dungeon.
merge(ConfigFileName) ->
	Result = file:consult(ConfigFileName),
	case Result of
			{ok, RoomConf} ->
				RoomProcs = [room:start(Description) || {room, Description} <- RoomConf],
				dungeon ! {merge, RoomProcs},
				{ok, merged};
			{error, {Line, Mod, Term}} ->
				Reason = file:format_error({Line, Mod, Term}),
				{fail, Reason};
			{error, Error} ->
				{fail, Error}
	end.

% dungeon_loop
%
% TODO: Update documentation
%
% Main message loop for the Dungeon.
% The dungeon responds to 4 types of messages.
% These messages are:
%		Player connected 	-> Spawn a new player in a randomly selected
%							   starting room.
%		Player disconnected -> Remove the disconnected player from the dungeon.
%		Player input		-> Pass player input down to the room they are in.
%		Room responses		-> Pass a room response up the chain to the connection.
dungeon_loop(Rooms, Connections) ->
	receive
		{merge, NewRooms} ->
			dungeon_loop(Rooms ++ NewRooms, Connections);
		{shutdown} ->
			io:format("Dungeon is shutting down.~n");
		{status} ->
			server ! {Rooms, Connections};
		{connected, Username} -> 
		    Result = connect_player(Connections, Username, Rooms),
			case Result of
				{ok, NewConnections} 	-> server ! {dungeon, ok, connected, Username},
										   dungeon_loop(Rooms, NewConnections);
				{error, Message} 		-> server ! {dungeon, error, connected, Username, Message},
										   dungeon_loop(Rooms, Connections)
			end;
		{disconnected, Username} ->
			Result = disconnect_player(Connections, Username),
			case Result of
				{ok, NewConnections} 	-> server ! {dungeon, ok, disconnected, Username},
							   			   dungeon_loop(Rooms, NewConnections);
				{error, Message} 		-> server ! {dungeon, error, disconnected, Username, Message},
							   			   dungeon_loop(Rooms, Connections)
			end;
		
		% propagate the event to all users who are in that room.
		{event, {Event, RoomProc}} 		-> propagate_event(Connections, RoomProc, Event),
										   dungeon_loop(Rooms, Connections);

		% player moved to a different room, update
		% the connection map.
		{characterMoved, PlayerProc, RoomProc} ->
			Result = move_player(Connections, PlayerProc, RoomProc),
			case Result of
				{ok, NewConnections} 	-> dungeon_loop(Rooms, NewConnections);
				{error, _Message}		-> io:format("Warning: Dungeon state may be inconsistent."),
										   dungeon_loop(Rooms, Connections)
			end;
		
		% black-hole EXIT messages.
		% The dungeon doesn't care or need to care if any processes die.
		{'EXIT', _, _} ->
			dungeon_loop(Rooms, Connections);
		
		{error, Any} ->
			io:format("Dungeon received an error message it didn't understand: ~p~n", [Any]),
			dungeon_loop(Rooms, Connections);
	
		{Username, {look}} ->
			%server ! {dungeon, ok, input, Username, {Verb, noDirectObject}},
			{PlayerProc, RoomProc} = dict:fetch(Username, Connections),
			Response = room:look(RoomProc),
			Event = {look, PlayerProc, RoomProc#room_proc.description, [Response]},
			server ! {dungeon, ok, Username, Event},
			dungeon_loop(Rooms, Connections);

		% Propgate input from the client down to the room where the player
		% is.
		{Username, {Verb, Object}} ->
			io:format("WTF: ~p ~p~n", [Username, {Verb, Object}]),
			server ! {dungeon, ok, input, Username, {Verb, Object}},
			{PlayerProc, RoomProc} = dict:fetch(Username, Connections),
			Input = #input{verb=Verb, subject=PlayerProc, object=Object},
			Response = room:targetInput(RoomProc, Input),
			io:format("TargetInput Response: ~p~n", [Response]),
			% TODO: add error handling
			dungeon_loop(Rooms, Connections);
		
		% catchall for invalid messages from the server
		{_Username, Any} -> 
			server ! {dungeon, error, input, Any},
			io:format("~p~n", [Any]),
			dungeon_loop(Rooms, Connections);
		
		% catchall for messages not matching the game protocol.
		{Any} ->
			io:format("~p~n", [Any]),
			dungeon_loop(Rooms, Connections)
	end.

% propagate_event
%
% Push an event up to all the users who were in the room when it occured.
propagate_event(Connections, RoomProc, Event) ->
	ConnectionList = dict:to_list(Connections),
	[server ! {dungeon, ok, Username, Event} || {Username, Room} <- ConnectionList, Room == RoomProc].

% move_player
%
% Update the dungeon's concept of which room a player
% is currently in.
move_player(Connections, PlayerProc, RoomProc) ->
	Username = PlayerProc#thing_proc.name,
	Result = dict:is_key(Username, Connections),
	case Result of
		false -> {error, "Username not in the dungeon"};
		true -> NewConnections = dict:update(Username, {PlayerProc, RoomProc}),
				{ok, NewConnections}
	end.

% connect_player
%
% Create a player character for the newly connected user
% and insert the player into a randomly selected
% starting room.
connect_player(Connections, Username, Rooms) ->
	StartingRoom = lists:nth(random:uniform(length(Rooms)), Rooms),
	Result = dict:is_key(Username, Connections),
	case Result of
		true -> {error, "Username already in the dungeon."};
		false ->
			PlayerRecord = player:start(Username, default, default, StartingRoom),
			NewConnections = dict:store(Username, {PlayerRecord, StartingRoom}, Connections),
			room:addThing(StartingRoom, PlayerRecord),
			{ok, NewConnections}
	end.

% disconnect_player
%
% Remove the disconneced user's player character
% from the dungeon.
disconnect_player(Connections, Username) ->
	Result = dict:is_key(Username, Connections),
	case Result of
		false -> {error, "Username not in the dungeon."};
		true ->
			{PlayerRecord, CurrentRoom} = dict:fetch(Username, Connections),
			NewConnections = dict:erase(Username, Connections),
			% remove the player from the room
			room:leaveGame(CurrentRoom, PlayerRecord),
			{ok, NewConnections}
	end.

