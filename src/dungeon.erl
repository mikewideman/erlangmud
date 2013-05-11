-module(dungeon).
-export([build_dungeon/1]).
-include("defs.hrl").

% build_dungeon
%
% Start a dungeon process and register it to the atom 'dungeon'.
% The dungeon will read its configuration out of the file
% with the name specified.
build_dungeon(ConfigFileName) ->
	{ok, Configuration} = yaml:load_file(ConfigFileName, [implicit_atoms]),
	RoomList = lists:keyfind(rooms, 1, Configuration),
	Dungeon = spawn(fun() -> dungeon_loop(RoomList, dict:new()) end),
	register(dungeon, Dungeon),
	Dungeon.

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
		{connected, Username} -> 
		    Result = connect_player(Connections, Username, Rooms),
			case Result of
				{ok, NewConnections} 	-> server ! {ok, connected, Username},
										   dungeon_loop(Rooms, NewConnections);
				{error, Message} 		-> server ! {error, connected, Username, Message},
										   dungeon_loop(Rooms, Connections)
			end;
		{disconnected, Username} ->
			Result = disconnect_player(Connections, Username),
			case Result of
				{ok, NewConnections} 	-> server ! {ok, disconnected, Username},
							   			   dungeon_loop(Rooms, NewConnections);
				{error, Message} 		-> server ! {error, disconnected, Username, Message},
							   			   dungeon_loop(Rooms, Connections)
			end;
		
		% propagate the event to all users who are in that room.
		{event, {Event, RoomProc}} -> dungeon_loop(Rooms, Connections);

		% player moved to a different room, update
		% the connection map.
		{characterMoved, PlayerProc, RoomProc} ->
			Result = move_player(Connections, PlayerProc, RoomProc),
			case Result of
				{ok, NewConnections} 	-> dungeon_loop(Rooms, NewConnections);
				{error, _Message}		-> io:format("Warning: Dungeon state may be inconsistent."),
										   dungeon_loop(Rooms, Connections);
		{Username, Verb, Object, Payload} ->
			{PlayerProc, RoomProc} = dict:fetch(Username, Connections),
			Input = #input{verb=Verb, subject=PlayerProc, object=Object, payload=Payload},
			room:targetInput(RoomProc, Input),
			% TODO: add error handling
			dungeon_loop(Rooms, Connections);
		Any -> io:format("~p~n", [Any]), dungeon_loop(Rooms, Connections)
	end.

% move_player
%
% Update the dungeon's concept of which room a player
% is currently in.
move_player(Connections, PlayerProc, RoomProc) ->
	Username = PlayerProc#character.name,
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
			% player:start returns a player pid... would be nicer if it
			% returned a player proc record.
			PlayerRecord = player:start(Username, default, default, StartingRoom),
			NewConnections = dict:store(Username, {PlayerRecord, StartingRoom}),
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
			% do something to remove the player from the room
			% room:delete_or_whatever(CurrentRoom, PlayerRecord)
			{ok, NewConnections}
	end.

