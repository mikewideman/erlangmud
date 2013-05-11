-module(dungeon).
-export([build_dungeon/1]).

% build_dungeon
%
% Start a dungeon process and register it to the atom 'dungeon'.
% The dungeon will read its configuration out of the file
% with the name specified.
build_dungeon(ConfigFileName) ->
	{ok, Configuration} = yaml:load_file(ConfigFileName, [implicit_atoms]),
	RoomList = lists:keyfind(rooms, 1, Configuration),
	Dungeon = spawn(fun() -> dungeon_loop(RoomList, []) end),
	register(dungeon, Dungeon),
	Dungeon.

% dungeon_loop
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
				{error, Message} 	-> server ! {error, connected, Username, Message},
							   dungeon_loop(Rooms, Connections)
			end;
		{disconnected, Username} ->
			Result = disconnect_player(Connections, Username),
			case Result of
				{ok, NewConnections} 	-> server ! {ok, disconnected, Username},
							   dungeon_loop(Rooms, NewConnections);
				{error, Message} 	-> server ! {error, disconnected, Username, Message},
							   dungeon_loop(Rooms, Connections)
			end;
		{Username, Verb, Object, DirectObject} ->
			% figure out the room and delegate the message.	
			dungeon_loop(Rooms, Connections);
		Any -> io:format("~p~n", [Any]), dungeon_loop(Rooms, Connections)
	end.

% connect_player
%
% Create a player character for the newly connected user
% and insert the player into a randomly selected
% starting room.
connect_player(Connections, Username, Rooms) ->
	StartingRoom = lists:nth(random:uniform(length(Rooms)), Rooms),
	Result = lists:keyfind(Username, 1, Connections),
	case Result of
		true -> {error, "Username already in the dungeon."};
		false ->
			% player:start returns a player pid... would be nicer if it
			% returned a player proc record.
			Player = player:start(Username, default, default, StartingRoom),
			{ok, [{Username, Player}] ++ Connections}
	end.

% disconnect_player
%
% Remove the disconneced user's player character
% from the dungeon.
disconnect_player(Connections, Username) ->
	Result = lists:keytake(Username, 1, Connections),
	case Result of
		false -> {error, "Username not in the dungeon."};
		{Username, {Username, Player}, NewConnections} ->
			% do something to remove the player from the room
			{ok, NewConnections}
	end.

