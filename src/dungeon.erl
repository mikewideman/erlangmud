-module(dungeon).
-export([start/1, merge/1, spawn/2, rooms/0]).
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
						RoomProcs = [room:start(Description) || {room, Description, _Things, _Doors} <- RoomConf],
						dungeon_loop(RoomProcs, dict:new()) end),
			register(dungeon, Dungeon),
			dungeon ! {init, RoomConf},
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

% rooms
%
% Return the list of rooms in the dungeon.
rooms() ->
	dungeon ! {rooms, self()},
	receive
		Any -> Any
	end.

% spawn
%
% Insert a Thing into a room.
spawn(Thing, Room) ->
	dungeon ! {spawn, Thing, Room},
	{ok}.

% h_populate_rooms
% 
% Init helper function. 
% Insert starting items and mobs into rooms.
h_populate_rooms(Rooms, RoomConf) ->
	Each = fun(Room) ->
		{room, _Description, Things, _Doors} = lists:keyfind(Room#room_proc.description, 2, RoomConf),
		[ dungeon:spawn(object:start(ThingAtom, ThingName, ThingVal, Room), Room) || {ThingAtom, ThingName, ThingVal} <- Things]
	end,
	lists:foreach(Each, Rooms).

% h_link_rooms
% 
% Init helper function.
% Link rooms with doors.
h_link_rooms(Rooms, RoomConf) ->
	Each = fun(Room) ->
		{room, _Description, _Things, Doors} = lists:keyfind(Room#room_proc.description, 2, RoomConf),
		[ room:makeDoor(Direction, Room, lists:nth(Number, Rooms)) || {Direction, Number} <- Doors]
	end,
	lists:foreach(Each, Rooms).
	

% dungeon_loop
%
% Main message loop for the Dungeon.
% The dungeon responds to several types of messages.
% Some of these messages are:
%		Player connected 	-> Spawn a new player in a randomly selected
%							   starting room.
%		Player disconnected -> Remove the disconnected player from the dungeon.
%		Player input		-> Pass player input down to the room they are in.
%		Room responses		-> Pass a room response up the chain to the connection.
%
% Other messages are documented at their pattern match in the following loop.
dungeon_loop(Rooms, Connections) ->
	receive

		{init, RoomConf} ->
			h_populate_rooms(Rooms, RoomConf),
			h_link_rooms(Rooms, RoomConf),
			dungeon_loop(Rooms, Connections);
		{rooms, Sender} ->
			Sender ! {Rooms},
			dungeon_loop(Rooms, Connections);
		{spawn, Thing, Room} ->
			room:addThing(Room, Thing),
			dungeon_loop(Rooms, Connections);
		{merge, NewRooms} ->
			dungeon_loop(Rooms ++ NewRooms, Connections);

		% Cease looping and print a status message.
		{shutdown} ->
			io:format("Dungeon is shutting down.~n");
		% Return the dungeon's status to the server.
		{status} ->
			server ! {Rooms, Connections},
			dungeon_loop(Rooms, Connections);

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
		{event, {Event, RoomProc}} 		-> io:format("Event: ~p ~p~n", [Event, RoomProc]),
										   propagate_event(Connections, RoomProc, Event),
										   dungeon_loop(Rooms, Connections);

		% player moved to a different room, update
		% the connection map.
		{characterMoved, {PlayerProc, RoomProc}} ->
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
		
		% Give the user a view of their surroundings
		{Username, {look}} ->
			%server ! {dungeon, ok, input, Username, {Verb, noDirectObject}},
			{PlayerProc, RoomProc} = dict:fetch(Username, Connections),
			io:format("Player looking around room: ~p~n",[RoomProc]),
			Response = room:look(RoomProc),
			ResponseStrings = lists:map(fun(Thing) -> Thing#thing_proc.name end, Response),
			Event = #event{verb=look, subject=PlayerProc, object=RoomProc#room_proc.description, payload=[{room_content, ResponseStrings}]},
			server ! {dungeon, ok, Username, Event},
			dungeon_loop(Rooms, Connections);

		% Propgate input from the client down to the room where the player
		% is.
		{Username, {Verb, Object}} ->
			server ! {dungeon, ok, input, Username, {Verb, Object}},
			{PlayerProc, RoomProc} = dict:fetch(Username, Connections),
			Input = #input{verb=Verb, subject=PlayerProc, object=Object},
			Response = room:targetInput(RoomProc, Input),
			io:format("TargetInput Response: ~p~n", [Response]),
			% TODO: add error handling for this?
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
	io:format("Propagate~n"),
	ConnectionList = dict:to_list(Connections),
	UsersToMessage = [{U, {T, R}} || {U, {T, R}} <- ConnectionList, R#room_proc.id == RoomProc#room_proc.id],
	io:format("~p~n", [UsersToMessage]),
	[server ! {dungeon, ok, Username, Event} || {Username, _Room} <- UsersToMessage].

% move_player
%
% Update the dungeon's concept of which room a player
% is currently in.
move_player(Connections, PlayerProc, RoomProc) ->
	Username = PlayerProc#thing_proc.name,
	Result = dict:is_key(Username, Connections),
	case Result of
		false -> {error, "Username not in the dungeon"};
		true -> NewConnections = dict:store(Username, {PlayerProc, RoomProc}, Connections),
				{ok, NewConnections}
	end.

% connect_player
%
% Create a player character for the newly connected user
% and insert the player into a randomly selected
% starting room.
connect_player(Connections, Username, Rooms) ->
	% StartingRoom = lists:nth(random:uniform(length(Rooms)), Rooms),
	StartingRoom = lists:nth(1, Rooms),
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

