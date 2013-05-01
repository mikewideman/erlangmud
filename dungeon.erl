-module(dungeon).
-export([build_dungeon/1, dungeon_loop/2]).
 
build_dungeon(ConfigFileName) ->
	{ok, Configuration} = yaml:load_file(ConfigFileName, [implicit_atoms]),
	RoomList = lists:keyfind(rooms, 1, Configuration),
	Dungeon = spawn(fun() -> dungeon_loop(RoomList, []) end),
	register(dungeon_, Dungeon),
	Dungeon.
	
dungeon_loop(Rooms, Connections) ->
	receive
		{input, Sender, InputTuple} -> 
			inform_room(Sender, InputTuple, Connections),
			dungeon_loop(Room, Connections);
		{action, Sender, ActionTuple} ->
			inform_room(Sender, ActionTuple, Connections),
			dungeon_loop(Room, Connections);
		{event, RoomRef, Event} ->
			inform_players(RoomRef, {event, RoomRef, Event}, Connections),
			dungeon_loop(Room, Connections);
		{spawn, RoomRef, ThingRef} ->
			RoomRef ! {spawn, ThingRef},
			dungeon_loop(Room, Connections);
		%{roomAdd, Source, Direction, Destination} ->;
		%{lookResult, PersonRef, [ThingsInRoom]} ->;
		Any -> io:format("~p~n", [Any]), dungeon_loop(Rooms, Connections)
	end.
	
inform_room(Sender, MsgTuple, Connections) ->
	{Sender, Room} = lists:keyfind(Sender, 1, Connections),
	Room ! MsgTuple.

inform_players(RoomRef, MsgTuple, Connections) ->
	Filter = fun({_Player, RoomRef}) -> true; (_) -> false end),
	InRoom = lists:filter(Filter, Connections),
	[inform_player(MsgTuple, PlayerPid) || {PlayerPid, RoomRef} <- InRoom].

inform_player(MsgTuple, PlayerPid) ->
	PlayerPid ! MsgTuple.