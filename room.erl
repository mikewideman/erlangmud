%%%=============================================================================
%%% @doc A dungeon room.
%%% Rooms are the smallest unit of geographical space in the dungeon. Rooms
%%% are connected to each other by doors (links to other rooms). Rooms may
%%% contain characters and/or items, or nothing at all. Rooms act as the liaison
%%% for communication between characters and other entities of the game by
%%% directing messages (e.g. game actions) to the concerned parties, as well as
%%% the publisher of game event messages which are sent to users.
%%% @end
%%%=============================================================================

-module(room).
-export([start/1, targetAction/2, targetAction/4]).
-include("room.hrl").

-spec start(string()) -> pid().
%% @doc Spawn a new room process, initializing it with a given Description.
%% Returns the room pid.
%% @todo what else will this function do?
%% @end
start(Description) ->
    Room = make_room(Description),
    % @todo link rooms, add content to rooms
    spawn(fun() -> main(Room) end).

-spec main(#room{}) -> no_return().
%% @doc The main function of a room process. Loops forever.
%% @end
%functions

%forward an action to the direct object. On success, an event is propagated. The result from the thing is returned.
%the objects are by human-readable name at this point, but will be converted to PIDs here
%should you wish to omit the indirect object, use the atom none
targetAction(RoomPid, {action, Verb, DirectObject, IndirectObject}) -> 
	RoomPid ! {self(), targetAction, {action, Verb, DirectObject, IndirectObject}}.
targetAction(RoomPid, Verb, DirectObject, IndirectObject) ->
	targetAction(RoomPid, {action, Verb, DirectObject, IndirectObject}).



main(Room) ->   % @todo consider that we will need to talk to the dungeon pid
	receive
		{Sender, targetAction, Action} -> 
			Sender ! s_targetAction(Action, Room#room.things),
			main(Room)
	end.

s_targetAction(HRAction, ThingList) ->
	Action = hrActionToPidAction(HRAction, ThingList),
	case Action of
		{error, Reason} -> {error, Reason};
		{action, _V, DirectObject, _} -> thing:handleAction(DirectObject, Action)
	end.
%HELPERS

%takes an action with human readable strings for objects and converts them to Pids.
%returns {error, something} on error.
hrActionToPidAction(HRAction, ThingList) ->
	{action, Verb, HRDirectObject, HRIndirectObject} = HRAction,
	DirectObject = findThingByName(ThingList, HRDirectObject),
	IndirectObject = case HRIndirectObject of
		none -> none;
		_ -> findThingByName(ThingList, HRIndirectObject)
	end,
	%check that all is okay
	case is_pid(DirectObject) of
		true -> {action, Verb, DirectObject, IndirectObject};
		false -> {error, DirectObject}
	end.
%get a PID for a thing from its hr name. 
%Find the thing with from its name. Maybe do some fuzzy matching in the future, or warn on multiple matches.
%returns atom notfound if not found.
findThingByName([H | T], Name) ->
	%assuming that Thing.name() will exist at some point
	TN = normalizeString(thing:name(H)),
	Equals = string:equal(TN, Name),
	if
		Equals -> H;
		true   -> findThingByName(T, Name)
	end;
findThingByName([], _) ->
	notfound.


normalizeString(String) -> 
	string:to_lower(string:strip(String)). %normalize it for better comparison
	

    
