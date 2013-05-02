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
-export([start/1, targetAction/3, targetAction/5, look/1, broadcast/2, targetInput/3]).
-include("room.hrl").
-include("action.hrl").

-spec start(string()) -> pid().
%% @doc Spawn a new room process, initializing it with a given Description.
%% Returns the room pid.
%% @todo what else will this function do?
%% @end
start(Description) ->
    Room = make_room(Description),
    % @todo link rooms, add content to rooms
    spawn(fun() -> main(Room) end).

%functions

%% @todo change to fit action() type
%%-spec targetAction(pid(), boolean(), action()) -> any().
%forward an action to the direct object. On success, an event is propagated. The result from the thing is returned.
%IsHR indiracets whether the objects are strings (human readable) or pids
%the objects are by human-readable name at this point, but will be converted to PIDs here
%should you wish to omit the indirect object, use the atom none
targetAction(RoomPid, IsHR, {action, Verb, DirectObject, IndirectObject}) -> 
	RoomPid ! {self(), IsHR, targetAction, {action, Verb, DirectObject, IndirectObject}},
	receive_response().
targetAction(RoomPid, IsHR, Verb, DirectObject, IndirectObject) ->
	targetAction(RoomPid, IsHR, {action, Verb, DirectObject, IndirectObject}).
targetInput(RoomPid, PlayerPid, {input, Verb, DirectObject, IndirectObject}) ->
	RoomPid ! {self(), targetInput, PlayerPid, {input, Verb, DirectObject, IndirectObject}},
	receive_response().

%get a list of pids of all the things in the room
look(RoomPid) ->
	RoomPid ! {self(), look},
	receive_response().
%Send everyone an arbitrary message using thing:receiveEvent (should be an event, if our defined format made any sense.) No return value.
%I'm using the event format {event, BY, VERB, ON, WITH}
%we should handel hr message text somewhere else
broadcast(RoomPid, Event) ->
	RoomPid ! {self(), broadcast, Event}.

%wait for an incoming message and return it as a return value
receive_response() ->
	receive
		Any -> Any
	after 0 ->
		timeout
	end.

-spec main(#room{}) -> no_return().
%% @doc The main function of a room process. Loops forever.
%% @end
main(Room) ->   % @todo consider that we will need to talk to the dungeon pid
	receive
		{Sender, targetAction, IsHR, Action} -> 
			Sender ! s_targetAction(Sender, IsHR, Action, Room#room.things),
			main(Room);
		{Sender, look}		->
			Sender ! Room#room.things, % @todo turn into game event or something
			main(Room);
		{_, broadcast, Event} ->
			doPropagateEvent(Event, Room#room.things), main(Room);
		{Sender, targetInput, PlayerPid, Input} ->
			Sender ! s_targetInput(PlayerPid, Input, Room#room.things),
			main(Room)
	after 0 -> main(Room)
	end.

s_targetAction(Sender, IsHR, InAction, ThingList) ->
	%convet the text actions to pids if not IsHr
 Action =	if 
		IsHR -> hrActionToPidAction(InAction, ThingList);
		true -> InAction
	end,
	case Action of
		{error, Reason} -> {error, Reason};
		{action, _V, DirectObject, _} -> 
			%if no problems finding the pids, forward it
			Result = thing:handleAction(DirectObject, Action),
			%propagate if okay. I don't know yet how Thing will say it was okay, so I guessed.
			case Result of 
				{ok, _} ->
					doPropagateEvent(makeEvent(Sender, Action), ThingList);
				_	-> Result
			end
	end.
s_targetInput(Player, HRAction, ThingList) ->
	Action = hrActionToPidAction(HRAction, ThingList),
	player:receiveInput(Player, Action).
makeEvent(Sender, Action) ->
	{action, Verb, DirectObject, IndirectObject} = Action,
	{event, Sender, Verb, DirectObject, IndirectObject}.
%turn the action into an event and tell everyone!
doPropagateEvent(Event, ThingList) ->
	ESend = fun(ThingPid) -> thing:receiveEvent(ThingPid, Event) end,
	lists:foreach(ESend, ThingList).
    % @todo we could use the lists:map approach our professor used in class today
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
	

    
