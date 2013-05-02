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
-export([start/1, targetAction/2, look/1, targetInput/2]).
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

%Target an action to the action's direct object. Sends it to thing:handleAction which returns an event. If successful, the event is propagated by sending it to thing:receiveEvent.
%on failure, returns {error, Reason} where Reason can be {notInRoom, What} where What is directObject or subject. Or Reason can be something from the Thing, or whatever other errors  might come up. Maybe there should be an error() type?
-spec targetAction(room_type(), action()) -> {'ok' | 'error', atom()}.
targetAction({RoomPid, _ , _}, Action) -> 
	RoomPid ! {self(), targetAction, Action},
	receive_response().
-spec targetInput(room_type(), hr_action()) -> {'ok' | 'error', atom()}.
targetInput({RoomPid, _ , _}, Action) ->
	RoomPid ! {self(), targetInput, Action},
	receive_response().

%get a list of pids of all the things in the room
-spec look(room_type()) -> [thing_type()].
look({RoomPid, _ , _}) ->
	RoomPid ! {self(), look},
	receive_response().
%Send everyone an arbitrary message using thing:receiveEvent (should be an event, if our defined format made any sense.) No return value.
%I'm using the event format {event, BY, VERB, ON, WITH}
%we should handel hr message text somewhere else
-spec broadcast(room_type(), event()) -> any().
broadcast({RoomPid, _, _}, Event) ->
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
		{Sender, targetAction, Action} -> 
			Sender ! s_targetAction(Room, Action),
			main(Room);
		{Sender, look}		->
			Sender ! Room#room.things, % @todo turn into game event or something
			main(Room);
		{_, broadcast, Event} ->
			doPropagateEvent(Event, Room#room.things), main(Room);
		{Sender, targetInput, Input} ->
			Sender ! s_targetInput(Room, Input),
			main(Room)
	after 0 -> main(Room)
	end.

%%%SERVER FUNCTIONS
s_targetAction(Room, Action) ->
	{_Verb, SubjectPid, DObjectPid} = Action,
	%first make sure the subject is in the room
	case lists:keysearch(SubjectPid, 3, Room#room.things) of
		false	->	{error, {notInRoom, subject}};
		{value, _Subject} ->
		%search for the direct object in the room, and send it the action
		case lists:keysearch(DObjectPid, 3, Room#room.things) of
			%the direct object exists. Send it the action.
			{value, Thing} -> case thing:handleAction(Thing, Action) of
				%if it errored, return the error
				{error, Reason} -> {error, Reason};
				%if it didn't error, assume it's an event, and propagate it
				Event		-> lists:foreach(fun(Thing) -> thing:receiveEvent(Thing, Event) end, Room#room.things)
			end; %end handle handle action
			false		-> {error, {notInRoom, directObject}}
		end %end search for DI
	end.



    
