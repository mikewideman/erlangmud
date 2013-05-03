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
-export([start/1, targetAction/2, look/1, targetInput/2, broadcast/2, addThing/2]).
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
2
%% @todo change to fit action() type
%%-spec targetAction(pid(), boolean(), action()) -> any().

%Target an action to the action's direct object. Sends it to thing:handleAction which returns an event. If successful, the event is propagated by sending it to thing:receiveEvent.
%on failure, returns {error, Reason} where Reason can be {notInRoom, What} where What is directObject or subject. Or Reason can be something from the Thing, or whatever other errors  might come up. Maybe there should be an error() type?
-spec targetAction(#room_proc{}, #action{}) -> {'ok' | 'error', atom()}.
targetAction(Room_Proc, Action) -> 
	Room_Proc#room_proc.pid ! {self(), targetAction, Action},
	receive_response().
%Targets input to a person from the user, converting the direct object's name from a hr string to a thing type in the process.
%(IE it converts Input to Action)
%Input in the form {Verb :: verb(), Subject :: pid(), DObject :: string()} 
%sends it to person in the form of #action{}
%returns the result from person OR {error, {why, who}}
-spec targetInput(#room_proc{}, #action{})-> {'ok' | 'error', atom()}.
targetInput(Room_Proc, Input) when Input#action.type == input->
	Room_Proc#room_proc.pid ! {self(), targetInput, Input},
	receive_response().

%get a list of all the things in the room
-spec look(#room_proc{}) -> [thing_type()].
look(Room_Proc) ->
	Room_Proc#room_proc.pid ! {self(), look},
	receive_response().
%Send everyone an arbitrary message using thing:receiveEvent (should be an event, if our defined format made any sense.) No return value.
%I'm using the event format {event, BY, VERB, ON, WITH}
%we should handel hr message text somewhere else
-spec broadcast(#room_proc{}, #action{}) -> any().
broadcast(Room_Proc, Event) when Event#action.type == event->
	Room_Proc#room_proc.pid ! {self(), broadcast, Event}.

%add a thing to the room (enter it, spawn it, whatever you want to call it). 
%an event will be propagated
-spec addThing(#room_proc{}, thing_type()) -> any().
addThing(Room_Proc, Thing) ->
	Room_Proc#room_proc.pid ! {self(), addThing, Thing}.

%wait for an incoming message and return it as a return value
%TODO: maybe check that it was the response we were expecting?
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
    %% @todo modify Room and main with 'NewRoom' or something of that sort
	receive
		{Sender, targetAction, Action} when is_record(Action, action) -> 
			Sender ! s_targetAction(Room, Action),
			main(Room);
		{Sender, look}		->
			Sender ! Room#room.things, % @todo turn into game event or something
			main(Room);
		{_, broadcast, Event} ->
			propagateEvent(Room, Event),
			main(Room);
		{Sender, targetInput, Input} ->
			Sender ! s_targetInput(Room, Input),
			main(Room);
		{_, addThing, Thing} ->
			main(s_addThing(Room, Thing))
	after 0 -> main(Room)
	end.

%%%SERVER FUNCTIONS
s_targetAction(Room, Action) ->
	SubjectPid = Action#action.subject,
	DObjectPid = Action#action.object,
	%first make sure the subject is in the room
	case lists:keysearch(SubjectPid, 1, Room#room.things) of %% the character_t keeps the pid in its first index
		false	->	{error, {notInRoom, subject}};
		{value, _Subject} ->
		%search for the direct object in the room, and send it the action
		case lists:keysearch(DObjectPid, 1, Room#room.things) of
			%the direct object exists. Send it the action.
			{value, Thing} -> case thing:handleAction(Thing, Action) of
				%if it errored, return the error
				{error, Reason} -> {error, Reason};
				%if it didn't error, assume it's an event, and propagate it
				Event		-> propagateEvent(Room, Event)
			end; %end handle handle action
			false		-> {error, {notInRoom, directObject}}
		end %end search for DI
	end.

%Targets input to a person from the user, converting the direct object's name from a hr string to a thing type in the process.
%(IE it converts Input to Action)
%Input in the form {Verb :: verb(), Subject :: pid(), DObject :: string()} 
%% Isn't that an action? ^
%sends it to person in the form of #action{}
%returns the result from person OR {error, {why, who}}
s_targetInput(Room, Input) ->
	Verb    = Action#action.verb,
	Subject = Action#action.subject,
	DObject = Action#action.object,
	DObject = hrThingToThing(Room, DObjectString),
		case DObject of
			{error, Reason} -> {error, {Reason, directObject}};
            %% what is this person module?
			_		-> person:targetInput(Subject, {Verb, Subject, DObject})
		end;
-spec(#room{}, thing_type()) -> #room{}.
s_addThing(Room, Thing) -> 
	NewRoom = Room#room{things=[Thing | AllThings]}
	propagateEvent(Room, {enter, Thing}),
	NewRoom.
%%%HELPER%%%
propagateEvent(Room, Event) ->
	lists:foreach(fun(Thing) -> thing:receiveEvent(Thing, Event) end, Room#room.things).
%get a thing in the room by its name. 
%possible errors are {error, Reason} where Reason is notInRoom or multipleMatches}
hrThingToThing(Room, ThingString) ->
	%normalize the string. get rid of case and whitespace. Maybe do some fuzzy matching someday.
	N = string:strip(string:to_lower(ThingString)),
	E = fun({_, _, Name}) -> string:equal(Name, N) end, %an equality function
	case lists:filter(E, Room#room.things) of
		[] -> {error, notInRoom};

		%multiple things in the room with the same name! Error (maybe handle more gracefully another time?)
		List when length(List) > 1 -> {error, multipleMatches};

		[Thing | _Cdr] 	-> Thing %hey, looky here. Found your thing!
	end.


