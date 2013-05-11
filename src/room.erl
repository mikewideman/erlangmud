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
-export([start/1, targetAction/2, look/1, targetInput/2, broadcast/2, addThing/2, leaveGame/2).
-include("defs.hrl").

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(room,
    { id = make_ref()       :: reference()
    , description           :: string()
    , things = []           :: list(#character_proc{})  %% @todo #item_proc{}
    , north_door = none     :: pid() | 'none'           %% @todo room types?
    , east_door = none      :: pid() | 'none'
    , south_door = none     :: pid() | 'none'
    , west_door = none      :: pid() | 'none'
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(string()) -> pid().
%% @doc Spawn a new room process, initializing it with a given Description.
%% Returns the room pid.
%% @todo what else will this function do?
%% @end
start(Description) ->
    Room = #room{description = Description},
    % @todo link rooms, add content to rooms
    spawn(fun() -> main(Room) end).

%functions
%% @todo change to fit action() type
%%-spec targetAction(pid(), boolean(), action()) -> any().

%Target an action to the action's direct object. Sends it to thing:handleAction which returns an event. If successful, the event is propagated by sending it to thing:receiveEvent.
%on failure, returns {error, Reason} where Reason can be {notInRoom, What} where What is directObject or subject. Or Reason can be something from the Thing, or whatever other errors  might come up. Maybe there should be an error() type?
-spec targetAction  ( Room_Proc :: #room_proc{}
                    , Action    :: #action{}
                    ) ->      {'ok', #action{}}     %% Positive ACK
                            | {'error', term()}.    %% Negative ACK
targetAction(Room_Proc, Action) -> 
	Room_Proc#room_proc.pid ! {self(), targetAction, Action},
	receive
        Response -> Response
    %% @todo timeout
    end.
%Targets input to a person from the user, converting the direct object's name from a hr string to a thing type in the process.
%(IE it converts Input to Action)
%Input in the form {Verb :: verb(), Subject :: pid(), DObject :: string()} 
%sends it to player in the form of #action{}
%returns the result from player OR {error, {why, who}}
-spec targetInput(#room_proc{}, #input{}, timeout())-> 'ok' | {'error', atom() | tuple()}.
targetInput(Room_Proc, Input, Timeout) when Input#action.type == input->
	Room_Proc#room_proc.pid ! {self(), targetInput, Input, Timeout},
	receive_response().

%get a list of all the things in the room
-spec look(#room_proc{}) -> [thing_type()].
look(Room_Proc) ->
	Room_Proc#room_proc.pid ! {self(), look},
	receive_response().
%Send everyone an arbitrary message using thing:receiveEvent (should be an event, if our defined format made any sense.) No return value.
%I'm using the event format {event, BY, VERB, ON, WITH}
%we should handel hr message text somewhere else
-spec broadcast ( Room_Proc     :: #room_proc{}
                , Event         :: #event{}
                , Excluded      :: #character_proc{}
                ) -> any().
broadcast(Room_Proc, Event, Excluded) ->
	Room_Proc#room_proc.pid ! {self(), broadcast, Event, Excluded}.

%add a thing to the room (enter it, spawn it, whatever you want to call it). 
%an event will be propagated
-spec addThing(#room_proc{}, thing_type()) -> 'ok'.
addThing(Room_Proc, Thing) ->
	Room_Proc#room_proc.pid ! {self(), addThing, Thing},
	ok.

-spec leaveGame(#room_proc{}, player_type()) -> {'error', atom()} | 'ok'.
leaveGame(Room_Proc, Player) ->
	Room_Proc#room_proc.pid ! {self(), leaveGame, Player},
	receive_response().

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
            {NewRoom, Message} = s_targetAction(Room, Action),
			Sender ! Message,
			main(NewRoom);
		{Sender, look}		->
			Sender ! Room#room.things, % @todo turn into game event or something
			main(Room);
		{_, broadcast, Event, Excluded} ->
			propagateEvent(Room, Event, Excluded),
			main(Room);
		{Sender, targetInput, Input, Timeout} ->
			Sender ! s_targetInput(Room, Input, Timeout),
			main(Room);
		{_, addThing, Thing} ->
			main(s_addThing(Room, Thing));
		{Sender, leaveGame, Player} ->
			case s_leaveGame(Room, Player) of
				{error, Reason} -> Sender ! {error, Reason}, main(Room);
				{ok, NewRoom} -> Sender ! ok, main(NewRoom)
			end
	after 0 -> main(Room)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec s_targetAction    ( Room :: #room{}
                        , Action :: #action{}
                        ) ->      {#room{}, {'ok', #action{}}
                                | {#room{}, {'error', term()}.
%% @doc Validate the Action and turn it into an Event, and notify every thing
%% in the room that the Event occurred. Acknowledge the validity of the Action
%% to the character that caused it as well. If necessary, update the state
%% of the room. Return the room state and the acknowledgement message.
%% @end
s_targetAction(Room, Action) ->
	%% Check for Subject's presence in room.
    %% @todo Update if subjects are capable of not being characters.
    TheSubject = lists:keyfind  ( Action#action.subject#character_proc.id
                                , #character_proc.id
                                , Room#room.things),
	case TheSubject of
		false ->
            %% Subject not in room.
            %% Check if Object is this room, this means character is entering.
            if is_record(Action#action.object, #room_proc)
            andalso Action#action.object#room_proc.id == Room#room.id ->
                %% Subject is trying to enter this room.
                Event = actionToEvent(Action),
                propogateEvent(Room, Event, Action#action.subject),
                { Room#room{things = [Action#action.subject | things]}
                , {ok, Action}}
            if not is_record(Action#action.object, #room_proc) ->
                %% Object is not a room and Subject is not in this room.
                {Room, {error, {notInRoom, TheSubject}}}
            end;
		Subject ->
            %% Subject is in room.
            %% Check whether Object is a character or a room.
            if is_record(Action#action.object, character_proc) ->
                %% Object is a character.
                %% Check for Object's presence in room.
                TheObject = lists:keyfind   ( Action#action.object#character_proc.id
                                            , #character_proc.id
                                            , Room#room.things),
                case TheObject of
                    false ->
                        {Room, {error, {notInRoom, Object}}};
                    Object ->
                        Event = actionToEvent(Action),
                        Propogate = fun(Thing) ->
                            if Thing /= Object ->
                                player:receiveEventNotification(Object, Event);
                            if Thing == Object ->
                                skip
                            end
                        end,
                        lists:foreach(Propogate, Room#room.things),
                        {Room, {ok, Action}}
                end
            if is_record(Action#action.object, room_proc) ->
                %% Object is a room. Character is trying to enter another room.
                %% @todo Update if other actions besides enter can be done to rooms.
                %% Check for matching door in room.
                if Action#action.object == Room#room.north_door
                orelse Action#action.object == Room#room.east_door
                orelse Action#action.object == Room#room.south_door
                orelse Action#action.object == Room#room.west_door ->
                    %% Door is in room. Tell next room that player is entering.
                    room:targetAction(Object, Action),
                if Action#action.object /= Room#room.north_door
                andalso Action#action.object /= Room#room.east_door
                andalso Action#action.object /= Room#room.south_door
                andalso Action#action.object /= Room#room.west_door ->
                %% @todo LYSE says to do this long stuff, but if true would be shorter...
                    %% Door is not in room.
                    {Room, {error, {notInRoom, Object}}}
                end
            end
	end.

%Targets input to a player from the user, converting the direct object's name from a hr string to a thing type in the process.
%(IE it converts Input to Action)
%Input in the form {Verb :: verb(), Subject :: pid(), DObject :: string()} 
%% Isn't that an action? ^
%sends it to player in the form of #action{}
%returns the result from player OR {error, {why, who}}
s_targetInput(Room, Input, Timeout) ->
	Verb    = Action#input.verb,
	Subject = Action#input.player,
	DObject = Action#input.object,
	DObject = hrThingToThing(Room, DObjectString),
		case DObject of
			{error, Reason} -> {error, {Reason, directObject}};
						%should be in consistent form of 'ok' | {'error', Reason}
			_		->	player:performAction(Subject, make_action(Verb, Subject, Object), Timeout) %no idea what the timeout should be.
		end.
-spec(#room{}, thing_type()) -> #room{}.
s_addThing(Room, Thing) -> 
	NewRoom = Room#room{things=[Thing | AllThings]}
	propagateEvent(Room, #event{verb=enter, subject=Thing, object=#room_proc{pid=Room#room.pid, id=Room#room.id, description=Room#room.description}),
	NewRoom.
s_leaveGame(Room, Player)  -> 
	case lists:keysearch(Player, #room.things, Room#room.things) of 
		false -> {error, notInRoom};
		{value, Elem} ->
			propagateEvent(make_event(left, Player, none)),
			NewRoom = Room#room{things=lists:delete(Elem, Room#room.things)},
			{ok, NewRoom}
	end.
%%%HELPER%%%

-spec propogateEvent    ( Room          :: #room_proc{}
                        , Event         :: #event{}
                        , Excluded      :: #character_proc{}
                        ) -> 'ok'.
%% @doc Notify every Thing in the room that Event has occurred, except for the
%% Excluded thing which caused the Event.
%% @end
propagateEvent(Room, Event, Excluded) ->
    Propogate = fun(Thing, ExcludedThing) ->
        if Thing /= ExcludedThing ->
            player:receiveEventNotification(Thing, Event);
        if Thing == ExcludedThing ->
            skip
        end
    end,
    lists:foreach(fun(T) -> Propogate(T, Excluded) end, Room#room.things).
    
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


actionToEvent(Action) ->
	#event{ verb=Action#action.verb, 
		subject=Action#action.subject,
		object=Action#action.object,
		payload=Action#action.payload
		}.
