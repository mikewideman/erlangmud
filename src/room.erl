%%%=============================================================================
%%% @doc A dungeon room.
%%% Rooms are the smallest unit of geographical space in the dungeon. Rooms
%%% are connected to each other by doors (links to other rooms). Rooms may
%%% contain characters and/or items, or nothing at all. Rooms act as the liaison
%%% for communication between characters and other entities of the game by
%%% directing messages (e.g. game actions) to the concerned parties, as well as
%%% the publisher of game event messages which are sent to users.
%%%
%%% Each room assumes that the atom 'dungeon' is registered as the pid of the
%%% dungeon module. @see dungeon
%%% @end
%%%=============================================================================

-module(room).
-export([ start/1, targetAction/2, look/1, targetInput/2, broadcast/3
        , addThing/2, leaveGame/2, makeDoor/3, removeDoor/3 ]).
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
    , things = []           :: list(#thing_proc{})
    , north_door = none     :: #room_proc{} | 'none'
    , east_door = none      :: #room_proc{} | 'none'
    , south_door = none     :: #room_proc{} | 'none'
    , west_door = none      :: #room_proc{} | 'none'
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(string()) -> #room_proc{}.
%% @doc Spawn a new room process, initializing it with a given Description.
%% Returns the room pid.
%% @todo what else will this function do?
%% @end
start(Description) ->
    Room = #room{description = Description},
    % @todo link rooms, add content to rooms
    #room_proc  { pid = spawn_link(fun() -> main(Room), process_flag(trap_exit, true) end)
                , id = Room#room.id
                , description = Description}.

-spec targetAction  ( Room_Proc :: #room_proc{}
                    , Action    :: #action{}
                    ) ->      {'ok', #action{}}     %% Positive ACK
                            | {'error', term()}.    %% Negative ACK
%% @doc Target an action to the action's direct object. If the action is valid,
%% an Event will be propagated to everything in the room and an ok message will
%% be returned. If not, an error message will be returned.
%%
%% @see player:performAction/2
%% @see thing:receiveEventNotification/2
%% @end
targetAction(Room_Proc, Action) -> 
	Id = make_ref(),
	Room_Proc#room_proc.pid ! {self(), targetAction, Id, Action},
	receive_response(Id).

-spec targetInput   ( Room  :: #room_proc{}
                    , Input :: #input{}
                    ) -> 'ok' | {'error', atom() | tuple()}.
%% @doc Transform user input into a command which represents an Action. Sends
%% the Action to the player in order for it to perform the Action.
%% @see player:performAction/2
%% @end
targetInput(Room_Proc, Input) ->
	Id = make_ref(),
	Room_Proc#room_proc.pid ! {self(), targetInput,Id, Input},
	receive_response(Id).

-spec look(Room_Proc :: #room_proc{}) -> list(#thing_proc{}).
%% @doc Get a list of all the things in the room.
%% @end
look(Room_Proc) ->
	Id = make_ref(),
	Room_Proc#room_proc.pid ! {self(), look, Id},
	receive_response(Id).

-spec broadcast ( Room_Proc     :: #room_proc{}
                , Event         :: #event{}
                , Excluded      :: #thing_proc{}
                ) -> any().
%% @doc Broadcast the occurrence of an event to everything in the room.
%% @see thing:receiveEventNotification/2
%% @end
broadcast(Room_Proc, Event, Excluded) ->
	Room_Proc#room_proc.pid ! {self(), broadcast, Event, Excluded}.

%% @doc Add a thing to the room. Propagate the occurrence of this event to
%% everything in the room.
%% @see thing:receiveEventNotification/2
%% @end
-spec addThing(Room_Proc :: #room_proc{}, Thing :: #thing_proc{}) -> 'ok'.
addThing(Room_Proc, Thing) ->
	Room_Proc#room_proc.pid ! {self(), addThing, Thing},
	ok.

-spec leaveGame ( Room_Proc     :: #room_proc{}
                , Player        :: #thing_proc{}
                ) -> {'error', atom()} | 'ok'.
%% @doc Notify the room that a player has left the game.
%% @see thing:receiveEventNotification/2
%% @end
leaveGame(Room_Proc, Player) ->
	Id = make_ref(),
	Room_Proc#room_proc.pid ! {self(), leaveGame, Id, Player},
	receive_response(Id).

-spec makeDoor  ( Direction :: 'north' | 'east' | 'south' | 'west'
                , ThisRoom  :: #room_proc{}
                , OtherRoom :: #room_proc{}
                ) -> error().
%% @doc Connect a Room to another Room (unidirectionally).
%% @end
makeDoor(Direction, ThisRoom, OtherRoom) ->
	Id = make_ref(),
    ThisRoom#room_proc.pid ! {self(), makeDoor, Id, Direction, OtherRoom},
    receive_response(Id).
    
-spec removeDoor    ( Direction :: 'north' | 'east' | 'south' | 'west'
                    , ThisRoom  :: #room_proc{}
                    , OtherRoom :: #room_proc{}
                    ) ->    { reference()
                            , {ok} 
                            | {error, noDoorToRemove} 
                            | {error, incorrectDoorToRemove}}.
%% @todo move these error meessages to error().
%% @doc Remove a Room's connection to another Room (unidirectionally).
%% @end
removeDoor(Direction, ThisRoom, OtherRoom) ->
	Id = make_ref(),
    ThisRoom#room_proc.pid ! {self(), removeDoor, Id, Direction, OtherRoom},
    receive_response(Id).

-spec main(#room{}) -> no_return().
%% @doc The main function of a room process. Loops forever.
%% @end
main(Room) ->
	receive
		{Sender, targetAction, Id, Action} when is_record(Action, action) ->
		    	{NewRoom, Message} = s_targetAction(Room, Action),
				Sender ! {Id, Message},
				main(NewRoom);
		{Sender, look, Id}		->
			Sender ! {Id, Room#room.things}, % @todo turn into game event or something
			main(Room);
		{_, broadcast, Event, Excluded} ->
			propagateEvent(Room, Event, Excluded),
			main(Room);
		{Sender, targetInput,Id, Input} ->
			Sender ! {Id, s_targetInput(Room, Input)},
			main(Room);
		{_, addThing, Thing} ->
			main(s_addThing(Room, Thing));
		{Sender, leaveGame, Id, Player} ->
			Rm = case s_leaveGame(Room, Player) of
				{error, Reason} -> Sender ! {Id, {error, Reason}}, Room;
				{ok, NewRoom} -> Sender ! {Id, ok}, NewRoom
			end,
			main(Rm);
		{Sender, makeDoor, Id, Direction, OtherRoom} ->
		    NewRoom = case s_makeDoor(Direction, Room, OtherRoom) of
                {error, Reason} ->
                    Sender ! {Id, {error, Reason}},
                    Room;
                {ok, NewRoom1} ->
                    Sender ! {Id, {ok}},
                    NewRoom1
            end,
            main(NewRoom);
        {Sender, removeDoor, Id, Direction, OtherRoom} ->
            NewRoom = case s_removeDoor(Direction, Room, OtherRoom) of
                {error, Reason} ->
                    Sender ! {Id, {error, Reason}},
                    Room;
                {ok, NewRoom1} ->
                    Sender ! {Id, {ok}},
                    NewRoom1
            end,
            main(NewRoom);
		{'EXIT', Pid, _Reason} ->
			%not related to a public function
			%Something I likned to died. Act as if it exited
			%doesn't return anything, but may propagate an event.
			case lists:keyfind(Pid, #thing_proc.pid, Room#room.things) of
				false ->
					notInRoom;
				Thing ->
					s_leaveGame(Room, Thing)
			end
			
	after 0 -> main(Room)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec s_targetAction    ( Room      :: #room{}
                        , Action    :: #action{}
                        ) ->  {#room{}, {'ok', #action{}}}
                            | {#room{}, {'error', term()}}.
%% @doc Validate the Action and turn it into an Event, and notify every thing
%% in the room that the Event occurred. Acknowledge the validity of the Action
%% to the character that caused it as well. If necessary, update the state
%% of the room. Return the room state and the acknowledgement message.
%% @end
s_targetAction(Room, Action) when Action#action.object /= none ->
	%% Check for Subject's presence in room.
    %% @todo Update if subjects are capable of not being characters.
    TheSubject = lists:keyfind  ( Action#action.subject#thing_proc.id
                                , #thing_proc.id
                                , Room#room.things),
	case TheSubject of
		false ->
            %% Subject not in room.
            %% Check if Object is this room, this means character is entering.
            if is_record(Action#action.object, room_proc)
            andalso Action#action.object#room_proc.id == Room#room.id ->
                %% Subject is trying to enter this room.
                Event = actionToEvent(Action),
                propagateEvent(Room, Event, Action#action.subject),
                %% Update dungeon's knowledge of character location.
                dungeon !   { characterMoved
                            , {Action#action.subject, Action#action.object}},
                { Room#room{things = [Action#action.subject | things]}
                , {ok, Action}};
            not is_record(Action#action.object, room_proc) ->
                %% Object is not a room and Subject is not in this room.
                {Room, {error, {notInRoom, TheSubject}}}
            end;
		_Subject ->
            %% Subject is in room.
            %% Check whether Object is a character or a room.
            if is_record(Action#action.object, thing_proc) ->
                %% Object is a character.
                %% Check for Object's presence in room.
                TheObject = lists:keyfind   ( Action#action.object#thing_proc.id
                                            , #thing_proc.id
                                            , Room#room.things),
                case TheObject of
                    false ->
                        {Room, {error, {notInRoom, TheObject}}};
                    _Object ->
                        Event = actionToEvent(Action),
                        % Propogate = fun(Thing) ->
                            % if Thing /= Object ->
                                % thing:receiveEventNotification(Object, Event);
                            % Thing == Object ->
                                % skip
                            % end
                        % end,
                        % lists:foreach(Propogate, Room#room.things),
                        propagateEvent(Room, Event, TheSubject),
                        {Room, {ok, Action}}
                end;
            is_record(Action#action.object, room_proc) ->
                %% Object is a room. Character is trying to enter another room.
                %% @todo Update if other actions besides enter can be done to rooms.
                %% Check for matching door in room.
                if Action#action.object == Room#room.north_door
                orelse Action#action.object == Room#room.east_door
                orelse Action#action.object == Room#room.south_door
                orelse Action#action.object == Room#room.west_door ->
                    %% Door is in room. Tell next room that player is entering.
                    room:targetAction(Action#action.object, Action);
                Action#action.object /= Room#room.north_door
                andalso Action#action.object /= Room#room.east_door
                andalso Action#action.object /= Room#room.south_door
                andalso Action#action.object /= Room#room.west_door ->
                    %% Door is not in room.
                    {Room, {error, {notInRoom, Action#action.object}}}
                end
            end
	end;
s_targetAction(Room, Action) when Action#action.object == none ->
    {Room, {error, {noSuchDoor}}}.

-spec s_targetInput ( Room :: #room_proc{}
                    , Input :: #input{}
                    ) -> {'error', {term(), 'directObject'}} | none().
%% @doc Targets input to a player from the user, converting the direct object's
%% name from a hr string to a thing type in the process.
%% (IE it converts Input to Action)
%% sends it to player in the form of #action{}
%% @end
s_targetInput(Room, Input) ->
	Verb = Input#input.verb,
	Subject = Input#input.subject,
	Object = Input#input.object,
	ObjectThing = hrThingToThing(Room, Object),
    case ObjectThing of
        {error, Reason} ->
            {error, {Reason, directObject}};
            %should be in consistent form of 'ok' | {'error', Reason}
        _ ->
            Action = #action{verb = Verb, subject = Subject, object = ObjectThing},
            player:performAction(Subject, Action)
    end.
        
% @TODO: Allow things besides characters
-spec s_addThing(Room :: #room{}, Thing :: #thing_proc{}) -> #room{}.
%% @doc Add a thing to the room. Propagate this as an event.
%% @end
s_addThing(Room, Thing) -> 
	NewRoom = Room#room{things=[Thing | Room#room.things]},
	link(Thing#thing_proc.pid),
	propagateEvent(Room, #event{verb=enter, subject=Thing, object=#room_proc{pid=self(), id=Room#room.id, description=Room#room.description}}, Thing),
	NewRoom.

-spec s_leaveGame   ( Room :: #room{}
                    , Thing :: #thing_proc{}
                    ) -> {error, notInRoom} | {ok, #room{}}.
%% @doc Remove the leaving thing from the room. Propogate this as an event.
%% @end
s_leaveGame(Room, Thing)  -> 
	case lists:keyfind(Thing#thing_proc.id, #thing_proc.id, Room#room.things) of 
		false -> {error, notInRoom};
		Elem when is_record(Elem, thing_proc) ->
            Room_Proc = #room_proc  { pid = self()
                                    , id = Room#room.id
                                    , description = Room#room.description
                                    },
            Event = #event{verb = left, subject = Thing, object = Room_Proc},
			propagateEvent(Room, Event, Thing),
			NewRoom = Room#room{things=lists:delete(Elem, Room#room.things)},
			{ok, NewRoom}
	end.

-spec s_makeDoor    ( Direction :: 'north' | 'east' | 'south' | 'west'
                    , ThisRoom  :: #room{}
                    , OtherRoom :: #room_proc{}
                    ) -> {ok, #room{}} | {error, doorAlreadyMade | otherDoorAlreadyMade}.
%% @doc Add a door to another room.
%% @end
s_makeDoor(Direction, ThisRoom, OtherRoom) ->
    case Direction of
        north ->
            if ThisRoom#room.north_door == none ->
                {ok, ThisRoom#room{north_door = OtherRoom}};
            ThisRoom#room.north_door == OtherRoom ->
                {error, doorAlreadyMade};
            ThisRoom#room.north_door /= none
            andalso ThisRoom#room.north_door /= OtherRoom ->
                {error, otherDoorAlreadyMade}
            end;
        east ->
            if ThisRoom#room.east_door == none ->
                {ok, ThisRoom#room{east_door = OtherRoom}};
            ThisRoom#room.east_door == OtherRoom ->
                {error, doorAlreadyMade};
            ThisRoom#room.east_door /= none
            andalso ThisRoom#room.east_door /= OtherRoom ->
                {error, otherDoorAlreadyMade}
            end;
        south ->
            if ThisRoom#room.south_door == none ->
                {ok, ThisRoom#room{south_door = OtherRoom}};
            ThisRoom#room.south_door == OtherRoom ->
                {error, doorAlreadyMade};
            ThisRoom#room.south_door /= none
            andalso ThisRoom#room.south_door /= OtherRoom ->
                {error, otherDoorAlreadyMade}
            end;
        west ->
            if ThisRoom#room.west_door == none ->
                {ok, ThisRoom#room{west_door = OtherRoom}};
            ThisRoom#room.west_door == OtherRoom ->
                {error, doorAlreadyMade};
            ThisRoom#room.west_door /= none
            andalso ThisRoom#room.west_door /= OtherRoom ->
                {error, otherDoorAlreadyMade}
            end
    end.

-spec s_removeDoor  ( Direction :: 'north' | 'east' | 'south' | 'west'
                    , ThisRoom  :: #room{}
                    , OtherRoom :: #room_proc{}
                    ) -> {ok, #room{}} | {error, noDoorToRemove | incorrectDoorToRemove}.
%% @doc Remove the door to another room.
%% @end
s_removeDoor(Direction, ThisRoom, OtherRoom) ->
    case Direction of
        north ->
            if ThisRoom#room.north_door == OtherRoom ->
                {ok, ThisRoom#room{north_door = OtherRoom}};
            ThisRoom#room.north_door == none ->
                {error, noDoorToRemove};
            ThisRoom#room.north_door /= none
            andalso ThisRoom#room.north_door /= OtherRoom ->
                {error, incorrectDoorToRemove}
            end;
        east ->
            if ThisRoom#room.east_door == OtherRoom ->
                {ok, ThisRoom#room{east_door = OtherRoom}};
            ThisRoom#room.east_door == none ->
                {error, noDoorToRemove};
            ThisRoom#room.east_door /= none
            andalso ThisRoom#room.east_door /= OtherRoom ->
                {error, incorrectDoorToRemove}
            end;
        south ->
            if ThisRoom#room.south_door == OtherRoom ->
                {ok, ThisRoom#room{south_door = OtherRoom}};
            ThisRoom#room.south_door == none ->
                {error, noDoorToRemove};
            ThisRoom#room.south_door /= none
            andalso ThisRoom#room.south_door /= OtherRoom ->
                {error, incorrectDoorToRemove}
            end;
        west ->
            if ThisRoom#room.west_door == OtherRoom ->
                {ok, ThisRoom#room{west_door = OtherRoom}};
            ThisRoom#room.west_door == none ->
                {error, noDoorToRemove};
            ThisRoom#room.west_door /= none
            andalso ThisRoom#room.west_door /= OtherRoom ->
                {error, incorrectDoorToRemove}
            end
    end.
                    
%%%HELPER%%%

-spec propagateEvent    ( Room          :: #room{}
                        , Event         :: #event{}
                        , Excluded      :: #thing_proc{} | 'none'
                        ) -> any().
%% @doc Notify every Thing in the room that Event has occurred, except for the
%% Excluded thing which caused the Event. Notify the dungeon of the event
%% occurring as well.
%% @end
propagateEvent(Room, Event, Excluded) ->
    Propagate = fun(Thing, ExcludedThing) ->
        if Thing /= ExcludedThing ->
            thing:receiveEventNotification(Thing, Event);
        Thing == ExcludedThing ->
            skip
        end
    end,
    lists:foreach(fun(T) -> Propagate(T, Excluded) end, Room#room.things),
    Room_Proc = #room_proc  { pid = self()
                            , id = Room#room.id
                            , description = Room#room.description},
    dungeon ! {event, {Event, Room_Proc}}.

-spec hrThingToThing    ( Room :: #room{}
                        , ThingString :: string()
                        ) ->      {'error', 'notInRoom'}
                                | {'error', 'multipleMatches'}
                                | #thing_proc{}
                                | #room_proc{}.
%% @doc Get a thing in the room by its name.
%% possible errors are {error, Reason} where Reason is notInRoom or multipleMatches}
%% @end
hrThingToThing(Room, "north door") -> Room#room.north_door;
hrThingToThing(Room, "south door") -> Room#room.south_door ;
hrThingToThing(Room, "east door") -> Room#room.east_door ;
hrThingToThing(Room, "west door") -> Room#room.west_door;
hrThingToThing(Room, ThingString) ->
	%normalize the string. get rid of case and whitespace. Maybe do some fuzzy matching someday.
	N = string:strip(string:to_lower(ThingString)),
	E = fun(Thing) -> string:equal(string:to_lower(Thing#thing_proc.name), N) end, %an equality function
	case lists:filter(E, Room#room.things) of
		[] -> {error, notInRoom};

		%multiple things in the room with the same name! Error (maybe handle more gracefully another time?)
		List when length(List) > 1 -> {error, multipleMatches};

		[Thing | _Cdr] 	-> Thing %hey, looky here. Found your thing!
	end.

-spec actionToEvent(Action :: #action{}) -> #event{}.
%% @doc Transform an Action into an Event.
%% @end
actionToEvent(Action) ->
	#event{ verb=Action#action.verb, 
		subject=Action#action.subject,
		object=Action#action.object,
		payload=Action#action.payload
		}.

-spec receive_response(CallId :: reference()) -> 'timeout' | any().
%% @doc Wait for an incoming message and return it as a return value.
%% pass a unique reference. It will assume the return message (from the main loop) returns in the form {id, Result} where id is the same as the one passed into the response. This will only receive a response with a matching id.
%% @end
receive_response(CallId) ->
	receive
		{CallId, Any} -> Any
	after 1000 ->
		timeout
	end.
