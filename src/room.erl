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
        , addThing/2, leaveGame/2 ]).
-include("defs.hrl").

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

-type thing_proc() ::
      #character_proc{}
    % | #item_proc{}
    .
%% thing() is a type which represents the sorts of Things which can be inside
%% a room.

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(room,
    { id = make_ref()       :: reference()
    , description           :: string()
    , things = []           :: list(thing_proc())  %% @todo #item_proc{}
    , north_door = none     :: #room_proc{} | 'none'           %% @todo room types?
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
	Room_Proc#room_proc.pid ! {self(), targetAction, Action},
	receive_response().

-spec targetInput   ( Room :: #room_proc{}
                    , Input :: #input{}
                    ) -> 'ok' | {'error', atom() | tuple()}.
%% @doc Transform user input into a command which represents an Action. Sends
%% the Action to the player in order for it to perform the Action.
%% @see player:performAction/2
%% @end
targetInput(Room_Proc, Input) ->
	Room_Proc#room_proc.pid ! {self(), targetInput, Input},
	receive_response().

-spec look(Room_Proc :: #room_proc{}) -> list(thing_proc()).
%% @doc Get a list of all the things in the room.
%% @end
look(Room_Proc) ->
	Room_Proc#room_proc.pid ! {self(), look},
	receive_response().

-spec broadcast ( Room_Proc     :: #room_proc{}
                , Event         :: #event{}
                , Excluded      :: #character_proc{}
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
-spec addThing(Room_Proc :: #room_proc{}, Thing :: thing()) -> 'ok'.
addThing(Room_Proc, Thing) ->
	Room_Proc#room_proc.pid ! {self(), addThing, Thing},
	ok.

-spec leaveGame ( Room_Proc :: #room_proc{}
                , Player :: #character_proc{}
                ) -> {'error', atom()} | 'ok'.
%% @doc Notify the room that a player has left the game.
%% @see thing:receiveEventNotification/2
%% @end
leaveGame(Room_Proc, Player) ->
	Room_Proc#room_proc.pid ! {self(), leaveGame, Player},
	receive_response().

-spec main(#room{}) -> no_return().
%% @doc The main function of a room process. Loops forever.
%% @end
main(Room) ->
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
		{Sender, targetInput, Input} ->
			Sender ! s_targetInput(Room, Input),
			main(Room);
		{_, addThing, Thing} ->
			main(s_addThing(Room, Thing));
		{Sender, leaveGame, Player} ->
			case s_leaveGame(Room, Player) of
				{error, Reason} -> Sender ! {error, Reason}, main(Room);
				{ok, NewRoom} -> Sender ! ok, main(NewRoom)
			end;
		{'EXIT', Pid, _Reason} ->
			%not related to a public function
			%Something I likned to died. Act as if it exited
			%doesn't return anything, but may propagate an event.
			case lists:keyfind(Pid, #character_proc.pid, Room#room.things) of
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

-spec s_targetAction    ( Room :: #room{}
                        , Action :: #action{}
                        ) ->      {#room{}, {'ok', #action{}}}
                                | {#room{}, {'error', term()}}.
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
            if is_record(Action#action.object, room_proc)
            andalso Action#action.object#room_proc.id == Room#room.id ->
                %% Subject is trying to enter this room.
                Event = actionToEvent(Action),
                propagateEvent(Room, Event, Action#action.subject),
                { Room#room{things = [Action#action.subject | things]}
                , {ok, Action}},
                %% Update dungeon's knowledge of character location.
                dungeon ! {characterMoved, {Action#action.subject, Room}};
            not is_record(Action#action.object, room_proc) ->
                %% Object is not a room and Subject is not in this room.
                {Room, {error, {notInRoom, TheSubject}}}
            end;
		_Subject ->
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
                        {Room, {error, {notInRoom, TheObject}}};
                    Object ->
                        Event = actionToEvent(Action),
                        Propogate = fun(Thing) ->
                            if Thing /= Object ->
                                thing:receiveEventNotification(Object, Event);
                            Thing == Object ->
                                skip
                            end
                        end,
                        lists:foreach(Propogate, Room#room.things),
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
	end.

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
	ObjectString = hrThingToThing(Room, Object),
    case ObjectString of
        {error, Reason} ->
            {error, {Reason, directObject}};
            %should be in consistent form of 'ok' | {'error', Reason}
        _ ->
            Action = #action{verb = Verb, subject = Subject, object = Object},
            player:performAction(Subject, Action)
    end.
        
% @TODO: Allow things besides characters
-spec s_addThing(Room :: #room{}, Thing :: #character_proc{}) -> #room{}.
%% @doc Add a thing to the room. Propagate this as an event.
%% @end
s_addThing(Room, Thing) -> 
	NewRoom = Room#room{things=[Thing | Room#room.things]},
	link(Thing#character_proc.pid),
	propagateEvent(Room, #event{verb=enter, subject=Thing, object=#room_proc{pid=self(), id=Room#room.id, description=Room#room.description}}, Thing),
	NewRoom.

-spec s_leaveGame   ( Room :: #room{}
                    , Player :: #character_proc{}
                    ) -> {error, notInRoom} | {ok, #room{}}.
%% @doc Remove the leaving player from the room. Propogate this as an event.
%% @end
s_leaveGame(Room, Player)  -> 
	case lists:keysearch(Player, #room.things, Room#room.things) of 
		false -> {error, notInRoom};
		{value, Elem} ->
            Event = #event{verb = left, subject = Player, object = none},
			propagateEvent(Room, Event, Player),
			NewRoom = Room#room{things=lists:delete(Elem, Room#room.things)},
			{ok, NewRoom}
	end.
%%%HELPER%%%

-spec propagateEvent    ( Room          :: #room_proc{}
                        , Event         :: #event{}
                        , Excluded      :: #character_proc{} | 'none'
                        ) -> any().
%% @doc Notify every Thing in the room that Event has occurred, except for the
%% Excluded thing which caused the Event. Notify the dungeon of the event
%% occurring as well.
%% @end
propagateEvent(Room, Event, Excluded) ->
    Propogate = fun(Thing, ExcludedThing) ->
        if Thing /= ExcludedThing ->
            thing:receiveEventNotification(Thing, Event);
        Thing == ExcludedThing ->
            skip
        end
    end,
    lists:foreach(fun(T) -> Propogate(T, Excluded) end, Room#room.things),
    dungeon ! {event, {Event, Room}}.

-spec hrThingToThing    ( Room :: #room{}
                        , ThingString :: string()
                        ) ->      {'error', 'notInRoom'}
                                | {'error', 'multipleMatches'}
                                | thing().
%% @doc Get a thing in the room by its name.
%% possible errors are {error, Reason} where Reason is notInRoom or multipleMatches}
%% @end
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

-spec actionToEvent(Action :: #action{}) -> #event{}.
%% @doc Transform an Action into an Event.
%% @end
actionToEvent(Action) ->
	#event{ verb=Action#action.verb, 
		subject=Action#action.subject,
		object=Action#action.object,
		payload=Action#action.payload
		}.

-spec receive_response() -> 'timeout' | any().
%% @doc Wait for an incoming message and return it as a return value.
%% TODO: maybe check that it was the response we were expecting?
%% @end
receive_response() ->
	receive
		Any -> Any
	after 0 ->
		timeout
	end.
