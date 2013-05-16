%%%=============================================================================
%%% @doc A Player Character (PC).
%%% PCs are the user's avatar in the game. PCs are controlled by the user.
%%% @end
%%%=============================================================================

-module(player).
-extends(thing).
-export([start/4, performAction/2]).
-include("defs.hrl").

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(pc,
    { id = make_ref()   :: reference()
    , name              :: string()
    , health = 1        :: non_neg_integer()
    , attack = 1        :: pos_integer()
    %% @todo make item processes stay alive, but use exclusion
    % , inventory = []    :: list()
    , room              :: #room_proc{}
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start ( Name      :: string()
            , Health    :: non_neg_integer() | 'default'
            , Attack    :: pos_integer() | 'default'
            , Room      :: pid()
            ) -> #thing_proc{}.
%% @doc Spawn a new player process, initializing it with the given name, health,
%% and room (in which it is located). The health and attack may be set to the
%% default values if desired. Returns the player pid.
%%
%% @see make_character/4
%% @end
start(Name, default, default, Room) ->
    Player = #pc{name = Name, room = Room},
    h_start(Player);
start(Name, default, Attack, Room) ->
    Player = #pc{name = Name, attack = Attack, room = Room},
    h_start(Player);
start(Name, Health, default, Room) ->
    Player = #pc{name = Name, health = Health, room = Room},
    h_start(Player);
start(Name, Health, Attack, Room) ->
    Player = #pc{name = Name, health = Health, attack = Attack, room = Room},
    h_start(Player).

-spec h_start(Player :: #pc{}) -> #thing_proc{}.
%% @doc Given the constructed player record, start the main loop.
%% @end
h_start(Player) ->
    #thing_proc { pid = spawn(fun() -> main(Player) end)
                , id = Player#pc.id
                , name = Player#pc.name}.
                
-spec main  ( Player :: #pc{}) -> no_return().
%% @doc The main function of a Player. Loops forever so long as the Player does
%% not die.
%% @end
main(Player) when Player#pc.health > 0 ->
    NewPlayer = receive
        Action when is_record(Action, action) ->
            %% Got a command to perform an action.
            ActionToSend = case Action#action.verb of
                attack ->
                    %% Got a command to perform an attack action.
                    %% Payload includes damage done.
                    Action#action{payload = [{damage, Player#pc.attack}]};
                enter ->
                    %% Got a command to perform an enter action.
                    %% No need to add payload.
                    Action;
                take ->
                    %% Got a command to perform a take action.
                    %% No need to add payload.
                    Action;
                drink ->
                    %% Got a command to perform a drink action.
                    %% No need to add payload.
                    Action;
                _ReceivedVerb ->
                    %% Got some other action.
                    Action
            end,
            {Subject, Object} = {ActionToSend#action.subject, ActionToSend#action.object},
            case room:targetAction(Player#pc.room, ActionToSend) of
                {error, {notInRoom, Subject}} ->
                    %% This player is not in the room which told the player to
                    %% perform this action.
                    Player;
                    %% @todo bubble error message up to client
                {error, {notInRoom, Object}} ->
                    %% The object of the action is not in the room which told
                    %% the player to perform this action.
                    Player;
                    %% @todo bubble error message up to client
                {error, {noSuchDoor}} ->
                    %% The object of the action is a door which does not exist.
                    %% E.g., the door is 'none'.
                    Player;
                    %% @todo bubble error message up to client
                {ok, ActionToSend} ->
                    %% The action was successful.
                    case ActionToSend#action.verb of
                        attack ->
                            %% Successfully attacked.
                            Player;
                        enter ->
                            %% Successfully entered new room.
                            %% @todo this is redundant with the event receive
                            Player#pc{room = ActionToSend#action.object};
                        take ->
                            %% Successfully took a weapon.
                            Player;
                        drink ->
                            %% Successfully drank a potion.
                            Player;
                        _SentVerb ->
                            %% Some other successful action.
                            Player
                    end;
                _Response ->
                    Player
            end;
        Event when is_record(Event, event) ->  
            %% Notified of a game event.
            Player_Proc = #thing_proc   { pid = self()
                                        , id = Player#pc.id
                                        , name = Player#pc.name},
            case Event#event.verb of
                attack when Event#event.object#thing_proc.pid == self() ->
                    %% Notified of an attack event.
                    {damage, DamageTaken} = lists:keyfind(damage, 1, Event#event.payload),
                    HealthRemaining = Player#pc.health - DamageTaken,
                    NewPlayer1 = if HealthRemaining > 0 ->
                            Player#pc{health = HealthRemaining};
                    HealthRemaining =< 0 ->
                            Player#pc{health = 0}
                    end,
                    %% @todo bubble up remaining health to client
                    Status = #event { verb = display_status
                                    , subject = self()
                                    , object = NewPlayer1#pc.room
                                    , payload = [{health, NewPlayer1#pc.health}
                                                ,{attack, NewPlayer1#pc.attack}]
                                    },
                    room:broadcast(Player#pc.room, Status, Player_Proc),
                    NewPlayer1;
                heal when Event#event.object#thing_proc.pid == self() ->
                    %% Notified of a heal event.
                    {heal, HealthRestored} = lists:keyfind(heal, 1, Event#event.payload),
                    NewPlayer1 = Player#pc{health = Player#pc.health + HealthRestored},
                    %% @todo bubble up remaining health to client
                    Status = #event { verb = display_status
                                    , subject = self()
                                    , object = NewPlayer1#pc.room
                                    , payload = [{health, NewPlayer1#pc.health}
                                                ,{attack, NewPlayer1#pc.attack}]
                                    },
                    room:broadcast(Player#pc.room, Status, Player_Proc),
                    NewPlayer1;
				inc_attack when Event#event.object#thing_proc.pid == self() ->
                    %% Notified of a inc_attack event.
                    {inc_attack, Attackinc} = lists:keyfind(inc_attack, 1, Event#event.payload),
                    NewPlayer1 = Player#pc{attack = Player#pc.attack + Attackinc},
                    %% @todo bubble up current attack power to client
                    Status = #event { verb = display_status
                                    , subject = self()
                                    , object = NewPlayer1#pc.room
                                    , payload = [{health, NewPlayer1#pc.health}
                                                ,{attack, NewPlayer1#pc.attack}]
                                    },
                    room:broadcast(Player#pc.room, Status, Player_Proc),
                    NewPlayer1;
                enter when Event#event.subject#thing_proc.pid == self() ->
                    %% Notified of an entered event.
                    Player#pc{room = Event#event.object};
                died when Event#event.subject#thing_proc.pid /= self() ->
                    %% Notified of a died event.
                    %% @todo Any need to change state?
                    Player;
                _Verb ->
                    %% Notified of some other event.
                    Player
            end
    after 0 ->
        Player
    end,
    main(NewPlayer);
%% @doc Player dies and exits with reason {died, Player} where Player is the
%% #player{} record.
%% @end
main(Player) when Player#pc.health == 0 ->
    Player_Proc = #thing_proc   { pid = self()
                                , id = Player#pc.id
                                , name = Player#pc.name
                                },
    notifyRoomOfDeath(Player#pc.room, Player_Proc),
    %% @todo Need to do anything else before exiting?
    exit({died, Player_Proc}).

-spec performAction ( Player_Proc   :: #thing_proc{}
                    , Action        :: #action{}
                    ) -> any().
%% @doc Tell a Player to perform an Action. The incoming Action may be modified
%% before it is sent to the Room.
%% @see room:targetAction/2
%% @end
performAction(Player_Proc, Action) ->
    %% @todo Need a return value as to whether the action was supported or not.
    Player_Proc#thing_proc.pid ! Action.


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec notifyRoomOfDeath ( Room_Proc     :: #room_proc{}
                        , Player_Proc   :: #thing_proc{}
                        ) -> any().
%% @doc Tell the Room you are in that you have died.
%% @end
notifyRoomOfDeath(Room_Proc, Player_Proc) ->
    Event = #event  { verb  = died
                    , subject = Player_Proc
                    , object = Room_Proc
                    , payload = []
                    },
    %% @todo Need a return value?
    room:broadcast(Room_Proc, Event, Player_Proc).
