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

-record(character,
    { id = make_ref()   :: reference()
    , name              :: string()
    , health = 1        :: non_neg_integer()
    , attack = 1        :: pos_integer()
    , inventory = []    :: list()               %% @todo: define item type
    , room              :: #room_proc{}
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start ( Name      :: string()
            , Health    :: non_neg_integer() | 'default'
            , Attack    :: pos_integer() | 'default'
            , Room      :: pid()
            ) -> #character_proc{}.
%% @doc Spawn a new player process, initializing it with the given name, health,
%% and room (in which it is located). The health and attack may be set to the
%% default values if desired. Returns the player pid.
%%
%% @see make_character/4
%% @end
start(Name, Health, Attack, Room) ->
    Player = #character { name = Name
                        , health = Health
                        , attack = Attack
                        , room = Room
                        },
    #character_proc { pid = spawn(fun() -> main(Player) end)
                    , id = Player#character.id
                    , name = Name}.

-spec main  ( Player :: #character{}
            ) -> no_return().
%% @doc The main function of a Player. Loops forever so long as the Player does
%% not die.
%% @end
main(Player) when Player#character.health > 0 ->
    NewPlayer = receive
        Action when is_record(Action, action) ->
            %% Got a command to perform an action.
            ActionToSend = case Action#action.verb of
                attack ->
                    %% Got a command to perform an attack action.
                    Damage = {damage, Player#character.attack},
                    Action#action{payload = [Damage]};
                enter ->
                    %% No need to add payload.
                    Action;
                _ReceivedVerb ->
                    %% Got some other action.
                    Action
            end,
            {Subject, Object} = {ActionToSend#action.subject, ActionToSend#action.object},
            case room:targetAction(Player#character.room, ActionToSend) of
                {error, {notInRoom, Subject}} ->
                    %% This player is not in the room which told the player to
                    %% perform this action.
                    %% @todo Any need to change state?
                    Player;
                {error, {notInRoom, Object}} ->
                    %% The object of the action is not in the room which told
                    %% the player to perform this action.
                    %% @todo Any need to change state?
                    Player;
                {ok, ActionToSend} ->
                    % The action was successful.
                    %% @todo
                    case ActionToSend#action.verb of
                        attack ->
                            %% Attack succeeded.
                            %% @todo Any need to change state?
                            Player;
                        enter ->
                            %% Successfully entered new room.
                            Player#character{room = ActionToSend#action.object};
                        _SentVerb ->
                            %% Some other successful action.
                            Player
                    end;
                _Response ->
                    Player
            end;
        Event when is_record(Event, event) ->  
            %% Notified of a game event.
            case Event#event.verb of
                attack when Event#event.object#character_proc.pid == self() ->
                    %% Notified of an attack event.
                    {damage, DamageTaken} = lists:keysearch(damage, 1, Event#event.payload),
                    HealthRemaining = Player#character.health - DamageTaken,
                    if  HealthRemaining > 0 ->
                            Player#character{health = HealthRemaining};
                        HealthRemaining =< 0 ->
                            Player#character{health = 0}
                    end;
                enter when Event#event.subject#character_proc.pid == self() ->
                    %% Notified of an entered event.
                    Player#character{room = Event#event.object};
                died when Event#event.subject#character_proc.pid /= self() ->
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
main(Player) when Player#character.health == 0 ->
    notifyRoomOfDeath   ( Player#character.room
                        , #character_proc   { pid = self()
                                            , id = Player#character.id
                                            , name = Player#character.name
                                            }
                        ),
    %% @todo Need to do anything else before exiting?
    %% @todo This exposes the #player{} record, should use proc.
    exit({died, Player}).

-spec performAction ( Player_Proc   :: #character_proc{}
                    , Action        :: #action{}
                    ) -> any().
%% @doc Tell a Player to perform an Action. The incoming Action may be modified
%% before it is sent to the Room.
%% @see room:targetAction/2
%% @end
performAction(Player_Proc, Action) ->
    %% @todo Need a return value as to whether the action was supported or not.
    Player_Proc#character_proc.pid ! Action.


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec notifyRoomOfDeath ( Room_Proc     :: #room_proc{}
                        , Player_Proc   :: #character_proc{}
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
