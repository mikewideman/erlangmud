%%%=============================================================================
%%% @doc A Player Character (PC).
%%% PCs are the user's avatar in the game. PCs are controlled by the user.
%%% @end
%%%=============================================================================

-module(player).
-export([start/1, performAction/2, recieveEventNotification/2]).
-include("character.hrl").
-include("room.hrl").
-include("action.hrl").

% start(Name) -> 
	% Player = spawn(player, play, [self(), Name]),
	% register(player, Player),
	% io:format("started the thread: ~p", [Player]).

% play(Pid, Name) -> play(Pid, Name, 0).

% play(Pid, Name, Curr_room) ->
	% receive 
		% {Pid, move} ->
			% io:format("~p is moving...", [Pid])
			
        % %{Start_room, player_command, {Action}} ->
		
            % % would Action have already been a completely parsed command which
            % % we know to be valid, or do we validate whether we can perform
            % % the command the user sent? e.g. if user says drop sword,
            % % but we don't have a sword, who figures this out, the Room or the
            % % Player?
        % %    {Results} = room:receiveAction(self(), Start_room, {Action},
        % %    
        % %    NewPlayerRecord = case {Results} of
        % %        {died} ->
        % %            {Pid, "I'm dead", Start_room};
        % %        {entered, NewRoom} ->
        % %            {Pid, Name, NewRoom};
        % %        _ ->
        % %            {Pid, Name, Start_room}
        % %    end
	% end,
	
    % % have play take the player record as its argument
	% play(Pid, Name, Curr_room).

% status() -> self().


%%% Just a different approach here, don't worry

-spec start ( Name      :: string()
            , Health    :: non_neg_integer() | 'default'
            , Attack    :: pos_integer() | 'default'
            , Room      :: pid()
            ) -> pid().
%% @doc Spawn a new player process, initializing it with the given name, health,
%% and room (in which it is located). The health and attack may be set to the
%% default values if desired. Returns the player pid.
%%
%% @see make_character/4
%% @end
start(Name, Health, Attack, Room) ->
    Player = make_character(Name, Health, Attack, Room),
    spawn(fun() -> main(Player) end).

-spec main  ( Player :: #character{}
            ) -> no_return().
%% @doc The main function of a Player. Loops forever so long as the Player does
%% not die.
%% @end
main(Player) when Player#character.health > 0 ->
    {CurrRoomPid, _CurrRoomId, _CurrRoomDesc} = Player#character.room,
    NewPlayer = receive
        Action when is_record(Action, action) ->
            %% Got a command to perform an action.
            % Verb = Action#action.verb,
            % Subject = Action#action.subject,
            % Object = Action#action.object,
            % %% @todo may want to send a payload, e.g. attack damage; see below
            ActionToSend = case Action#action.verb of
                attack ->
                    Damage = {damage, Player#character.attack},
                    Action#action{payload = [Damage]};
                enter ->
                    %% No need to add payload.
                    Action;
                _Other ->
                    Action
            end,
            Response = room:targetAction(CurrRoomPid, ActionToSend),
            case Response of
                % @todo each case in this block needs to return the NewPlayer
                {error, {notInRoom, ActionToSend#action.subject}} ->
                    %% This player is not in the room which told the player to
                    %% perform this action.
                    %% @todo Any need to change state?
                    Player;
                {error, {notInRoom, ActionToSend#action.object}} ->
                    %% The object of the action is not in the room which told
                    %% the player to perform this action.
                    % if is_record(ActionToSend#action.object, room_proc) ->
                        % %% An enter failed.
                    % if not is_record(ActionToSend#action.object, room_proc) ->
                        % %% Some other action failed.
                    % end,
                    %% @todo Any need to change state?
                    Player;
                {ok, ActionToSend} ->
                    % the action was successful
                    %% @todo
                    NewPlayer = case ActionToSend#action.verb of
                        attack ->
                            %% Attack succeeded.
                            %% @todo Any need to change state?
                            Player;
                        enter ->
                            %% Successfully entered new room.
                            Player#character{room = ActionToSend#action.object};
                        _Other ->
                            %% Some other successful action.
                            Player
                    end;
                _Other ->
                    Player
            end;
            % Response = room:targetAction(CurrRoomPid, Action),
            % case Verb of
                % % each case in this block needs to return the NewPlayer
                % attack ->
                    % % got a command to perfom attack action
                    % %% @todo handle response
                    % Player;
                % enter ->
                    % % got a command to perform enter action
                    % %% @todo handle response
                    % case Response of
                        % ok -> Player#character{room = Object};
                        % {error, _Something} ->
                            % %% do something else
                    % Player;
                % % look ->
                    % % % got a command to perform look action
                    % % Response = Room:look(CurrRoomPid),
                    % % %% @todo handle response, which is a list of Things
                    % % Player
                % _Other ->
                    % Player
            % end;
        Event when is_record(Event, event) -> %% @todo define event record
            %% Notified of a game event.
            % Participle = Event#event.participle,
            % Subject = Event#event.subject,
            % Object = Event#event.object,
            % %% @todo may want to receive a payload, e.g. damage taken
            % Payload = Event#event.payload,
            % %% @todo notify user of event (if someone else isn't doing that)
            case Event#event.participle of
                attacked when Object#character_proc.pid == self() ->
                    %% Notified of attack event.
                    % %% @todo decide how to get DamageTaken
                    % %% @todo consider a #payload or #payload_value record
                    {damage, DamageTaken} = lists:keysearch(damage, 1, Payload),
                    HealthRemaining = Player#character.health - DamageTaken,
                    if  HealthRemaining > 0 ->
                            Player#character{health = HealthRemaining};
                        HealthRemaining =< 0 ->
                            Player#character{health = 0}
                    end;
                entered when Subject#character_proc.pid == self() ->
                    %% Notified of entered event.
                    Player#character{room = Object};
                died when Subject#character_proc.pid /= self() ->
                    %% Notified of died event.
                    %% @todo Any need to change state?
                    Player;
                _Other ->
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
    %% @todo do anything else we might want here
    exit({died, Player}).

-spec performAction ( Player_Proc   :: #character_proc{}
                    , Action        :: #action{}
                    ) -> any().
%% @doc Tell a Player to perform an Action. The incoming Action may be modified
%% before it is sent to the Room.
%% @see room:targetAction/2
%% @end
performAction(Player_Proc, Action) ->
    Player_Proc#character_proc.pid ! Action.
    % is the process calling this function actually concerned with a return?
    % receive
        % Any -> Any
    % after Timeout ->
        % timeout
    % end.

-spec receiveEventNotification  ( Player_Proc   :: #character_proc
                                , Event         :: #event
                                ) -> any().
%% @doc Notify the Player of a game event. The Player may send an action in
%% response to this event, or it may ignore it.
%% @see room:broadcast/2
%% @end
% %% @todo consider naming
receiveEventNotification(Player_Proc, Event) ->
    Player_Proc#character_proc.pid ! Event.
    % is the process calling this function actually concerned with a return?
    % receive
        % Any -> Any
    % after Timeout ->
        % timeout
    % end.

-spec notifyRoomOfDeath ( Room_Proc     :: #room_proc
                        , Player_Proc   :: #character_proc
                        ) -> any().
%% @doc Tell the Room you are in that you have died.
%% @end
notifyRoomOfDeath(Room_Proc, Player_Proc) ->
    Event = make_event(died, Player_Proc, Room_Proc, []),
    room:broadcast(Room_Proc, Event, Player_Proc).
    % is the process calling this function actually concerned with a return?
    % receive
        % Any -> Any
    % after Timeout ->
        % timeout
    % end.
