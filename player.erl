%%%=============================================================================
%%% @doc A Player Character (PC).
%%% PCs are the user's avatar in the game. PCs are controlled by the user.
%%% @end
%%%=============================================================================

-module(player).
-export([start/1, performAction/2]).
-include("character.hrl").
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

-spec start(string(), non_neg_integer(), pos_integer(), pid()) -> pid().
%% @doc Spawn a new player process, initializing it with the given name, health,
%% and room (in which it is located). Returns the player pid.
%% @end
start(Name, Health, Attack, Room) ->
    Player = make_character(Name, Health, Attack, Room),
    spawn(fun() -> main(Player) end).

-spec main(#character{}) -> no_return().
%% @doc The main function of a Player. Loops forever so long as the Player does
%% not die.
%% @end
main(Player) when Player#character.health > 0 ->
    {CurrRoomPid, _CurrRoomId, _CurrRoomDesc} = Player#character.room,
    NewPlayer = receive
        %% @todo differentiate between events and actions, guard on contents
        Action when is_record(Action, action) ->
            % got a command to perform an action
            Verb = Action#action.verb,
            Subject = Action#action.subject,
            Object = Action#action.object,
            NewPlayer1 = case Verb of
                attack ->
                    % got a command to perfom attack action
                    Response = Room:targetAction(CurrRoomPid, Action),
                    %% @todo handle response
                    Player;
                enter ->
                    % got a command to perform enter action
                    Response = Room:targetAction(CurrRoomPid, Action),
                    %% @todo handle response
                    Player;
                look ->
                    % got a command to perform look action
                    Response = Room:look(CurrRoomPid),
                    %% @todo handle response, which is a list of Things
                    Player
            end;
        Event when is_record(Event, event) -> %% @todo define event record
            % notified of a game event
            Participle = Event#event.participle,
            %% @todo identify other parts of events
            %% @todo notify user of event (if someone else isn't doing that)
            NewPlayer1 = case Participle of
                attacked ->
                    % notified of attacked event
                    %% @todo differentiate between you being attacked and someone else being attacked
                    %% @todo decide what DamageTaken would be in the event record
                    HealthRemaining = Player#character.health - DamageTaken,
                    if  HealthRemaining > 0 ->
                            Player#character{health = HealthRemaining};
                        HealthRemaining =< 0 ->
                            Player#character{health = 0}
                    end;
                entered ->
                    % notified of entered event
                    %% @todo decide what NewRoom would be in the event record
                    Player#character{room = NewRoom}
            end
    after 0 ->
        Player
    end,
    main(NewPlayer);
%% @doc Player dies and exits with reason {died, Player} where Player is the
%% #player{} record.
%% @end
main(Player) when Player#character.health == 0 ->
    notifyOfDeath(  Player#character.room
                    , #player_proc  { pid = self()
                                    , id = Player#character.id
                                    , name = Player#character.name
                                    }
                    ),
    %% @todo do anything else we might want here
    exit({died, Player}).

-spec performAction(#character_proc{}, #action{}) -> any().
%% @doc Tell a Player to perform an Action.
%% @end
performAction(Player_Proc, Action) ->
    % is the room going to turn the incoming message into an #action{}, or is
    % it up to the player to do so?
    Player_Proc#character_proc.pid ! Action,
    % is the process calling this function actually concerned with a return?
    receive
        Any -> Any
    after 0 ->  %% @todo choose a timeout amount or use a timeout argument
        timeout
    end.

-spec notifyOfDeath(#room_proc, #character_proc) -> any().
%% @doc Tell the Room you are in that you have died.
%% @end
notifyOfDeath(Room_Proc, Player_Proc) ->
    room:broadcast(Room_Proc, {died, Player_Proc}),
    receive
        Any -> Any
    after 0 ->  %% @todo choose a timeout amount or use a timeout argument
        timeout
    end.
    
