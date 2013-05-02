%%%=============================================================================
%%% @doc A Player Character (PC).
%%% PCs are the user's avatar in the game. PCs are controlled by the user.
%%% @end
%%%=============================================================================

-module(player).
-export([start/1, play/2, status/0]).
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
%% @doc The main function of a player. Loops forever.
%% @end
main(Player) ->
    {CurrRoomPid, _CurrRoomId, _CurrRoomDesc} = Player#character.room,
    NewPlayer = receive
        %% @todo differentiate between events and actions, guard on contents
        Action when is_record(Action, action) ->
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
                    Response = Room:lookAction(CurrRoomPid),
                    %% @todo
                    Player
            end;
        Event when is_record(Event, event) -> %% @todo define event record
            Participle = Event#event.participle,
            %% @todo identify other parts of events
            %% @todo notify user of event (if someone else isn't doing that)
            NewPlayer1 = case Participle of
                attacked ->
                    % notified of attacked event
                    %% @todo decide what DamageTaken would be in the event record
                    HealthRemaining = Player#character.health - DamageTaken,
                    if  HealthRemaining > 0 ->
                            Player#character{health = HealthRemaining};
                        HealthRemaining =< 0 ->
                            %% @todo die
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
    main(NewPlayer).
    