%%%=============================================================================
%%% @doc A Player Character (PC).
%%% PCs are the user's avatar in the game. PCs are controlled by the user.
%%% @end
%%%=============================================================================

-module(player).
-export([start/1, play/2, status/0]).
-include("character.hrl").

start(Name) -> 
	Player = spawn(player, play, [self(), Name]),
	register(player, Player),
	io:format("started the thread: ~p", [Player]).

play(Pid, Name) -> play(Pid, Name, 0).

play(Pid, Name, Curr_room) ->
	receive 
		{Pid, move} ->
			io:format("~p is moving...", [Pid])
			
        %{Start_room, player_command, {Action}} ->
		
            % would Action have already been a completely parsed command which
            % we know to be valid, or do we validate whether we can perform
            % the command the user sent? e.g. if user says drop sword,
            % but we don't have a sword, who figures this out, the Room or the
            % Player?
        %    {Results} = room:receiveAction(self(), Start_room, {Action},
        %    
        %    NewPlayerRecord = case {Results} of
        %        {died} ->
        %            {Pid, "I'm dead", Start_room};
        %        {entered, NewRoom} ->
        %            {Pid, Name, NewRoom};
        %        _ ->
        %            {Pid, Name, Start_room}
        %    end
	end,
	
    % have play take the player record as its argument
	play(Pid, Name, Curr_room).

status() -> self().