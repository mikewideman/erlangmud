-module(player).
-export([start/1, play/2, status/0]).

start(Name) -> 
	Player = spawn(player, play, [self(), Name]),
	register(player, Player),
	io:format("started the thread: ~p", [Player]).

play(Pid, Name) -> play(Pid, Name, 0).
play(Pid, Name, Start_room) ->
	receive 
		{ClientPid, move} ->
			io:format("~p is moving...", [ClientPid])
        {Start_room, player_command, {Action} ->
            % would Action have already been a completely parsed command which
            % we know to be valid, or do we validate whether we can perform
            % the command the user sent? e.g. if user says drop sword,
            % but we don't have a sword, who figures this out, the Room or the
            % Player?
            {Results} = room:receiveAction(self(), Start_room, {Action},
            case {Results} of
                {died} ->
                    NewPlayerRecord = {Pid, "I'm dead", Start_room};
                {entered, NewRoom} ->
                    NewPlayerRecord = {Pid, Name, NewRoom};
                _ ->
                    ok
	end,
    % have play take the player record as its argument
	play(Pid, Name, Start_room).

status() -> self().