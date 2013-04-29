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
	end,
	play(Pid, Name, Start_room).

status() -> self().