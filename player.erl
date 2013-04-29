-module(player).
-export([start/1, status/0]).

start(Name) -> 
	player = spawn(player, play, {self(), Name}),
	register(player, player),
	io:format("started the thread: ~w~n", [player]).

play(Pid, Name) -> play(Pid, Name, 0).
play(Pid, Name, Start_room) ->
	receive 
		{ClientPid, move} ->
			io:format("~w~n is moving...", [ClientPid])
	end,
	play(Pid, Name, Start_room).

status() -> self().