%%%=============================================================================
%%% @doc A dungeon room.
%%% Rooms are the smallest unit of geographical space in the dungeon. Rooms
%%% are connected to each other by doors (links to other rooms). Rooms may
%%% contain characters and/or items, or nothing at all. Rooms act as the liaison
%%% for communication between characters and other entities of the game by
%%% directing messages (e.g. game actions) to the concerned parties, as well as
%%% the publisher of game event messages which are sent to users.
%%% @end
%%%=============================================================================

-module(room).
-include("room.hrl").