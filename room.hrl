%%%=============================================================================
%%% @doc Interface for rooms.
%%% Entities ("Things") in the system as well as the Dungeon at large need to
%%% communicate with rooms. This header defines the interface for communicating
%%% with rooms.
%%% @end
%%%=============================================================================


%% @doc A room's state.
%% `id': the unique identifier for the room.
%%
%% `description': a human-readable description of the room.
%%
%% `things': a list of the "Things" (i.e. PCs, NPCs, and items) in the room.
%%
%% `north_door', `east_door', `south_door', `west_door': doors to neighboring
%% rooms in the cardinal direction of their respective field names.
-record(room,
    { id :: reference()
    , description :: string()
    , things :: list()
    , north_door :: pid() | none()
    , east_door :: pid() | none()
    , south_door :: pid() | none()
    , west_door :: pid() | none()
    }).