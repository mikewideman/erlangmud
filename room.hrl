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
%% @end
-record(room,
    { id                    :: reference()
    , description           :: string()
    , things = []           :: list()           % @todo item type
    , north_door = none     :: pid() | none     % @todo room types?
    , east_door = none      :: pid() | none
    , south_door = none     :: pid() | none
    , west_door = none      :: pid() | none
    }).

-spec make_room(string()) -> #room{}.
%% @doc Create a new room with no linked rooms and nothing in it. Returns the
%% created room.
%% @end
make_room(Description) ->
    #room
        { id = make_ref()
        , description = Description
        }.

% -spec link_rooms_north_south(#room{}, #room{}) -> {#room{}, #room{}}.
% %% @doc Link two rooms with a north-south door. The first room will be the
% %% northern room. The second will be the southern room. Returns the modified
% %% rooms.
% %% @end
% link_rooms_north_south(Room1, Room2) ->
    % { Room1#room
        % { south_door = %% @todo get pid for link
        % }
    % , Room2#room
        % { north_door = %% @todo get pid for link
        % }
    % }.

% -spec link_rooms_east_west(#room{}, #room{}) -> {#room{}, #room{}}.
% %% @doc Link two rooms with a east-west door. The first room will be the
% %% eastern room. The second will be the western room. Returns the modified
% %% rooms.
% %% @end
% link_rooms_east_west(Room1, Room2) ->
    % { Room1#room
        % { west_door = %% @todo get pid for link
        % }
    % , Room2#room
        % { east_door = %% @todo get pid for link
        % }
    % }.