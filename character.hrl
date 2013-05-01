%%%=============================================================================
%%% @doc Character interface for PCs and NPCs
%%% Characters share many characteristics (ha!) in common.
%%% @end
%%%=============================================================================

%% @doc A character's state.
%% `id': the unique identifier for the character.
%%
%% `name': a human-readable string which will be displayed to the user as the
%% name of the character and will be used to refer to the character in commands,
%% but need not be unique.
%%
%% `health': a number which must be at least 0. A health of 0 indicates that
%% the character is dead, otherwise the character is alive.
%%
%% `inventory': a list of items which the character is carrying.
%%
%% `room': the pid of the room in which the character resides. Character
%% processes must speak to the room to perform actions.
%% @end
-record(character,
    { id                :: reference()
    , name              :: string()
    , health            :: non_neg_integer()
    , inventory = []    :: list()
    , room              :: pid()
    }).

-spec make_character(string(), non_neg_integer(), pid()) -> #character{}.
%% @doc Creates a new character with no inventory. Returns the new character.
make_character(Name, Health, Room) ->
    #character
        { id = make_ref()
        , name = Name
        , health = Health
        , room = Room
        }.