%%%=============================================================================
%%% @doc Character interface for PCs and NPCs
%%% Characters share many characteristics (ha!) in common.
%%% @end
%%%=============================================================================

-include("room.hrl").

%% @doc A character's state. To be maintained by a character process.
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
    , health = 1        :: non_neg_integer()
    , attack = 1        :: pos_integer()
    , inventory = []    :: list()               % @todo: define item type
    , room              :: #room_proc{}
    }).
    
% -type character_t() :: {Proc :: pid(), Id :: reference(), Name :: string()}.
% %% character() is a type which includes a character process and all of its
% %% record data which will not change, which is useful for avoiding unnecessary
% %% queries to other processes

%% @doc A character process. Includes the process id and the static state
%% of that character's #character record.
%% `pid': the character process' pid.
%%
%% `id': the unique identifier for the character, see #character.id.
%%
%% `name': the name of the character, see #character.name.
%% @end
-record(character_proc,
    { pid               :: pid()
    , id                :: reference()
    , name              :: string()
    }).

-spec make_character( string()
                    , non_neg_integer() | 'default'
                    , pos_integer() | 'default'
                    , room_t()
                    ) -> 
    #character{}.
%% @doc Create a new character with no inventory. If the default values for
%% health or attack are desired, use the atom 'default' for their arguments.
%% Returns the new character.
%% @end
make_character(Name, default, default, Room) ->
    #character  { id = make_ref()
                , name = Name
                , room = Room
                };
make_character(Name, Health, default, Room) ->
    #character  { id = make_ref()
                , name = Name
                , health = Health
                , room = Room
                };
make_character(Name, default, Attack, Room) ->
    #character  { id = make_ref()
                , name = Name
                , attack = Attack
                , room = Room
                };
make_character(Name, Health, Attack, Room) ->
    #character  { id = make_ref()
                , name = Name
                , health = Health
                , attack = Attack
                , room = Room
                }.