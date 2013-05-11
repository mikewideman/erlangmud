%%%=============================================================================
%%% @doc Record and type definitions for game state.
%%% 
%%% @end
%%%=============================================================================

%%%%%%%%%%%%%%
%%% Types %%%%
%%%%%%%%%%%%%%

-type verb() :: atom().
%% verb() is an atom which is recognized as a valid verb in a command sentence
%% issued by the user or by character actions in general. In other words, verbs
-type payload_value() ::
      {'damage', Damage :: non_neg_integer()}   %% DamageTaken or DamageDone
    | {atom(), any()}
    .
%% payload_value() is a special kind of tuple which is made of an atomic
%% tag which describes the data and the data itself.

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(character_proc,
    { pid           :: pid()
    , id            :: reference()
    , name          :: string()
    }).

-record(room_proc,
    { pid           :: pid()
    , id            :: reference()
    , description   :: string()
    }).

-record(action,
    { verb          :: verb()
    , subject       :: #character_proc{}
    , object        :: #character_proc{} | #room_proc{} %% @todo | #item_proc{}
    , payload = []  :: list(payload_value())
    }).

-record(input,
    { verb          :: verb()
    , subject       :: string()
    , object        :: string()
    , payload = []  :: list(payload_value())
    }).

-record(event,
    { verb    :: verb()
    , subject       :: #character_proc{}
    , object        :: #character_proc{} | #room_proc{} %% @todo | #item_proc{}
    , payload = []  :: list(payload_value())
    }).
