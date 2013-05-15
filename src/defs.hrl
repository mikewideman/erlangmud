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


%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(thing_proc,
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
    , subject       :: #thing_proc{}
    , object        :: #thing_proc{} | #room_proc{} %% @todo | #item_proc{}
    , payload = []  :: list(payload_value())
    }).

-record(input,
    { verb          :: verb()
    , subject       :: #thing_proc{}
    , object        :: string()
    , payload = []  :: list(payload_value())
    }).

-record(event,
    { verb    :: verb()
    , subject       :: #thing_proc{}
    , object        :: #thing_proc{} | #room_proc{} %% @todo | #item_proc{}
    , payload = []  :: list(payload_value())
    }).

%%%%%%%%%%%%%%%%%%%
% Error Definition
%%%%%%%%%%%%%%%%%%%

-type thing() :: #thing_proc{}.
%% payload_value() is a special kind of tuple which is made of an atomic
%% tag which describes the data and the data itself.

%all the errors! (should be defined here please. A master list of errors keeps me ffrom having to search every file again to find what could go wrong)
-type error() :: 
	{'error', 
		%in room, when trying to target an action with something not in the room
		{'notInRoom', Thing :: thing()} |

		%used for targeting Input, when strings cannot be matched to room contents
		{'multipleMatches' | 'notInRoom', 'directObject' | 'indirectObject'} |

		%used for things when they do not support an action that occured on them
		%not actually implemented anywhere right now, but should be
		'doesNotUnderstand'
	}.
