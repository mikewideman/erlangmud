%%%=============================================================================
%%% @doc Record and type definitions for game state.
%%% 
%%% @end
%%%=============================================================================

%%%%%%%%%%%%%%
%%% Types %%%%
%%%%%%%%%%%%%%

-type verb() ::
      'attack'
    | 'enter'
    | 'pick_up'
    | 'inc_attack'
    | 'drink'
    | 'heal'
    | 'died'
    | 'left'
    | atom().   %% lets you use any atom you want, the above are just for
                %% reference. 
%% verb() is an atom which is recognized as a valid verb in a command sentence
%% issued by the user or by character actions in general. In other words, verbs
%% are the types of things that can happen.

-type payload_value() ::
      {'damage', Damage :: pos_integer()}       %% DamageTaken or DamageDone
    | {'heal', Heal :: pos_integer()}           %% HealAmount or HealthRestored
    | {'inc_attack', Attackinc :: pos_integer()}
    | {'room_content', ContentString :: [string()]}
    | {atom(), any()}
    .
%% payload_value() is a type which describes extra data in actions and events.
%% It comes in the form of a key, value pair.

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(thing_proc,
    { pid               :: pid()
    , id = make_ref()   :: reference()
    , name              :: string()
    }).

-record(room_proc,
    { pid               :: pid()
    , id = make_ref()   :: reference()
    , description       :: string()
    }).

-record(action,
    { verb          :: verb()
    , subject       :: #thing_proc{}
    , object        :: #thing_proc{} | #room_proc{}
    , payload = []  :: list(payload_value())
    }).

-record(input,
    { verb          :: verb()
    , subject       :: #thing_proc{}
    , object        :: string()
    , payload = []  :: list(payload_value())
    }).

-record(event,
    { verb          :: verb()
    , subject       :: #thing_proc{}
    , object        :: #thing_proc{} | #room_proc{}
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
