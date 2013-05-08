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
    | 'look'
    %% @todo define more verbs
    .
%% verb() is an atom which is recognized as a valid verb in a command sentence
%% issued by the user or by character actions in general. In other words, verbs
%% are the valid "kinds" of actions, although it is possible to union other atoms
%% with verb() in function specs to make other "kinds" of actions admissible in
%% certain contexts.

-type participle() ::
      'attacked'
    | 'entered'
    | 'died'
    | verb()
    % might die be a verb, so that it is an action to die?
    .
%% participle() is an atom which is recognized as a valid past participle
%% ('the past form of the verb') of a sentence describing a game event. In other
%% words, participles are the valid "kinds" of events, although, like with
%% verbs, it is possible to union other atoms with participle() in function
%% specs to make other "kinds" of events admissible in certain contexts.
%% A participle() need not correlate to one or any verb().

-type payload_value() ::
      {'damage', Damage :: non_neg_integer()}   %% DamageTaken or DamageDone
    | {atom(), any()}
%% payload_value() is a special kind of tuple which is made of an atomic
%% tag which describes the data and the data itself.

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record(character_proc,
    { pid          :: pid()
    , id           :: reference()
    , name         :: string()
    }).

-record(room_proc,
    { pid          :: pid()
    , id           :: reference()
    , description  :: string()
    }).

-record(action,
    { verb          :: verb() | participle()
    , subject       :: #character_proc{}
    , object        :: #character_proc{} | #room_proc{} %% @todo | #item_proc{}
    , payload = []  :: list(payload_value())
    }).

-record(event
    { participle    :: participle()
    , subject       :: #character_proc{}
    , object        :: #character_proc{} | #room_proc{} %% @todo | #item_proc{}
    , payload = []  :: list(payload_value())
    }).
