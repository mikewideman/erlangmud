%%%=============================================================================
%%% @doc Definitions of game actions.
%%% This header defines the types of actions which can be performed as well as
%%% how they are represented as data structures.
%%%
%%% Actions are an expression of things which characters do in the game, e.g.
%%% attack.
%%%
%%% These actions will be formed by character modules and passed to rooms, where
%%% they will be interpreted and their results carried out.
%%% @end
%%%=============================================================================

-include("character.hrl").

-type verb() ::
      'attack'
    | 'enter'
    | 'look'
    %% @todo define more verbs
    .
%% verb() is an atom which is recognized as a valid verb in a command sentence
%% issued by the user. In general, verbs are the valid "kinds" of actions.

% -type action() :: {Verb :: verb(), Subject :: pid(), Object :: pid()}.
% %% action() is a sentence, which contains a verb, a subject, and an object.
% %% @todo consider defining a type for subjects and objects.

%% @doc The formal action structure. Represented as the parts of a sentence
%% which indicate an action, e.g. "attack skeleton".
%% `verb': the verb (type of action) of the sentence.
%%
%% `subject': the subject (the performer of the action) of the setence.
%%
%% `object': the object (the target of the action) of the sentence. i.e. the
%% direct object of the sentence.
%% @end
-record(action,
    { verb                  :: verb()
    , subject               :: #character_proc{}
    , object                :: #character_proc{}    %% @todo allow for item_procs
    }).

-spec make_action(verb(), pid(), pid()) -> #action{}.
%% @doc Create an action structure given the parts of the sentence which form
%% it.
%% @end
make_action(Verb, Subject, Object) ->
    #action
            { verb = Verb
            , subject = Subject
            , object = Object
            }.