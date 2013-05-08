%%%=============================================================================
%%% @doc Definitions of game actions.
%%% This header defines the types of actions which can be performed as well as
%%% how they are represented as data structures.
%%%
%%% Actions are an expression of things which characters do in the game, e.g.
%%% attack.
%all this documentation referred to an old design
%%% @end
%%%=============================================================================

-include("character.hrl").
-include("room.hrl").

%the type of verbs allowed is determined by the thing that receives them. This type is simply for readability.
-type verb() :: atom().


%% @doc The formal action structure. Represented as the parts of a sentence
%% which indicate an action, e.g. "attack skeleton".
%% `verb': the verb (type of action) of the sentence.
%%
%% `subject': the subject (the performer of the action) of the setence.
%%
%% `object': the object (the target of the action) of the sentence. i.e. the
%% direct object of the sentence.
%% `type` : whether it represents an action (default), input, or an event.
%% @end
-record(action,
    { verb                  :: verb() 
    , subject               :: #character_proc{}
    , object                :: #character_proc{} | #room_proc{}
                                %% @todo allow for item_procs
    % , payload = []          :: list(payload_value())
    % , type = action         :: 'action' | 'input' | 'event'
    }).

-record(input,
    { verb                  :: verb() 
    , player               :: #character_proc{}
    , object                :: string()
    }).
%% @doc The formal event structure. Represented as the parts of a sentence
%% which indicate an event, e.g. "skeleton attacked you" / "you were attacked
%% by skeleton", or "(character) entered room".
%% `verb': the  verb  of the sentence (present tense for consistensy)
%% .
%%
%% `subject': the subject ( i.e., the thing which caused the event) of the
%% sentence, e.g. the skeleton which attacked some character, or the character
%% entering some room.
%%
%% `object': the object (i.e., the thing to which the event happened) of the
%% sentence, e.g. the character which the skeleton attacked, or the room which
%% the character entered.
%% @end
-record(event
    { verb            :: verb()
    , subject               :: #character_proc{}
    , object                :: #character_proc{} | #room_proc{}
                                %% @todo allow for item_procs
    % , payload = []          :: list(payload_value())
    }).
    

-spec make_action   ( Verb          :: verb()
                    , Subject       :: #character_proc{}
                    , Object        :: #character_proc{} | #room_proc{}
                    % , Payload       :: list(payload_value())
                    ) -> #action{}.
%% @doc Create an action structure given the parts of the sentence which form
%% it.
%% @end
make_action(Verb, Subject, Object) ->
    #action { verb = Verb
            , subject = Subject
            , object = Object
            % , payload = Payload
            }.

-spec make_event    ( Verb    :: verb()
                    , Subject       :: #character_proc{}
                    , Object        :: #character_proc{} | #room_proc
                    % , Payload         :: list(payload_value())
                    ) -> #event{}.
%% @doc Create an event structure given the parts of the sentence which form it.
%% @end
make_event(Verb, Subject, Object) ->
    #event  { verb = Verb
            , subject = Subject
            , object = Object
            % , payload = Payload
            }.
