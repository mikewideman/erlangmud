%%%=============================================================================
%%% @doc Definitions of game events.
%%% This header defines the types of events which can occur as well as how they
%%% are represented as data structures.
%%% Events are actions after they occur. 
%%% They are in almost the same form as an action (subject, verb, direct object, sometimes an indirect object), but things are not expected to take action on them. They are passive. 
%%% Events MAY also carry a human readable string for user output.
%%% @end
%%%=============================================================================

%% @todo define participles (present and past) e.g. attacking, attacked
%% @todo consider active vs passive voice
%% @doc see [http://en.wikipedia.org/wiki/Participle]
%% @end
-import("verb.hrl").
-type participle() ::
      'attacked'
    | 'entered' 
    | verb()
    %% @todo define more participles
    .
-type event :: {Verb :: participle(), Subject :: pid(), DirectObject :: pid()} | {Verb :: participle(), Subject :: pid(), DirectObject :: pid(), Text ::: string()}.
%% participle() is a type which correlates to a verb() in action.hrl, but 
%% is the past version of it, e.g. attack -> attacked.
