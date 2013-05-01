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

-type verb() ::
      'attack'
    | 'enter'
    %% @todo define more verbs
    .
%% verb() is an atom which is recognized as a valid verb in a command sentence
%% issued by the user. In general, verbs are the valid "kinds" of actions.

-type action() :: {Verb :: verb(), Subject :: pid(), Object :: pid()}.
%% action() is a sentence, which contains a verb, a subject, and an object.
%% @todo consider defining a type for subjects and objects.