%%%=============================================================================
%%% @doc Definitions of game events.
%%% This header defines the types of events which can occur as well as how they
%%% are represented as data structures.
%%%
%%% Events are an expression of things which happen in the game as a result of
%%% another event or an action - but events are not actions. For example,
%%% "attack" is an action, but being attacked is not - it is an event.
%%% Similarly, the occurrence of an attack being performed is an event, i.e.
%%% "attacking".
%%%
%%% These events will be formed passively by characters and items as well as
%%% actively by rooms so their meaning can be interpreted by the room in which
%%% they occur and the concerned parties can be notified of what occurred.
%%% @end
%%%=============================================================================

%% @todo define participles (present and past) e.g. attacking, attacked
%% @todo consider active vs passive voice
%% @doc see [http://en.wikipedia.org/wiki/Participle]
%% @end