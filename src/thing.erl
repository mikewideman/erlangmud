%%%=============================================================================
%%% @doc Any Thing in the game.
%%% Things include inanimate Objects, Players, and NPCs, and they all receive the same messages. 
%%% This module contains the wrapping functions for all these modules. They can extent Thing if they want them, too.
%%% @end
%%%=============================================================================

-module(thing).
-export([receiveEventNotification/2]).
-include("defs.hrl").

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-spec receiveEventNotification  ( Character_Proc   :: #character_proc{}
                                , Event         :: #event{}
                                ) -> any().
%% @doc Notify the Thing of a game event. The Thing may send an action in
%% response to this event, or it may ignore it.
%% @see room:broadcast/2
%% @end
% %% @todo consider naming
receiveEventNotification(Character_Proc, Event) ->
    Character_Proc#character_proc.pid ! Event.