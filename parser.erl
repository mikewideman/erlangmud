%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Command Parser
% Takes human input (i.e. "Attack troll with sword")
% and parses it into a game-usable format.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(parser).
-export( [ parse/1 ] ).

parse(String) ->
	doParse( string:tokens( string:to_lower(String), " " ) ).

doParse( [] ) ->
	{error, "EmptyCommand"};

doParse( [Verb] ) ->
	{Verb};

doParse( [Verb, Object] ) ->
	{Verb, Object};

doParse( [Verb, DObj, "with", IObj] ) ->
	{Verb, DObj, IObj};

doParse( _ ) ->
	{error, "InvalidCommandForm"}.
