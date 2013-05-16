%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Command Parser
% Takes human input (i.e. "Attack troll with sword")
% and parses it into a game-usable format.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(parser).
-export( [ parse/1 ] ).

parse(Input) ->
	case string:strip(Input) of
		"" -> {error, empty};
		_ -> doParse(Input)
	end.
	
doParse(Input) ->
	case string:str(Input, " ") of
		%just a verb
		0 -> {string:strip(Input)};
		%verb and object
		Pos -> string:tokens(Input, " ")
	end.
