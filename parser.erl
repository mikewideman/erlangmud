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
		Other -> {ok, doParse(Input)}
	end.
doParse(Input) ->
	case string:str(Input, " ") of
		%just a verb
		0 -> {list_to_atom(string:strip(Input))};
		%verb and object
		Pos -> {string:strip(string:sub_string(Input, 1, Pos)), string:strip(string:sub_string(Input, Pos))}
	end.
