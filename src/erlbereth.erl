-module(erlbereth).
-behaviour(application).
-export([start/2, stop/1]).

%% @doc Start the Erlbereth server.
%% @end
start(_Type, _Args) ->
    {ok, server:start()}.
    
%% @doc Stop the Erlbereth server.
%% @end
stop(_State) ->
    server:shutdown().