%%%=============================================================================
%%% @doc A Hostile Non-Player Character (NPC).
%%% NPCs are the games way of interacting. NPCs are controlled by internal logic.
%%% NPCs can be spawned by rooms and will attack players who enter their room.
%%% NPCs are not your friends the do not want to talk they only want to kill you.
%%% @end
%%%=============================================================================
-module(ai).
-extends(thing).
-export([start/4]).
-include("defs.hrl").

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

-record( npc,
    { id = make_ref()   :: reference()
    , name              :: string()
    , health = 1        :: non_neg_integer()
    , attack = 1        :: pos_integer()
	, pcs = []			:: list()
    , room              :: #room_proc{}
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start ( Name      :: string()
            , Health    :: non_neg_integer() | 'default'
            , Attack    :: pos_integer() | 'default'
			, Room_proc :: #room_proc{}
            ) -> pid().


start(Name, Health, Attack, Room_proc) ->
 NPC = #npc 			{ name = Name
                        , health = Health
                        , attack = Attack
						, pcs =[]
                        , room = Room_proc
                        },
	Pid = spawn(ai, loop, [ NPC]),
	Name = "Evil Dude",
	#thing_proc{pid=Pid, name=Name}.
	
%%% Creates an Hostile NPC and sends it back to the room	
	
%%%-spec loop ( Name      :: string()
 %%%           , Health    :: non_neg_integer() | 'default'
    %%%        , Attack    :: pos_integer() | 'default'
    %%%        , Room      :: pid()
	%%%		,Room_proc  :: room_proc
	%%%		, Lerst		:: list()
	%%%		) -> any().
	
loop(NPC) ->
	receive
	Event when is_record(Event, event) ->
		case Event#event.verb of 
		enter -> loop2(NPC#npc{pcs = [Event#event.subject]});
		_Any -> loop(NPC)
	end
end.

loop2(NPC) ->
	[H | T] =  NPC#npc.pcs,
	Players = T ++ H,
	% Hth = NPC#npc.health,
	Subject = #thing_proc{pid = self()},
	Room = NPC#npc.room,
	Evernt= #event{verb = die, subject = Subject},  %% @todo assign object
	Hth = receive
	Event when is_record(Event, event) ->
		case Event#event.verb of 
			enter -> Players =  T ++ [Event#event.subject] ++ [H], NPC#npc.health;
			attack -> NPC#npc.health - element(2, Event#event.payload)
		end
	after 
		2500 ->
		Room#room_proc.pid ! #action { verb = attack
						% @todo Need to put ID here which  I can't get until npc record is used
						, subject = #thing_proc{name=NPC#npc.name, pid=self()}
						, object = H
						, payload =[{damage, NPC#npc.attack, NPC}]
						},
            NPC#npc.health
    end,
    if 
		Hth > 0 ->loop2(NPC#npc{ health = Hth, pcs = Players});
		Hth =< 0 ->				
			room:broadcast(Room, Evernt, #thing_proc{pid = self()}) 
	end.

		
	
