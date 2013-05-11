%%%=============================================================================
%%% @doc A Hostile Non-Player Character (NPC).
%%% NPCs are the games way of interacting. NPCs are controlled by internal logic.
%%% NPCs can be spawned by rooms and will attack players who enter their room.
%%% NPCs are not your friends the do not want to talk they only want to kill you.
%%% @end
%%%=============================================================================
-module(ai).
-compile(export_all).
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
            , Room      :: pid()
			, Room_proc :: #room_proc{}
            ) -> pid().


start(Name, Health, Attack, Room) ->
	Pid = spawn(ai, loop, [ Name, Health, Attack, Room, []]),
	Name = "Evil Dude",
	#character_proc{pid=Pid, name=Name}.
	
%%% Creates an Hostile NPC and sends it back to the room	
	
-spec loop ( Name      :: string()
            , Health    :: non_neg_integer() | 'default'
            , Attack    :: pos_integer() | 'default'
            , Room      :: pid()
			,Room_proc  :: room_proc
			, Lerst		:: list()
			) -> any().
	
loop(Name, Health, Attack, Room, Room_proc,[]) ->
	receive
	Event when is_record(Event, event) ->
		case Event#event.verb of 
		enter -> loop2(Name, Health, Attack, Room, [Event#event.subject]);
		_Any -> loop(Name, Health, Attack, Room, [])
	end
end.

loop2(Name, Health, Attack, Room, Room_proc, [H|T]) ->
	Players =  T ++ [H],
	Hth = Health,
	Subject = #character_proc{pid = self()},
	Evernt= #event{verb = die, subject = Subject},
	receive
	Event when is_record(Event, event) ->
		case Event#event.verb of 
			enter -> Players =  T ++ [Event#event.subject] ++ [H];
			attack -> Hth = Health - element(2, Event#event.payload)
		end
	after 
		2500 ->
		Room ! #action { verb = attack
						% @todo Need to put ID here which  I can't get until npc record is used
						, subject = #character_proc{name=Name, pid=self()}
						, object = H
						, payload =[{damage, Attack}]
						}, 
		if 
		Hth > 0 ->loop2(Name, Hth, Attack, Room, Room_proc, Players);
		Hth =< 0 ->				
			room:broadcast(Room_proc, Evernt, #character_proc{pid = self()}) 
				end
			end.

		
	
