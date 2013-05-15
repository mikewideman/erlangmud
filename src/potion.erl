%%%=============================================================================
%%% @doc A health potion (pot).
%%% consuming it regenerates 25 health
%%% @end
%%%=============================================================================
-module(potion).
-extends(thing).
-compile(export_all).
-include("defs.hrl").

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Records %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%


statrt(Type)->
	case Type of
		potion -> Pid = spawn(potion, potionloop, []);
		_Any -> Pid = spawn(potion, rockloop, [])
	end,
	Name = "Health Potion",
	#character_proc{pid=Pid, name=Name}.
	

rockloop() ->
	receive
		_Any -> rockloop()
	end.
	
potionloop()->
	receive 
		Event when is_record(Event, event) ->
			case Event#event.verb of 
				drink -> thing:receiveEventNotification(Event#event.object, #event{ verb = heal
																				, subject = Event#event.object
																				, object = Event#event.subject
																				, payload = [{heal, 25}]})
			end;
		_Any-> potionloop()
	end.