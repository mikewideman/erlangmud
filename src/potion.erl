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


start(Type)->
	case Type of
		potion -> Pid = spawn(potion, potionloop, [])
			,Proc=#thing_proc{pid=Pid, name="Health Potion"};
		_Any -> Pid = spawn(potion, rockloop, [])
			,Proc=#thing_proc{pid=Pid, name="Rock"}
	end,
	Proc.
	

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
