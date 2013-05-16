%%%=============================================================================
%%% @doc A health potion (pot).
%%% consuming it regenerates 25 health
%%% @end
%%%=============================================================================
-module(object).
-extends(thing).
-compile(start/1).
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

-spec start(Type :: atom(), Name :: string() Value :: any()) -> #thing_proc{}.
%% @doc Start an object's process with a given name. The process will run until
%% the object is interacted with. Returns the thing_proc of the object.
%% @end
start(Type, Name, Value)->
    Proc = case Type of
        potion ->
            #thing_proc{pid = spawn(fun() -> potionloop(Value) end), name = Name};
        weapon ->
            #thing_proc{pid = spawn(fun() -> weaponloop(Value) end), name = Name};
        _Any ->
            #thing_proc{pid = spawn(fun() -> rockloop() end), name = Name}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%    

-spec rockloop() -> no_return().
%% @doc An inanimate rock.
%% @end
rockloop() ->
    receive
        _Any -> rockloop()
    end.

-spec potionloop(HealAmount :: pos_integer()) -> no_return().
%% @doc A health potion.    
%% @end
%% @todo: the potion never gets used up.
potionloop(HealAmount)->
    receive 
        Event when  is_record(Event, event)
                    andalso Event#event.object#thing_proc.pid == self() ->
            case Event#event.verb of 
                drink ->
                    thing:receiveEventNotification
                        ( Event#event.object
                        , #event{ verb = heal
                                , subject = Event#event.object
                                , object = Event#event.subject
                                , payload = [{heal, HealAmount}]});
                _Any ->
                    %% @todo tell room an invalid verb was sent
                    potionloop()
            end;
        _Any-> potionloop()
    end.

-spec weaponloop(AttackBonus :: pos_integer()) -> no_return().
%% @doc A weapon.
%% @end
weaponloop(AttackBonus) ->     
    receive 
        Event when  is_record(Event, event)
                    andalso Event#event.object#thing_proc.pid == self() ->
            case Event#event.verb of 
                pick_up ->
                    thing:receiveEventNotification
                        ( Event#event.object
                        , #event{ verb = inc_attack
                                , subject = Event#event.object
                                , object = Event#event.subject
                                , payload = [{inc_attack, AttackBonus}]});
                _Any ->
                    %% @todo tell room an invalid verb was sent
                    potionloop()
            end;
        _Any-> potionloop()
    end.