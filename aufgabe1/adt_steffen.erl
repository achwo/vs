-module(dlq).
-export([create_new/0, add/3, get/2, get_last_msg_nr/1]).

create_new() -> 
    [{{"Dummy Message", 0, 0, 0, 0},0}].
    
add({Text, COut, HBQIn, _, ClientIn}, Nr, Queue) ->
    case full(Queue) of
        true    ->     lists:keysort(2, [{{Text, COut, HBQIn, util:time_in_ms(), ClientIn}, Nr}] ++ delete_first(Queue));
        false     ->    lists:keysort(2, [{{Text, COut, HBQIn, util:time_in_ms(), ClientIn}, Nr}] ++ Queue)
    end.


get(_, []) ->
    false;

get(Nr, [{_, NNr} | XS]) when Nr /= NNr ->
    get(Nr, XS);

get(Nr, [{Nachricht, _} | XS]) when XS /= [] ->
    {Nachricht, Nr, true};

get(Nr, [{Nachricht, _} | _]) ->
    {Nachricht, Nr, false}.


get_last_msg_nr([{_, NNr} | []]) ->
    NNr;

get_last_msg_nr([_|XS]) ->
    get_last_msg_nr(XS).    
        
        
full(Queue) ->
    second_entry(application:get_env(server, dlq_max_size)) =< length(Queue).
    
second_entry({_, Val}) ->
    Val.
    
delete_first([_|XS]) ->
    XS.