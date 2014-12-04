%% author: xzm
%% date: Dec.4,2014

-module(toy_dbg).

-export([start/1, store/2, retrieve/1,stop/0]).

%% Args is the the argument Options for ets:new(Name, Options), such as [ordered_set].
start(Args) ->
    Toy_table = ets:new(toy_table,Args).

%% start(Args) ->
%%     toy_table = ets:new(toy_table,[named_table |Args]).

store(Key, Value) ->
    ets:insert(toy_table,{Key,Value}).

retrieve(Key) ->
    [{Key, Value}] = ets:lookup(toy_table,Key),
    Value.
