%% author: xzm
%% date: 2014-12-08

-module(function_scope).

-export([a/0,b/1,declare_and_run/0,dr_with_shadow/0,dr_without_shadow/0]).

a() ->
    Secret = "Money",
    fun() -> Secret end.
 
b(F) ->
    "a/0's password is "++F().

declare_and_run()->
    fun()->io:format("declare an anonymous function and run it immediately.~n") end().

dr_with_shadow() ->
    A = 1,
    (fun(A) -> A = 2 end)(2).

dr_without_shadow() ->
    A = 1,
    (fun() -> A = 2 end)().

%% compile this module:
%%    $ erlc function_scope.erl
%% Listed below are the compiling warnings:
%%     function_scope.erl:19: Warning: variable 'A' is unused
%%     function_scope.erl:20: Warning: variable 'A' shadowed in 'fun'
%%     function_scope.erl:24: Warning: no clause will ever match
%%     function_scope.erl:24: Warning: the guard for this clause evaluates to 'false'
%%
%% after compilation, run in Eshell:
%%    1> function_scope:b(function_scope:a()).
%%    "a/0's password is Money"
%%    2> function_scope:declare_and_run().
%%    declare an anonymous function and run it immediately.
%%    ok
%%    3> function_scope:dr_with_shadow().
%%    2
%%    4> function_scope:dr_without_shadow().
%%    ** exception error: no match of right hand side value 2
%%         in function  function_scope:'-dr_without_shadow/0-fun-0-'/0 (function_scope.erl, line 24)
