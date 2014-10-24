%% author: xzm
%% date: 2014-10-24
%% the module demonstrates the following usages:
%%        1. examing file type with lists:suffix;
%%        2. classifying files with filename prefix and lists:prefix.
%%
%% Notice:
%%    when L is a list consisting of serveral strings, the function definitions, f([L]) and f(L), are totally different.
%%

-module(list_prefix_suffix).

-export([start/0]).

%% ======================BODY======================

outputStrList([]) ->
    ok;
outputStrList(L) ->
    [StrListH|StrListT] = L,     %% NOTICE: if we define the function as outputStrList(StrListH|StrListT), there is an logic error, because the last element("list_prefix_suffix.erl") will be treated as a dividual list.
    io:format("~s    ",[StrListH]),%% NOTICE: here must place StrListH in a list, otherwise an exception error will be thrown.
    outputStrList(StrListT).


test() ->
    Suffix_file = ".erl",
    Prefix_file = "list_",

    List_filename = 
        ["atom_assigned.erl", "caller_module.erl", "ifcase.erl", "record_dot.erl", "callee_module.erl", "caller_process.erl", "list_flatten.erl", "callee_process.erl", "elog3.config", "list_prefix_suffix.erl"],
    outputStrList(List_filename),
    io:format("~n------------------------------------------------~n"),

    List1 = lists:filter(
                        fun(X) -> lists:suffix(Suffix_file,X) end,
                        List_filename
                        ),
    outputStrList(List1),
    io:format("~n------------------------------------------------~n"),

    List2 = lists:filter(
                        fun(X) -> lists:prefix(Prefix_file,X) end,
                        List_filename
                        ),
    outputStrList(List2),
    io:format("~n------------------------------------------------~n").


start() ->
    test().

%% ========================END=======================
