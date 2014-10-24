%% author: xzm
%% date: 2014-10-24
%% the module is to show the usage of lists:flatten.
%% Notice:
%%    fun(L) and fun([L]) are totally different.

-module(list_flatten).

-export([start/0]).

%% ==================MAIN BODY====================

flatten_strlist_level_2to1([]) ->
    [];
flatten_strlist_level_2to1(L) ->
    [H|T] = L,
    H ++ flatten_strlist_level_2to1(T).

%% ====================TEST PART==================

test() ->
    List1 = 
    [
        [1,
            [2,
                [3],
                []
            ]
        ],
        [
            [
                [4]
            ]
        ],
        [5,6]
    ],
    io:format("~p~n", [List1]),
    io:format("~p~n", [lists:flatten(List1)]),

    %% --------------------------------

    List2 = [["str1","str2"],["str3","str4"],["str5","str6"]],
    io:format("~p~n", [List2]),
    io:format("~p~n", [lists:flatten(List2)]),
    
    List3 = flatten_strlist_level_2to1(List2),
    io:format("~p~n", [List3]),
    io:format("~p~n", [lists:flatten(List3)]).

start() ->
    test().

%% ========================END=======================
