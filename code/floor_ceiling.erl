%% author: xzm
%% date: 2011-11-13

%% implement floor/1 and ceiling/1 with trunc/1

-module(floor_ceiling).
-export([start/0]).

%% ====================MAIN PART====================

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;    %% NOTICE: this is a special usage of guard clause.
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    if
        X - T > 0 -> T + 1;
        true-> T
    end.


%% ====================TEST PART==================

test() ->
    List_real = [-3.14, -2, 0, 9.18],
    io:format("list of reals:~n ~p~n~n", [List_real]),

    List_floor = lists:map(fun(X)->floor(X) end, List_real),
    io:format("list of floors:~n ~p~n~n", [List_floor]),

    List_ceil = lists:map(fun(X)->ceiling(X) end,List_real),
    io:format("list of ceilings:~n ~p~n~n", [List_ceil]),

    ok.

start() ->
    test().

%% ========================END=======================
