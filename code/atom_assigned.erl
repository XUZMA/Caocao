%% author: xzm
%% date: 2014-10-23
%% the module is used to test whether an atom can appear on the left side of an assignment expression.

-module(atom_assigned).

-export([start/0]).

%% ====================TEST PART==================

-define(TESTATOM, testatom).
-define(ATOMTEST, atomtest).
-define(MONTH, month).

test() ->
    MonthList = [testatom, atomtest,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec],

    io:format("~p    ~p    ~p    ~p    ~p    ~p    ~p    ~p~n",
        [
        ?TESTATOM = lists:nth(1, MonthList),
        ?TESTATOM == lists:nth(1, MonthList),
        ?TESTATOM =:= lists:nth(1, MonthList),
        ?TESTATOM =/= lists:nth(1, MonthList),

        testatom = testatom,
        testatom == testatom,
        testatom =:= testatom,
        testatom =/= testatom
        ]),

    io:format("--------pass the 1st barrier.~n"),

    io:format("~p    ~p    ~p    ~p    ~p    ~p    ~p    ~p~n",
        [
        ?ATOMTEST = lists:nth(2, MonthList),
        ?ATOMTEST == lists:nth(2, MonthList),
        ?ATOMTEST =:= lists:nth(2, MonthList),
        ?ATOMTEST =/= lists:nth(2, MonthList),

        atomtest = atomtest,
        atomtest == atomtest,
        atomtest =:= atomtest,
        atomtest =/= atomtest
        ]),

    io:format("--------pass the 2nd barrier.~n"),

    ?MONTH = lists:nth(3, MonthList),
    io:format("pass the 3rd barrier.~n"),

    ok.

start() ->
    test().

%% ========================END=======================
