-module(ifcase).
-author('xzm@debian').

-export([days_of_month/2, start/0]).

%% ====================MAIN PART====================


days_of_month(Year, Month) -> 
%% ------------------------------------------------
%% All years divisible by 400 are leap
%% Years divisible by 100 are not leap (except the 400 rule above)
%% Years divisible by 4 are leap (except the 100 rule above)
    Leap =
        if
            trunc(Year / 400) * 400 == Year ->
                leap;
            trunc(Year / 100) * 100 == Year ->
                not_leap;
            trunc(Year / 4) * 4 == Year ->
                leap;
            true ->
                not_leap
        end,
%% ------------------------------------------------
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.

%% ====================TEST PART==================

monthStr(Month) ->
    case Month of
        jan -> "Jan.";
        feb -> "Feb.";
        mar -> "Mar.";
        apr -> "Apr.";
        may -> "May.";
        jun -> "Jun.";
        jul -> "Jul.";
        aug -> "Aug.";
        sep -> "Sep.";
        oct -> "Oct.";
        nov -> "Nov.";
        dec -> "Dec."
    end.

test_month_print(_,[]) ->
    ok;
test_month_print(Year,[MH|MT]) ->
    io:format("There are ~p days in ~s ~p.\n",[days_of_month(Year, MH), monthStr(MH),Year]),
    test_month_print(Year,MT).
%%    If the last statement is written as follows:
%%        test_month_print(Year,[MT]).
%%    an exception error will throw:
%%    ** exception error: no case clause matching [feb,mar,apr,may,jun,jul,aug,sep,
%%                                                 oct,nov,dec]
%%    Bear in mind:
%%        [MH|MT] produce a term MH and a list MT.

test_print([],_) ->
    ok;
test_print(_,[]) ->
    ok;
test_print([YH|YT],MonthList) ->
    test_month_print(YH,MonthList),
    test_print(YT,MonthList).

test() ->
    MonthList = [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec],
    YearList = [2000,2004,2008,2012,2013,2014],
    test_print(YearList, MonthList),
    ok.

start() ->
    test().

%% ========================END=======================
