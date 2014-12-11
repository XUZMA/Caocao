%% author: xzm
%% date: 2014-12-11
%% 

%% 

%% Erlang Operator Precedence
%% operators in order of descending priority
%% /, *, div, rem, band, and
%% +, -, bor, bxor, bsl, bsr, or, xor
%% ++, --
%% ==, /=, =<, <, >=, >, =:=, =/=
%% andalso
%% orelse
%% =!

-module('operator_precedence').
-export([start/0]).

-define(is_nn_number(_R),
    (is_number(_R) andalso _R >= 0)).

%% notice the different validation functions.
%% the 2nd is more concise and extensible than the 1st one.

%% validate a probability distribution.
valid_distribution1(Prob) ->
    if
        is_list(Prob) and (length(Prob) == 5) ->
            [P1,P2,P3,P4,P5] = Prob,
            if
                ?is_nn_number(P1) andalso
                ?is_nn_number(P2) andalso
                ?is_nn_number(P3) andalso
                ?is_nn_number(P4) andalso
                ?is_nn_number(P5) 
                        ->
                    Total = P1 + P2 + P3 + P4 + P5,
                    (Total == 100) andalso
                    (0 =< P1) and (P1 =< 100) andalso
                    (0 =< P2) and (P2 =< 100) andalso
                    (0 =< P3) and (P3 =< 100) andalso
                    (0 =< P4) and (P4 =< 100) andalso
                    (0 =< P5) and (P5 =< 100);
		true ->false
            end;
        true -> false
    end.

valid_distribution2(Prob) ->
    is_list(Prob) 
    andalso (length(Prob) == 5) 
    andalso lists:all(fun is_number/1,Prob)
    andalso lists:all(fun(X)-> (0 =< X)andalso(X =< 100)end,Prob)
    andalso (lists:sum(Prob)==100).

start()->

    Probability_list = [20,20,20,20,20],
    VD1 = valid_distribution1(Probability_list),
    VD2 = valid_distribution2(Probability_list),
    case VD1 of
        true ->
            io:format("valid probability list = ~p~n",[Probability_list]);
	_ ->
            io:format("invalid probability list = ~p~n",[Probability_list])
    end,
    case VD2 of
        true ->
            io:format("valid probability list = ~p~n",[Probability_list]);
	_ ->
            io:format("invalid probability list = ~p~n",[Probability_list])
    end.
