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

%% validate a probability distribution.
valid_distribution(Prob) ->
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


start()->

    Probability_list = [20,20,20,20,20],
    VD = valid_distribution(Probability_list),
    case VD of
        true ->
            io:format("valid probability list = ~p~n",[Probability_list]);
	_ ->
            io:format("invalid probability list = ~p~n",[Probability_list])
    end.
