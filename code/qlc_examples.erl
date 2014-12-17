%% author: xzm
%% date: 2014-12-17
%% 

%% examples of qlc usages.

-module(qlc_examples).
-export([start/0]).

-include_lib("stdlib/include/qlc.hrl").

start()->
    Q = qlc:q(
        [{A,X,Z,W} ||
        A <- [a,b,c],
        {X,Z} <- [{a,1},{b,4},{c,6}],
        {W,Y} <- [{2,a},{3,b},{4,c}],
        X =:= Y],
        {cache, list}),
    io:format("~s~n", [qlc:info(Q)]),

    io:format("------------------------------------------------~n"),

    T = gb_trees:empty(),
    QH = qlc:q(
        [X || 
            {{X,Y},_} <- gb_table:table(T),
            ((X == 1) or (X == 2)) andalso ((Y == a) or (Y == b) or (Y == c))]),
    io:format("~s~n", [qlc:info(QH)]),

    ok.
