%% author: xzm
%% date: 2014-12-12
%% 

%% multiple list comprehension [{X,Y}||X<-L1,Y<-L2]

-module(multiple_comprehension).
-export([start/0]).

start()->
    L1 = [1,2,3,4,5],
    L2 = [-1,-2,-3,-4,-5],

    L3 = [{X,Y}|| X<-L1,Y<-L2],

    io:format("L1 = ~p~n",[L1]),
    io:format("L2 = ~p~n",[L2]),
    io:format("L3 = ~p~n",[L3]),

    ok.
