%% author: xzm
%% date: 2014-12-12
%% 

%% multiple list comprehension [{X,Y}||X<-L1,Y<-L2]
%% https://www.haskell.org/hugs/pages/users_guide/hugs-ghc.html
%% 6.1.2. Parallel list comprehensions (a.k.a. zip-comprehensions)
%% Parallel list comprehensions are a natural extension to list comprehensions. List comprehensions can be thought of as a nice syntax for writing maps and filters. Parallel comprehensions extend this to include the zipWith family.
%% A parallel list comprehension has multiple independent branches of qualifier lists, each separated by a "|" symbol. For example, the following zips together two lists:
%% [ (x, y) | x <- xs | y <- ys ]
%% The behavior of parallel list comprehensions follows that of zip, in that the resulting list will have the same length as the shortest branch.
%% We can define parallel list comprehensions by translation to regular comprehensions. Given a parallel comprehension of the form:
%% [ e | p1 <- e11, p2 <- e12, ...
%%     | q1 <- e21, q2 <- e22, ...
%%     ...
%% ]
%% This will be translated to:
%% [ e | ((p1,p2), (q1,q2), ...) <- zipN [(p1,p2) | p1 <- e11, p2 <- e12, ...]
%%                                       [(q1,q2) | q1 <- e21, q2 <- e22, ...]
%%                                       ...
%% ]
%% where "zipN" is the appropriate zip for the given number of branches. These functions must be in scope; the Prelude defines zip and zip3, but if you want to handle 4 or more lists in parallel, you will need to import List or Data.List.

-module(multiple_comprehension).
-export([start/0]).

start()->
    L1 = [a1,a2,a3,a4,a5],
    L2 = [b1,b2,b3,b4,b5],
    L3 = [{X,Y}|| X<-L1,Y<-L2],

    %% L4 = [{X,Y}|| X<-L1 || Y<-L2],

%%     L4 = [fun(Y) ->{Y} end || Y<-L2],
%% erlc multiple_comprehension.erl
%% multiple_comprehension.erl:32: Warning: variable 'Y' is unused
%% multiple_comprehension.erl:32: Warning: variable 'Y' shadowed in 'fun'
%% L4 = [#Fun<multiple_comprehension.0.71978625>,
%%       #Fun<multiple_comprehension.0.71978625>,
%%       #Fun<multiple_comprehension.0.71978625>,
%%       #Fun<multiple_comprehension.0.71978625>,
%%       #Fun<multiple_comprehension.0.71978625>]

%%     L4 = [[{X,Y}|| X<-L1] || Y<-L2],
%% L4 = [[{a1,b1},{a2,b1},{a3,b1},{a4,b1},{a5,b1}],
%%       [{a1,b2},{a2,b2},{a3,b2},{a4,b2},{a5,b2}],
%%       [{a1,b3},{a2,b3},{a3,b3},{a4,b3},{a5,b3}],
%%       [{a1,b4},{a2,b4},{a3,b4},{a4,b4},{a5,b4}],
%%       [{a1,b5},{a2,b5},{a3,b5},{a4,b5},{a5,b5}]]

    L4 = [[{X,Y}|| X<-L1] || Y<-L2],

    io:format("L1 = ~p~n",[L1]),
    io:format("L2 = ~p~n",[L2]),
    io:format("------------------------------------------------~n"),
    io:format("L3 = ~p~n",[L3]),
    io:format("------------------------------------------------~n"),
    io:format("L4 = ~p~n",[L4]),
    ok.
