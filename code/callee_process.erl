%% module(callee_process)

%% author: xzm
%% date: 2014-10-15

%% The pair modules caller_process and callee_process
%% are designed to test the processes spawned by ERTS.

-module(callee_process).
-export([start/1]).
-export([init/1]).

%% ------------------------------------------------
%% start/1 is used to test the relation between modules and processes.

start(N) ->
    io:format("----The pid of the callee process is ~p. N = ~p.~n", [self(), N]),
    start1(N).

start1(N) when N =:= 0 -> ok;
%%test if the above statement is :
%%    start1(N) when N == 0 -> ok;
start1(N) -> caller_process:start(N-1).

%% ------------------------------------------------
%% init/1 is used to test the relation between modules and processes.

init(N) ->
    io:format("----The pid of the callee process is ~p. N = ~p.~n", [self(),N]),
    init1(N).

init1(N) when N =:= 0 -> ok;
init1(N) -> spawn(caller_process,init,[N-1]).
