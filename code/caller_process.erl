%% module(caller_process)

%% author: xzm
%% date: 2014-10-15

%% The pair modules caller_process and called_process
%% are designed to test the processes spawned by ERTS.


-module(caller_process).
-export([start/1]).
-export([init/1]).

%% ------------------------------------------------
%% start/1 is used to test the relation between modules and processes.
%%
%% In erlang shell(Eshell), run:
%% 1>erlang:self().
%% 2>caller_process:start(3).
%% 
%% There is only one Erlang process, i.e., the Eshell process.

start(N) ->
    io:format("The pid of the caller process is ~p. N = ~p.~n", [self(),N]),
    start1(N).

start1(N) when N =:= 0 -> ok;
%%test if the above statement is :
%%    start1(N) when N == 0 -> ok;
start1(N) -> callee_process:start(N-1).

%% ------------------------------------------------
%% init/1 is used to test the relation between modules and processes.
%%
%% In erlang shell(Eshell), run:
%% 3>erlang:self().
%% 4>caller_process:init(3).
%%
%% There are THREE Erlang processes different than Eshell process, produced by spawn.

init(N) ->
    io:format("The pid of the caller process is ~p. N = ~p.~n", [self(),N]),
    init1(N).

init1(N) when N =:= 0 -> ok;
init1(N) -> spawn(callee_process,init,[N-1]).
