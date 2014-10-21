%% module(callee_module)

%% author: xzm
%% date: 2014-10-21

%% The pair modules caller_module and callee_module
%% are designed to test the macros: ?MODULE, ?LINE.

-module(callee_module).
-export([start/1]).

%% ------------------------------------------------
%% start/1 is used to test the relation between modules and modulees.

start(N) ->
    io:format("****Current process is ~p.~n",[self()]),
    io:format("****Current module is ~s.~n",[?MODULE]),
    io:format("****Current line is ~p.~n",[?LINE]),
    io:format("****Current N is ~p.~n~n", [N]),
    start1(N).

start1(N) when N =:= 0 -> ok;
start1(N) -> caller_module:start(N-1).
