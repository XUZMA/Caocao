%% author: xzm
%% date: 2014-10-28/29

%% http://erlang.org/doc/man/lists.html#keymember-3
%% keymember(Key, N, TupleList) -> boolean()
%% Types:
%% Key = term()
%% N = integer() >= 1
%% 1..tuple_size(Tuple)
%% TupleList = [Tuple]
%% Tuple = tuple()
%% Returns true if there is a tuple in TupleList whose Nth element compares equal to Key, otherwise false.

%%
%% TRICK:
%%
%% In case statement, if no pattern matches, then an exception is raised.
%% In if statement, at least one of the guards in the if expression must evaluate to true; otherwise, an exception will be raised.
%%
%% It is NOT a good programming practice in Erlang to have only one branch clause in case/if statement.
%% Unexpected runtime errors will throw out of  one-branch-clause case/if statement.
%%
%% It's a good practice to use _/true as the sink branch:
%%    _ should be the sink condition of the last clause for case-statement;
%%    while true should be the sink condition of the last clause for if-statement.
%%
%% NOTICE: One-branch-clause if statement is a C, rather than Erlang, programming usage.
%%

-module(keymember).
-export([start/0]).

start()->
    PEr2nd = {
        "Progranmming Erlang, Software For a Concurrent World",
        "Joe Armstrong",
        "2nd",
        "2013-10-05",
        "Pragmatic Bookshelf",
        "9781937785536",
        "548 pages"
        },
    PEl1st = {
        "Programming Elixir",    %% fullname
        "Dave Thomas",    %% author
        "1st",    %% vsn
        "2014-10-15",    %% date
        "Pragmatic Bookshelf",    %% pub
        "9781937785581",    %% isbn
        "240 pages"    %% pages
        },

    BookList = [PEr2nd, PEl1st],

    Bool_1st = lists:keymember("1st",3,BookList),
    Bool_2nd = lists:keymember("2nd",3,BookList),

    io:format("------------------------the 1st------------------------~n"),
    case Bool_1st of
        true ->
	    io:format("there are books of 1st version.~n");
	_ ->
        case Bool_2nd of
            true ->
		io:format("there are books of 2nd version.~n");
            _ ->
		io:format("no books of 1st or 2nd version.~n")
        end
    end,

    io:format("------------------------the 2nd------------------------~n"),
    if
        Bool_1st ->
	    io:format("there are books of 1st version.~n");
	true ->
        if
            Bool_2nd ->
		io:format("there are books of 2nd version.~n");
            true ->
		io:format("no books of 1st or 2nd version.~n")
        end
    end,

    io:format("------------------------the 3rd------------------------~n"),
    if
        Bool_1st ->
	    io:format("there are books of 1st version.~n");
        Bool_2nd ->
	    io:format("there are books of 2nd version.~n");
        true ->
	    io:format("no books of 1st or 2nd version.~n")
    end,

    io:format("------------------------the 4th------------------------~n"),
    if
        Bool_1st ->
	    io:format("there are books of 1st version.~n");
	true ->
            io:format("there is no book of 1st version.~n")
    end,
    if
        Bool_2nd ->
            io:format("there are books of 2nd version.~n");
	true ->
            io:format("there is no book of 2nd version.~n")
    end,
    if
        not Bool_1st, not Bool_2nd ->
            io:format("no book of 1st or 2nd version.~n");
	%% if there is only , a runtime error throws as follows:
        %%    ** exception error: no true branch found when evaluating an if expression
        true ->
            io:format("there are books of 1st or 2nd version.~n")
    end.

%% ====================END========================
