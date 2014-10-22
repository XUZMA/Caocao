%% author: xzm
%% date: 2014-10-21/22
%% module(record_dot) is to test the assignment of record and dot operation after a record struct.

-module(record_dot).
-export([test/0]).

-record(book_name, {fullname,author,vsn,date,pub,isbn,pages}).

test()->
    PE2nd_1 = #book_name{
        fullname  = "Progranmming Erlang, Software For a Concurrent World",
        author    = "Joe Armstrong",
        vsn       = "2nd",
        date      = "2013-10-05",
        pub       = "Pragmatic Bookshelf",
        isbn      = "9781937785536"
        },
    Pe2nd_2 = PE2nd_1#book_name{
        pages     = "548 pages"
        },

    #book_name{fullname=FULLNAME,author=AUTHOR,vsn=VSN,date=DATE,pub=PUB,isbn=ISBN,pages=PAGES} = Pe2nd_2,

    io:format("~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n",[FULLNAME,AUTHOR,VSN,DATE,PUB,ISBN,PAGES]),
    io:format("~s~n~s~n~s~n~s~n~s~n~s~n~s~n~n",[FULLNAME,AUTHOR,VSN,DATE,PUB,ISBN,PAGES]),

    io:format("~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n",
        [
        #book_name.fullname, %% NOTICE: the 1st oridinal is 2!
        #book_name.author,
        #book_name.vsn,
        #book_name.date,
        #book_name.pub,
        #book_name.isbn,
        #book_name.pages
        ]),

    io:format("~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n",
        [
        Pe2nd_2#book_name.fullname,
        Pe2nd_2#book_name.author,
        Pe2nd_2#book_name.vsn,
        Pe2nd_2#book_name.date,
        Pe2nd_2#book_name.pub,
        Pe2nd_2#book_name.isbn,
        Pe2nd_2#book_name.pages
        ]),

    io:format("~s~n~s~n~s~n~s~n~s~n~s~n~s~n~n",
        [
        Pe2nd_2#book_name.fullname,
        Pe2nd_2#book_name.author,
        Pe2nd_2#book_name.vsn,
        Pe2nd_2#book_name.date,
        Pe2nd_2#book_name.pub,
        Pe2nd_2#book_name.isbn,
        Pe2nd_2#book_name.pages
        ]),

    ok.
