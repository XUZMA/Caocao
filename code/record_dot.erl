%% author: xzm
%% date: 2014-10-21/22
%% module(record_dot) is to test the assignment of record and dot operation after record.
%% Notice:
%%    The dot operation result after the struct declaration and instantiation different.
%%    The dot operation result after the struct declaration is the ordinal ineger, beginning at 2;
%%    while the dot operation result after the struct declaration instantiation return the actual field value.

-module(record_dot).
-export([start/0]).

-record(book_name, {fullname,author,vsn,date,pub,isbn,pages}).

start()->
    PEr2nd_ = #book_name{
        fullname  = "Progranmming Erlang, Software For a Concurrent World",
        author    = "Joe Armstrong",
        vsn       = "2nd",
        date      = "2013-10-05",
        pub       = "Pragmatic Bookshelf",
        isbn      = "9781937785536"
        },
    PEr2nd = PEr2nd_#book_name{
        pages     = "548 pages"
        },

    PEl1st = #book_name{
        fullname  = "Programming Elixir",
        author    = "Dave Thomas",
        vsn       = "1st",
        date      = "2014-10-15",
        pub       = "Pragmatic Bookshelf",
        isbn      = "9781937785581",
        pages     = "240 pages"
        },

    #book_name{fullname=FULLNAME,author=AUTHOR,vsn=VSN,date=DATE,pub=PUB,isbn=ISBN,pages=PAGES} = PEr2nd,
    #book_name{fullname=Fullname,author=Author,vsn=Vsn,date=Date,pub=Pub,isbn=Isbn,pages=Pages} = PEl1st,

    io:format("~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n",[FULLNAME,AUTHOR,VSN,DATE,PUB,ISBN,PAGES]),
    io:format("~s~n~s~n~s~n~s~n~s~n~s~n~s~n~n",[Fullname,Author,Vsn,Date,Pub,Isbn,Pages]),

    io:format("record dot operation:~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n",
        [
            %% #book_name.book_name,
            %% the above line can not pass compiler.
            %% erlc record_dot.erl:34: field book_name undefined in record book_name
        #book_name.fullname,
            %% NOTICE: the 1st oridinal is 2!
            %% Just like in ets table(set/ordered_set/bag/duplicated_bag), for each key-value term, usually the key's ordinal is 1, and ets:lookup_element(tab1,key1,1) returns the list of key1.
        #book_name.author,
        #book_name.vsn,
        #book_name.date,
        #book_name.pub,
        #book_name.isbn,
        #book_name.pages
        ]),

    io:format("record instantiation dot operation:~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n",
        [
        PEr2nd#book_name.fullname,
        PEr2nd#book_name.author,
        PEr2nd#book_name.vsn,
        PEr2nd#book_name.date,
        PEr2nd#book_name.pub,
        PEr2nd#book_name.isbn,
        PEr2nd#book_name.pages
        ]),

    io:format("record instantiation dot operation:~n~s~n~s~n~s~n~s~n~s~n~s~n~s~n~n",
        [
        PEl1st#book_name.fullname,
        PEl1st#book_name.author,
        PEl1st#book_name.vsn,
        PEl1st#book_name.date,
        PEl1st#book_name.pub,
        PEl1st#book_name.isbn,
        PEl1st#book_name.pages
        ]),

    ok.
