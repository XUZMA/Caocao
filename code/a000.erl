%% author: xzm
%% date: 2014-12-09
%% 

%% a template to write simple test modules

-module('a000').
-export([start/0]).

-record(book_name, {fullname,author,vsn,date,pub,isbn,pages}).

start()->
    %% PEr2nd = #book_name{
    %%     fullname  = "Progranmming Erlang, Software For a Concurrent World",
    %%     author    = "Joe Armstrong",
    %%     vsn       = "2nd",
    %%     date      = "2013-10-05",
    %%     pub       = "Pragmatic Bookshelf",
    %%     isbn      = "9781937785536",
    %%     pages     = "548 pages"
    %%     },

    Record_field_list = record_info(fields,book_name),
    Record_size = record_info(size,book_name),

    io:format("record field list = ~p~n",[Record_field_list]),
    io:format("record size = ~p~n",[Record_size]),

    ok.
