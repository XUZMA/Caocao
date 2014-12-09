%% author: xzm
%% date: 2014-12-09
%% 

%% http://erlang.org/doc/reference_manual/records.html
%% 10.8  Internal Representation of Records
%% Record expressions are translated to tuple expressions during compilation.
%%  A record defined as
%% -record(Name, {Field1,...,FieldN}).
%% is internally represented by the tuple
%% {Name,Value1,...,ValueN}
%% where each ValueI is the default value for FieldI.
%% To each module using records, a pseudo function is added during compilation to obtain information about records:
%% record_info(fields, Record) -> [Field]
%% record_info(size, Record) -> Size
%% Size is the size of the tuple representation, that is one more than the number of fields.
%% In addition, #Record.Name returns the index in the tuple representation of Name of the record Record. Name must be an atom.

%% http://erlang.org/pipermail/erlang-questions/2007-January/024666.html
%% record_info is not a proper function. It only exists during compilation, which means that it cannot take variable arguments.


-module(record_pseudo_functions).
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
    %%    Record_field_list = record_info(fields,PEr2nd),
    %%    Record_size = record_info(size,PEr2nd),

    Record_field_list = record_info(fields,book_name),
    Record_size = record_info(size,book_name),

    io:format("record field list = ~p~n",[Record_field_list]),
    io:format("record size = ~p~n",[Record_size]),

    ok.
