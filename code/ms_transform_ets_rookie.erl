%% author: xzm
%% date: 2014.12.4
%% Reference: http://erlang.org/doc/man/ms_transform.html

-module(ms_transform_ets_rookie).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start/0]).

-record(emp,
        {empno,
            %% Employee number as a string,the key
        surname,
            %% Surname of the employee
        givenname,
            %% Given name of employee
        dept,
            %% Department, one of{dev,sales,prod,adm}
        empyear}).
            %% Year that the employee was employed

start()->

    Record_list =
            [{emp,"011103","Black","Alfred",sales,2000},
             {emp,"041231","Doe","John",prod,2001},
             {emp,"052341","Smith","John",dev,1997},
             {emp,"076324","Smith","Ella",sales,1995},
             {emp,"122334","Weston","Anna",prod,2002},
             {emp,"535216","Chalker","Samuel",adm,1998},
             {emp,"789789","Harrysson","Joe",adm,1996},
             {emp,"963721","Scott","Juliana",dev,2003},
             {emp,"989891","Brown","Gabriel",prod,1999}],
    
    ets:new(emp_tab,[{keypos,#emp.empno},named_table,ordered_set]),

    ets:insert(emp_tab, Record_list),

    Select_list =
        ets:select(
            emp_tab,
            ets:fun2ms(
                fun(#emp{empno=E,surname="Smith"})->{guru,E};
                    (#emp{empno=E,empyear=Y})when Y<1997->{inventory,E};
                    (#emp{empno=E,empyear=Y})when Y>2001->{newbie,E};
                    (#emp{empno=E,empyear=Y})->{rookie,E}end)),

    lists:map(fun({Rank, Emp_no})-> io:format("~p  ~s~n",[Rank, Emp_no]) end,Select_list),

    io:format("------------------------------------------------~n"),

    io:format("~p~n",[Select_list]).
