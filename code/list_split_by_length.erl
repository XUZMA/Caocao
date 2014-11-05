%% author: XZM
%% date: 2014-10-27

-module(list_split_by_length).
-export([print_list_element/1,start/0]).

%% ================================================
%% http://erlang.org/doc/man/lists.html#reverse-1
%% reverse(List1) -> List2
%% Types:
%% List1 = List2 = [T]
%% T = term()
%% Returns a list with the elements in List1 in reverse order.
%% reverse(List1, Tail) -> List2
%% Types:
%% List1 = [T]
%% Tail = term()
%% List2 = [T]
%% T = term()
%% Returns a list with the elements in List1 in reverse order, with the tail Tail appended. For example:
%% > lists:reverse([1, 2, 3, 4], [a, b, c]).
%% [4,3,2,1,a,b,c]

%% ================================================
%%
%% functionality: split a list into a series of sub-lists of n elements except the last one which might  has <=n elements.
%%
split_by_length(List, N) ->
    split_by_length__(List, N, {0,[]}, []).

%% In the definition of split_by_length__(List, N, {M,SubL}, Other),
%% M =< N.
%%
%% NOTICE: M is the element number of SubL!!!
%%
%% the heading N elements of List are move to SubL one by one on the first place;
%% when SubL has N elements, the list is 
%%
%% split_by_length__([], N, {N, SubL}, Other) ->
%%    ========>>>
split_by_length__([], _N, {_M, SubL}, Other) ->
    lists:reverse([lists:reverse(SubL) | Other]);

%% in the following clause,
%% reverse the reversed SubL, and append it to the head of Other;
%% set M at 0,SubL at [H], and iterate.
split_by_length__([H | List], N, {N, SubL}, Other) ->
    split_by_length__(
                        List,
                        N,
%%                        {0, [H]},
%%    ========>>>
                        {1, [H]},
                        [lists:reverse(SubL) | Other]
                        );

%% in the following clause, 
%% the parameters N and other remain unchanged,
%% M increments to N,
%% and the first N-M elements in [H|List] are appended to the head of SubL one by one.
split_by_length__([H | List], N, {M, SubL}, Other) ->
    split_by_length__(
                        List,
                        N,
                        {M+1, [H | SubL]},
                        Other).
%% 
%% If split_by_length__([], N, {M, SubL}, Other) 
%% is defined as
%% split_by_length__([], N, {N, SubL}, Other),
%% some runtime error will occur as follows:
%% ** exception error: no function clause matching 
%% Simulation:
%%     split_by_length([1,2,3,4,5], 3)
%%        -------->
%%     split_by_length__(
%%                    [1,2,3,4,5],
%%                    3,
%%                    {0,[]},
%%                    [])
%%        -------->
%%     split_by_length__(
%%                    [2,3,4,5],
%%                    3,
%%                    {1,[1]},
%%                    [])
%%        -------->
%%     split_by_length__(
%%                    [3,4,5],
%%                    3,
%%                    {2,[2,1]},
%%                    [])
%%        -------->
%%     split_by_length__(
%%                    [4,5],
%%                    3,
%%                    {3,[3,2,1]},
%%                    [])
%%        -------->
%%     split_by_length__(
%%                    [5],
%%                    3,
%%                    {0,[4]},
%%                    [[1,2,3]])
%%        -------->
%%     split_by_length__(
%%                    [],
%%                    3,
%%                    {1,[5,4]},
%%                    [[1,2,3]])
%%
%% Here appears a runtime error.
%% If split_by_length__([], N, {N, SubL}, Other) 
%% is defined as
%% split_by_length__([], N, {M, SubL}, Other),
%% program proceed as follows:
%%        -------->
%%    lists:reverse([lists:reverse([5,4]) | [1,2,3]])
%%        -------->
%%    lists:reverse([[4,5] | [1,2,3]])
%%        -------->
%%    [[1,2,3],[4,5]]
%% ------------------------------------------------

print_list_element([]) ->
    ok;
print_list_element([H|T]) ->
    io:format("~p~n",[H]),
    print_list_element(T).

%% ================================================

start() ->
    List_test = [1,2,3,4,5],
    io:format("~p~n", [List_test]),
    print_list_element([List_test]),
    print_list_element(List_test),
    print_list_element(lists:reverse(List_test)),
    io:format("------------------------------------------------~n"),
    print_list_element([split_by_length(List_test,1)]),
    print_list_element([split_by_length(List_test,2)]),
    print_list_element([split_by_length(List_test,3)]),
    print_list_element([split_by_length(List_test,4)]),
    print_list_element([split_by_length(List_test,5)]).

%% ========================END=======================
