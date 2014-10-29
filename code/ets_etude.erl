%% author: XZM
%% date: 2014-10-29

%% http://erlang.org/doc/man/ets.html
%%                --------------------------------
%% tab2list
%%
%% tab2list(Tab) -> [Object]
%% Types:
%% Tab = tab()
%% Object = tuple()
%% Returns a list of all objects in the table Tab.
%%                --------------------------------
%% new
%%
%% new(Name, Options) -> tid() | atom()
%% Types:
%% Name = atom()
%% Options = [Option]
%% Option = Type
%%        | Access
%%        | named_table
%%        | {keypos, Pos}
%%        | {heir, Pid :: pid(), HeirData}
%%        | {heir, none}
%%        | Tweaks
%% Type = type()
%% Access = access()
%% Tweaks = {write_concurrency, boolean()}
%%        | {read_concurrency, boolean()}
%%        | compressed
%% Pos = integer() >= 1
%% HeirData = term()
%% Creates a new table and returns a table identifier which can be used in subsequent operations. The table identifier can be sent to other processes so that a table can be shared between different processes within a node.
%% The parameter Options is a list of atoms which specifies table type, access rights, key position and if the table is named or not. If one or more options are left out, the default values are used. This means that not specifying any options ([]) is the same as specifying [set, protected, {keypos,1}, {heir,none}, {write_concurrency,false}, {read_concurrency,false}].
%% set
%%                 The table is a set table - one key, one object, no order among objects. This is the default table type.
%% ordered_set
%%                 The table is a ordered_set table - one key, one object, ordered in Erlang term order, which is the order implied by the < and > operators. Tables of this type have a somewhat different behavior in some situations than tables of the other types. Most notably the ordered_set tables regard keys as equal when they compare equal, not only when they match. This means that to an ordered_set, the integer() 1 and the float() 1.0 are regarded as equal. This also means that the key used to lookup an element not necessarily matches the key in the elements returned, if float()'s and integer()'s are mixed in keys of a table.
%% bag
%%                 The table is a bag table which can have many objects, but only one instance of each object, per key.
%% duplicate_bag
%%                 The table is a duplicate_bag table which can have many objects, including multiple copies of the same object, per key.
%% public Any process may read or write to the table.
%% protected The owner process can read and write to the table. Other processes can only read the table. This is the default setting for the access rights.
%% private Only the owner process can read or write to the table.
%% named_table If this option is present, the name Name is associated with the table identifier. The name can then be used instead of the table identifier in subsequent operations.
%% {keypos,Pos} Specfies which element in the stored tuples should be used as key. By default, it is the first element, i.e. Pos=1. However, this is not always appropriate. In particular, we do not want the first element to be the key if we want to store Erlang records in a table.
%% Note that any tuple stored in the table must have at least Pos number of elements.
%% {heir,Pid,HeirData} | {heir,none}
%% Set a process as heir. The heir will inherit the table if the owner terminates. The message {'ETS-TRANSFER',tid(),FromPid,HeirData} will be sent to the heir when that happens. The heir must be a local process. Default heir is none, which will destroy the table when the owner terminates.
%% {write_concurrency,boolean()} Performance tuning. Default is false, in which case an operation that mutates (writes to) the table will obtain exclusive access, blocking any concurrent access of the same table until finished. If set to true, the table is optimized towards concurrent write access. Different objects of the same table can be mutated (and read) by concurrent processes. This is achieved to some degree at the expense of memory consumption and the performance of sequential access and concurrent reading. The write_concurrency option can be combined with the read_concurrency option. You typically want to combine these when large concurrent read bursts and large concurrent write bursts are common (see the documentation of the read_concurrency option for more information). Note that this option does not change any guarantees about atomicy and isolation. Functions that makes such promises over several objects (like insert/2) will gain less (or nothing) from this option.
%% In current implementation, table type ordered_set is not affected by this option. Also, the memory consumption inflicted by both write_concurrency and read_concurrency is a constant overhead per table. This overhead can be especially large when both options are combined.
%% {read_concurrency,boolean()} Performance tuning. Default is false. When set to true, the table is optimized for concurrent read operations. When this option is enabled on a runtime system with SMP support, read operations become much cheaper; especially on systems with multiple physical processors. However, switching between read and write operations becomes more expensive. You typically want to enable this option when concurrent read operations are much more frequent than write operations, or when concurrent reads and writes comes in large read and write bursts (i.e., lots of reads not interrupted by writes, and lots of writes not interrupted by reads). You typically do not want to enable this option when the common access pattern is a few read operations interleaved with a few write operations repeatedly. In this case you will get a performance degradation by enabling this option. The read_concurrency option can be combined with the write_concurrency option. You typically want to combine these when large concurrent read bursts and large concurrent write bursts are common.
%% compressed If this option is present, the table data will be stored in a more compact format to consume less memory. The downside is that it will make table operations slower. Especially operations that need to inspect entire objects, such as match and select, will get much slower. The key element is not compressed in current implementation.
%%                --------------------------------
%% delete
%%
%% delete(Tab) -> true
%% Types:
%% Tab = tab()
%% Deletes the entire table Tab.
%%
%% delete(Tab, Key) -> true
%% Types:
%% Tab = tab()
%% Key = term()
%% Deletes all objects with the key Key from the table Tab.
%%                --------------------------------
%% insert
%%
%% insert(Tab, ObjectOrObjects) -> true
%% Types:
%% Tab = tab()
%% ObjectOrObjects = tuple() | [tuple()]
%% Inserts the object or all of the objects in the list ObjectOrObjects into the table Tab. If the table is a set and the key of the inserted objects matches the key of any object in the table, the old object will be replaced. If the table is an ordered_set and the key of the inserted object compares equal to the key of any object in the table, the old object is also replaced. If the list contains more than one object with matching keys and the table is a set, one will be inserted, which one is not defined. The same thing holds for ordered_set, but will also happen if the keys compare equal.
%% The entire operation is guaranteed to be atomic and isolated, even when a list of objects is inserted.
%%                --------------------------------
%% insert_new
%%
%% insert_new(Tab, ObjectOrObjects) -> boolean()
%% Types:
%% Tab = tab()
%% ObjectOrObjects = tuple() | [tuple()]
%% This function works exactly like insert/2, with the exception that instead of overwriting objects with the same key (in the case of set or ordered_set) or adding more objects with keys already existing in the table (in the case of bag and duplicate_bag), it simply returns false. If ObjectOrObjects is a list, the function checks every key prior to inserting anything. Nothing will be inserted if not all keys present in the list are absent from the table. Like insert/2, the entire operation is guaranteed to be atomic and isolated.
%%                --------------------------------
%%
%% lookup(Tab, Key) -> [Object]
%% Types:
%% Tab = tab()
%% Key = term()
%% Object = tuple()
%% Returns a list of all objects with the key Key in the table Tab.
%% In the case of set, bag and duplicate_bag, an object is returned only if the given key matches the key of the object in the table. If the table is an ordered_set however, an object is returned if the key given compares equal to the key of an object in the table. The difference being the same as between =:= and ==. As an example, one might insert an object with the integer() 1 as a key in an ordered_set and get the object returned as a result of doing a lookup/2 with the float() 1.0 as the key to search for.
%% If the table is of type set or ordered_set, the function returns either the empty list or a list with one element, as there cannot be more than one object with the same key. If the table is of type bag or duplicate_bag, the function returns a list of arbitrary length.
%% Note that the time order of object insertions is preserved; the first object inserted with the given key will be first in the resulting list, and so on.
%% Insert and look-up times in tables of type set, bag and duplicate_bag are constant, regardless of the size of the table. For the ordered_set data-type, time is proportional to the (binary) logarithm of the number of objects.
%%                --------------------------------
%% lookup_element(Tab, Key, Pos) -> Elem
%%
%% Types:
%% Tab = tab()
%% Key = term()
%% Pos = integer() >= 1
%% Elem = term() | [term()]
%% If the table Tab is of type set or ordered_set, the function returns the Pos:th element of the object with the key Key.
%% If the table is of type bag or duplicate_bag, the functions returns a list with the Pos:th element of every object with the key Key.
%% If no object with the key Key exists, the function will exit with reason badarg.
%% The difference between set, bag and duplicate_bag on one hand, and ordered_set on the other, regarding the fact that ordered_set's view keys as equal when they compare equal whereas the other table types only regard them equal when they match, naturally holds for lookup_element as well as for lookup.
%%                --------------------------------
%%
%% update_counter(Tab, Key, UpdateOp) -> Result
%% update_counter(Tab, Key, UpdateOp :: [UpdateOp]) -> [Result]
%% update_counter(Tab, Key, Incr) -> Result
%% Types:
%% Tab = tab()
%% Key = term()
%% UpdateOp = {Pos, Incr} | {Pos, Incr, Threshold, SetValue}
%% Pos = Incr = Threshold = SetValue = Result = integer()
%% This function provides an efficient way to update one or more counters, without the hassle of having to look up an object, update the object by incrementing an element and insert the resulting object into the table again. (The update is done atomically; i.e. no process can access the ets table in the middle of the operation.)
%% It will destructively update the object with key Key in the table Tab by adding Incr to the element at the Pos:th position. The new counter value is returned. If no position is specified, the element directly following the key (<keypos>+1) is updated.
%% If a Threshold is specified, the counter will be reset to the value SetValue if the following conditions occur:
%% The Incr is not negative (>= 0) and the result would be greater than (>) Threshold
%% The Incr is negative (< 0) and the result would be less than (<) Threshold
%% A list of UpdateOp can be supplied to do several update operations within the object. The operations are carried out in the order specified in the list. If the same counter position occurs more than one time in the list, the corresponding counter will thus be updated several times, each time based on the previous result. The return value is a list of the new counter values from each update operation in the same order as in the operation list. If an empty list is specified, nothing is updated and an empty list is returned. If the function should fail, no updates will be done at all.
%% The given Key is used to identify the object by either matching the key of an object in a set table, or compare equal to the key of an object in an ordered_set table (see lookup/2 and new/2 for details on the difference).
%% The function will fail with reason badarg if:
%% the table is not of type set or ordered_set,
%% no object with the right key exists,
%% the object has the wrong arity,
%% the element to update is not an integer,
%% the element to update is also the key, or,
%% any of Pos, Incr, Threshold or SetValue is not an integer
%%                --------------------------------
%% update_element(Tab, Key, ElementSpec :: {Pos, Value}) -> boolean()
%% update_element(Tab, Key, ElementSpec :: [{Pos, Value}]) ->
%%                   boolean()
%% Types:
%% Tab = tab()
%% Key = term()
%% Value = term()
%% Pos = integer() >= 1
%% This function provides an efficient way to update one or more elements within an object, without the hassle of having to look up, update and write back the entire object.
%% It will destructively update the object with key Key in the table Tab. The element at the Pos:th position will be given the value Value.
%% A list of {Pos,Value} can be supplied to update several elements within the same object. If the same position occurs more than one in the list, the last value in the list will be written. If the list is empty or the function fails, no updates will be done at all. The function is also atomic in the sense that other processes can never see any intermediate results.
%% The function returns true if an object with the key Key was found, false otherwise.
%% The given Key is used to identify the object by either matching the key of an object in a set table, or compare equal to the key of an object in an ordered_set table (see lookup/2 and new/2 for details on the difference).
%% The function will fail with reason badarg if:
%% the table is not of type set or ordered_set,
%% Pos is less than 1 or greater than the object arity, or,
%% the element to update is also the key
%%



-module(ets_etude).
-export([start/0]).

start() ->
    lists:foreach(fun test_ets/1,
		  [set, ordered_set, bag, duplicate_bag]).

test_ets(Mode) ->
    TableId = ets:new(test, [Mode]),

    ets:insert(TableId, {key1,1,1}),
    ets:insert(TableId, {key1,1,2}),
    ets:insert(TableId, {key1,2,1}),
    ets:insert(TableId, {key1,2,2}),
    ets:insert(TableId, {key1,2,2}),
    List1 = ets:tab2list(TableId),
    io:format("List1 = ~-13w  => ~p~n", [Mode, List1]),

    ets:insert_new(TableId, {key1,1,2,3}),
    ets:insert_new(TableId, {key1,1,2,3,4}),
    List2 = ets:tab2list(TableId),
    io:format("List2 = ~-13w  => ~p~n", [Mode, List2]),

    ets:insert(TableId, {key1,1,2,3}),
    ets:insert(TableId, {key1,1,2,3,4}),
    List3 = ets:tab2list(TableId),
    io:format("List3 = ~-13w  => ~p~n", [Mode, List3]),

    ets:insert_new(TableId, {key2,2,2}),
    List4 = ets:tab2list(TableId),
    io:format("List4 = ~-13w  => ~p~n", [Mode, List4]),

    io:format("lookup list(key1)  => ~p~n", [ets:lookup(TableId, key1)]),
    %% NOTICE: the Pos:th element of the object with the key Key.
    %% Notice the Pos:th as follows:
    %%     the primary key is placed at the 1st place, whose pos is 1.
    io:format("pos(1) elements    => ~p~n", [ets:lookup_element(TableId, key1,1)]),
    io:format("pos(2) elements    => ~p~n", [ets:lookup_element(TableId, key1,2)]),
    io:format("pos(3) elements    => ~p~n", [ets:lookup_element(TableId, key1,3)]),
    %%  when pos>length(tuple), the 2 statements below can cause runtime error:
    %%    io:format("pos(4) elements    => ~p~n", [ets:lookup_element(TableId, key1,4)]),
    %%    io:format("pos(5) elements    => ~p~n", [ets:lookup_element(TableId, key1,5)]),

    case Mode of
        bag ->
            io:format("Mode = ~-13w,   update_element - nop~n", [Mode]);
	duplicate_bag ->
            io:format("Mode = ~-13w,   update_element - nop~n", [Mode]);
	_ ->
            ets:update_element(TableId, key2, {2,100}),
            List5 = ets:tab2list(TableId),
            io:format("Mode = ~-13w,   update_element - List5 => ~p~n", [Mode, List5])
	end,
    
%set,ordered_set
 
    ets:delete(TableId, key1),
    List6 = ets:tab2list(TableId),
    io:format("List6 = ~-13w  => ~p~n", [Mode, List6]),

    ets:delete(TableId),
    io:format("------------------------------------------------~n").
