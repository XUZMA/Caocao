%% author: XZM
%% date: 2014-10-27

-module(colon_define).
-export([test_list_print/1,start/0]).

%% ================================================
-define(MSG0_FRIEND_APPLY, 2049:16). % 8:8, 1:8
-define(MSG0_FRIEND_REPLY_APPLY, 2050:16). % 8:8, 2:8
-define(MSG0_FRIEND_DEL, 2051:16). % 8:8, 3:8
-define(MSG0_FRIEND_INFOS, 2052:16). % 8:8, 4:8
-define(MSG0_FRIEND_LIST, 2053:16). % 8:8, 5:8
-define(MSG0_FRIEND_MY_INFO, 2054:16). % 8:8, 6:8
-define(MSG0_FRIEND_MSG0S, 2055:16). % 8:8, 7:8
-define(MSG0_FRIEND_NEAR_GIFT_INFOS, 2056:16). % 8:8, 8:8
-define(MSG0_FRIEND_CHANGE_UNREAD, 2057:16). % 8:8, 9:8
-define(MSG0_FRIEND_ROB_GIFT, 2058:16). % 8:8, 10:8
-define(MSG0_FRIEND_ASK_FOR_GIFT, 2059:16). % 8:8, 11:8
-define(MSG0_FRIEND_REP_ASK_FOR_GIFT, 2060:16). % 8:8, 12:8
-define(MSG0_FRIEND_SENG_GIFT, 2061:16). % 8:8, 13:8
-define(MSG0_FRIEND_SET_GIFT_STAT, 2062:16). % 8:8, 14:8
-define(MSG0_FRIEND_CHANGE_LIST, 2063:16). % 8:8, 15:8

-define(MSG_FRIEND_APPLY, 2049). % 8:8, 1:8
-define(MSG_FRIEND_REPLY_APPLY, 2050). % 8:8, 2:8
-define(MSG_FRIEND_DEL, 2051). % 8:8, 3:8
-define(MSG_FRIEND_INFOS, 2052). % 8:8, 4:8
-define(MSG_FRIEND_LIST, 2053). % 8:8, 5:8
-define(MSG_FRIEND_MY_INFO, 2054). % 8:8, 6:8
-define(MSG_FRIEND_MSGS, 2055). % 8:8, 7:8
-define(MSG_FRIEND_NEAR_GIFT_INFOS, 2056). % 8:8, 8:8
-define(MSG_FRIEND_CHANGE_UNREAD, 2057). % 8:8, 9:8
-define(MSG_FRIEND_ROB_GIFT, 2058). % 8:8, 10:8
-define(MSG_FRIEND_ASK_FOR_GIFT, 2059). % 8:8, 11:8
-define(MSG_FRIEND_REP_ASK_FOR_GIFT, 2060). % 8:8, 12:8
-define(MSG_FRIEND_SENG_GIFT, 2061). % 8:8, 13:8
-define(MSG_FRIEND_SET_GIFT_STAT, 2062). % 8:8, 14:8
-define(MSG_FRIEND_CHANGE_LIST, 2063). % 8:8, 15:8

-define(Binary_msg_friend, <<?MSG_FRIEND_APPLY, ?MSG_FRIEND_REPLY_APPLY, ?MSG_FRIEND_DEL, ?MSG_FRIEND_INFOS,?MSG_FRIEND_LIST, ?MSG_FRIEND_MY_INFO, ?MSG_FRIEND_MSGS, ?MSG_FRIEND_NEAR_GIFT_INFOS, ?MSG_FRIEND_CHANGE_UNREAD, ?MSG_FRIEND_ROB_GIFT, ?MSG_FRIEND_ASK_FOR_GIFT, ?MSG_FRIEND_REP_ASK_FOR_GIFT, ?MSG_FRIEND_SENG_GIFT, ?MSG_FRIEND_SET_GIFT_STAT, ?MSG_FRIEND_CHANGE_LIST>>).

-define(List_msg_friend, [?MSG_FRIEND_APPLY, ?MSG_FRIEND_REPLY_APPLY, ?MSG_FRIEND_DEL, ?MSG_FRIEND_INFOS,?MSG_FRIEND_LIST, ?MSG_FRIEND_MY_INFO, ?MSG_FRIEND_MSGS, ?MSG_FRIEND_NEAR_GIFT_INFOS, ?MSG_FRIEND_CHANGE_UNREAD, ?MSG_FRIEND_ROB_GIFT, ?MSG_FRIEND_ASK_FOR_GIFT, ?MSG_FRIEND_REP_ASK_FOR_GIFT, ?MSG_FRIEND_SENG_GIFT, ?MSG_FRIEND_SET_GIFT_STAT, ?MSG_FRIEND_CHANGE_LIST]).

%% List_msg_friend = [?MSG_FRIEND_APPLY, ?MSG_FRIEND_REPLY_APPLY, ?MSG_FRIEND_DEL, ?MSG_FRIEND_INFOS,?MSG_FRIEND_LIST, ?MSG_FRIEND_MY_INFO, ?MSG_FRIEND_MSGS, ?MSG_FRIEND_NEAR_GIFT_INFOS, ?MSG_FRIEND_CHANGE_UNREAD, ?MSG_FRIEND_ROB_GIFT, ?MSG_FRIEND_ASK_FOR_GIFT, ?MSG_FRIEND_REP_ASK_FOR_GIFT, ?MSG_FRIEND_SENG_GIFT, ?MSG_FRIEND_SET_GIFT_STAT, ?MSG_FRIEND_CHANGE_LIST].
%% It's a syntax error to define a variable out of the scope of functions.

test_list_print([]) ->
    ok;
test_list_print([H|T]) ->
    io:format("~p~n",[H]),
    test_list_print(T).

start() ->
    test_list_print([?Binary_msg_friend]),
    test_list_print(?List_msg_friend).

%% ========================END=======================