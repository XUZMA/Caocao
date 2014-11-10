%% author: xzm
%% date: 2014-11-10

%% the module demonstrates the usage of  wxTextEntryDialog through  a Dialog to get user name.

-module( wxTextEntryDialog_demo).
-export([start/0]).
%%    ================================================

%%-include_lib(“wx/include/wx.hrl”).
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

usernameDialog() ->
    Prompt = "please entry your account name:",
    CaptionInfo = "login system",

    wx:new(),
    Dialog = wxTextEntryDialog:new(wx:null(),Prompt,[{caption,CaptionInfo}]),
    case wxTextEntryDialog:showModal(Dialog) of
        ?wxID_OK ->
            Str = wxTextEntryDialog:getValue(Dialog);
        ?wxID_CANCEL ->
            Str = null
    end,
    wxTextEntryDialog:destroy(Dialog),
    Str.

%%    ================================================

start() ->
    Str = usernameDialog(),
    io:format("~p~n", [Str]),
    ok.

%%    ================================================
