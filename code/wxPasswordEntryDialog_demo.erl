%% author: xzm
%% date: 2014-11-10

%% the module demonstrates the usage of  wxPasswordEntryDialog through  a Dialog to get account password.

-module(wxPasswordEntryDialog_demo).
-export([start/0]).

%%    ================================================

%%-include_lib(“wx/include/wx.hrl”).
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

passwordDialog() ->
    Prompt = "please enter your password of current account:",
    CaptionInfo = "login system",

    wx:new(),
    Dialog = wxPasswordEntryDialog:new(wx:null(),Prompt,[{caption,CaptionInfo}]),
    case wxTextEntryDialog:showModal(Dialog) of
        ?wxID_OK ->
            Str = wxPasswordEntryDialog:getValue(Dialog);
        ?wxID_CANCEL ->
            Str = null
    end,
    wxPasswordEntryDialog:destroy(Dialog),
    Str.

%%    ================================================

start() ->
    Str = passwordDialog(),
    io:format("~p~n", [Str]),
    ok.

%%    ================================================
