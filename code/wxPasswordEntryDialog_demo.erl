%% author: xzm
%% date: 2014-11-10

%% the module demonstrates the usage of  wxPasswordEntryDialog through  a Dialog to get account password.

-module(wxPasswordEntryDialog_demo).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

passwordDialog() ->
    Prompt = "please enter the password for current account:",
    CaptionInfo = "login system...",

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
