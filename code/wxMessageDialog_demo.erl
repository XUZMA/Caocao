%% author: xzm
%% date: 2014-11-10

%% the module demonstrates the usage of  wxMessageDialog through  a leaving Dialog.

-module( wxMessageDialog_demo).
-export([start/0]).
%%    ================================================

%%-include_lib(“wx/include/wx.hrl”).
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

byeDialog() ->
    Prompt = "Bye!",
    CaptionInfo = "logout system",

    wx:new(),
    Dialog = wxMessageDialog:new(wx:null(),Prompt,[{caption,CaptionInfo},{style,?wxOK bor ?wxICON_INFORMATION}]),
    wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog),
    ok.

%%    ================================================

start() ->
    byeDialog().

%%    ================================================
