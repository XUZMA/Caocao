%% author: xzm
%% date: 2014-11-10

%% the module demonstrates the usage of  wxMessageDialog through  a departure Dialog.

-module( wxMessageDialog_demo).
-export([start/0]).
%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

byeDialog() ->
    Prompt = "Bye!",
    CaptionInfo = "logout system...",

    wx:new(),
    Dialog = wxMessageDialog:new(wx:null(),Prompt,[{caption,CaptionInfo},{style,?wxOK bor ?wxICON_INFORMATION}]),
    wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog),
    ok.

%%    ================================================

start() ->
    byeDialog().

%%    ================================================
