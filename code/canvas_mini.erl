%% author: xzm
%% date: 2014-11-11

%% the module demonstrates the usage of  wxDC through  a serires of canvas drawing operations.

-module( canvas_mini).

%% -compile(export_all).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

canvas_mini() ->
%%    ----
     Wx=wx:new(),
     Frm=wxFrame:new(Wx, -1, "canvas mini"),
     wxFrame:show(Frm),

    Dc = wxClientDC:new(Frm),
    Pen = wxPen:new(),
    wxDC:setPen(Dc, Pen),
    wxDC:drawLine(Dc, {100,100},{200,200}),

%%    wxFrame:destroy(Frm).

    ok.

%%    ================================================

start() ->
    canvas_mini(),
    ok.

%%    ================================================
