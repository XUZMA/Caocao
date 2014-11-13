%% author: xzm
%% date: 2014-11-13

%% the module draw test figures for aoe.erl

-module( aoe_pic).

%% -compile(export_all).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

-define(cell_unit, 40).
-define(square_side, 40).
-define(center_x, 800).
-define(center_y, 400).

draw_axes(Dc) ->
    wxDC:drawLine(Dc,{800,0}, {800,820}),
    wxDC:drawLine(Dc,{0,400}, {2000,400}).

draw_circle(Dc) ->
    wxDC:drawCircle(Dc,{?center_x,?center_y},?cell_unit*4).

draw_square(Dc,K)->
    wxDC:drawRectangle(Dc, {?center_x+?square_side*K,?center_y+?square_side*K}, {?square_side,?square_side}).

canvas_mini() ->

%%    ---- initialize drawing environment ----
     Wx=wx:new(),
     Frm=wxFrame:new(Wx, -1, "canvas mini", [{pos,{0,0}},{size, {2000, 820}}]),
     wxFrame:show(Frm),
    Dc = wxClientDC:new(Frm),
    Pen = wxPen:new(),
    wxPen:setColour(Pen, 255,0,0),
    wxDC:setPen(Dc, Pen),

%%    ---- draw axes and circle
    draw_circle(Dc),
    draw_axes(Dc),

%%    ---- draw squares
    wxPen:setColour(Pen, 0,0,255),
    wxDC:setPen(Dc, Pen),
    List_pt = lists:seq(-6,6,1),
    lists:map(fun(K)->draw_square(Dc,K) end, List_pt),

%%    wxFrame:destroy(Frm).

    ok.

%%    ================================================

start() ->
    canvas_mini(),
    ok.

%%    ================================================
