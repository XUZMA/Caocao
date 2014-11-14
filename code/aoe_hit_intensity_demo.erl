%% author: xzm
%% date: 2014-11-13

%% in aoe.erl, 
%% the hit intensity list is 
%% [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%  7,7,7,7,7]
%% the module draw test figures.

-module( aoe_hit_intensity_demo).

%% -compile(export_all).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

-define(square_side, 10).

draw_axes(Dc) ->
    wxDC:drawLine(Dc,{0,0}, {0,820}),
    wxDC:drawLine(Dc,{0,0}, {2000,0}).

draw_arc(Dc) ->
    wxDC:drawArc(Dc,{0,200},{200,0},{0,0}).

draw_squares(Dc)->
    lists:map(
        fun({X,Y})-> wxDC:drawRectangle(Dc, {X,Y}, {?square_side,?square_side}) end,
        lists:map(fun(X)->{X*?square_side,X*?square_side} end,lists:seq(0,80,1))).

canvas_mini() ->

%%    ---- initialize drawing environment ----
     Wx=wx:new(),
     Frm=wxFrame:new(Wx, -1, "canvas mini", [{pos,{0,0}},{size, {2000, 820}}]),
     wxFrame:show(Frm),
    Dc = wxClientDC:new(Frm),
    Pen = wxPen:new(),

%%    ---- draw axes, squares, and circle
    wxPen:setColour(Pen, 255,0,0),
    wxDC:setPen(Dc, Pen),
    draw_axes(Dc),

    wxPen:setColour(Pen, 0,255,0),
    wxDC:setPen(Dc, Pen),
    draw_arc(Dc),

    wxPen:setColour(Pen, 0,0,255),
    wxDC:setPen(Dc, Pen),
    draw_squares(Dc),

%%    wxFrame:destroy(Frm).

    ok.

%%    ================================================

start() ->
    canvas_mini(),
    ok.

%%    ================================================
