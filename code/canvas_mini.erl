%% author: xzm
%% date: 2014-11-11/11-12/11-13

%% the module demonstrates the usage of  wxDC through  a serires of canvas drawing operations.

-module( canvas_mini).

%% -compile(export_all).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

%% parameters:
%% Dc::wxDC;
%% X_ro, x coordinate of region origin;
%% Y_ro, y coordinate of region origin;
%% L_r, length of region;
%% W_r, width of region;
%% L_g, length of grid;
%% W_g, width of grid.
draw_grid(Dc,X_ro,Y_ro,L_r,W_r,L_g,W_g) ->
    X_re = X_ro + L_r,
    Y_re = Y_ro + W_r,
    Gn_h = L_r div L_g,
    Gn_v = W_r div W_g,
    List_o_h = lists:seq(0,Gn_h,1),
    List_o_v = lists:seq(0,Gn_v,1),
    List_s_h = lists:map(fun(X) -> X_ro + X * L_g end, List_o_h),
    List_s_v = lists:map(fun(Y) -> Y_ro + Y * W_g end, List_o_v),
    lists:map(fun(X) -> wxDC:drawLine(Dc,{X,Y_ro},{X,Y_re}) end, List_s_h),
    lists:map(fun(Y) -> wxDC:drawLine(Dc,{X_ro,Y},{X_re,Y}) end, List_s_v),
    ok.

canvas_mini() ->

%%    ---- initialize drawing environment ----
     Wx=wx:new(),
     Frm=wxFrame:new(Wx, -1, "canvas mini", [{pos,{0,0}},{size, {2000, 820}}]),
     wxFrame:show(Frm),
    Dc = wxClientDC:new(Frm),
    Pen = wxPen:new(),

%%    ---- draw foreground arc ----

    wxPen:setColour(Pen, 255,0,0),
    wxDC:setPen(Dc, Pen),
    wxDC:drawArc(Dc,{1000,200},{1000,600},{800,400}),
    wxDC:drawArc(Dc,{1150,450},{1150,350},{800,400}),

%%    ---- draw background grid ----

    wxPen:setColour(Pen, 255,255,0),
    wxDC:setPen(Dc, Pen),
    draw_grid(Dc,1150,350,100,100,50,50) ,

    wxPen:setColour(Pen, 255,255,0),
    wxDC:setPen(Dc, Pen),
    draw_grid(Dc,1150,350,100,100,50,50) ,

    wxPen:setColour(Pen, 0,0,255),
    wxDC:setPen(Dc, Pen),
    draw_grid(Dc,1100,300,200,200,100,100) ,

    wxPen:setColour(Pen, 0,255,0),
    wxDC:setPen(Dc, Pen),
    draw_grid(Dc,1000,200,400,400,200,200) ,

    wxPen:setColour(Pen, 255,0,0),
    wxDC:setPen(Dc, Pen),
    draw_grid(Dc,0,0,1600,800,400,400) ,

%%    wxFrame:destroy(Frm).

    ok.

%%    ================================================

start() ->
    canvas_mini(),
    ok.

%%    ================================================
