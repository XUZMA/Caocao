%% author: xzm
%% date: 2014-11-17

%% the module draw test figures for aoe_grid.erl
%% the bug on the output picture is caused by float precision.

-module(aoe_grid_test_pic).

%% -compile(export_all).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

-define(cell_unit, 40).
-define(square_side, ?cell_unit).

-define(center_x, 800).
-define(center_y, 400).
-define(radius, 400).

draw_axes(Dc) ->
    wxDC:drawLine(Dc,{800,0}, {800,820}),
    wxDC:drawLine(Dc,{0,400}, {2000,400}).

draw_circle(Dc) ->
    wxDC:drawCircle(Dc,{?center_x,?center_y},?radius).

%% draw_arc(Dc) ->
%%     wxDC:drawArc(Dc, 
%%         {?center_x+erlang:trunc(?radius*math:cos(math:pi()/3)),?center_y+erlang:trunc(?radius*math:sin(math:pi()/3))},
%%         {?center_x+erlang:trunc(?radius*math:cos(math:pi()/6)),?center_y+erlang:trunc(?radius*math:sin(math:pi()/6))},
%%         {?center_x,?center_y}).

draw_lines(Dc) ->

    Angle_s = math:pi()/6,
    Angle_e = math:pi()/3,
    Half_pi = math:pi()/2,

    %% draw lines in quadrant I
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
        {erlang:trunc(?center_x+?radius*math:cos(Angle_s)), erlang:trunc(?center_y+?radius*math:sin(Angle_s))}),
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
      {erlang:trunc(?center_x+?radius*math:cos(Angle_e)), erlang:trunc(?center_y+?radius*math:sin(Angle_e))}),

    %% draw lines in quadrant II
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
        {erlang:trunc(?center_x+?radius*math:cos(Angle_s+Half_pi)), erlang:trunc(?center_y+?radius*math:sin(Angle_s+Half_pi))}),
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
      {erlang:trunc(?center_x+?radius*math:cos(Angle_e+Half_pi)), erlang:trunc(?center_y+?radius*math:sin(Angle_e+Half_pi))}),

    %% draw lines in quadrant III
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
        {erlang:trunc(?center_x+?radius*math:cos(Angle_s+2*Half_pi)), erlang:trunc(?center_y+?radius*math:sin(Angle_s+2*Half_pi))}),
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
      {erlang:trunc(?center_x+?radius*math:cos(Angle_e+2*Half_pi)), erlang:trunc(?center_y+?radius*math:sin(Angle_e+2*Half_pi))}),

    %% draw lines in quadrant IV
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
        {erlang:trunc(?center_x+?radius*math:cos(Angle_s+3*Half_pi)), erlang:trunc(?center_y+?radius*math:sin(Angle_s+3*Half_pi))}),
    wxDC:drawLine(
        Dc,
        {?center_x,?center_y},
      {erlang:trunc(?center_x+?radius*math:cos(Angle_e+3*Half_pi)), erlang:trunc(?center_y+?radius*math:sin(Angle_e+3*Half_pi))}).

draw_squares_vertical(Dc,{Ox,Oy1,Oy2})->
    lists:map(
        fun(Oy) -> wxDC:drawRectangle(Dc, {?center_x+?square_side*Ox,?center_y+?square_side*Oy}, {?square_side,?square_side}) end, 
        lists:seq(Oy1,Oy2,1)).

draw_squares(Dc,Tuplelist_LB)->
    lists:map(
        fun({Ox,Oy1,Oy2})->draw_squares_vertical(Dc,{Ox,Oy1,Oy2}) end,
        Tuplelist_LB).

canvas_mini() ->

%%    ---- initialize drawing environment ----
     Wx=wx:new(),
     Frm=wxFrame:new(Wx, -1, "canvas mini", [{pos,{0,0}},{size, {2000, 820}}]),
     wxFrame:show(Frm),
    Dc = wxClientDC:new(Frm),
    Pen = wxPen:new(),
    wxPen:setColour(Pen, 255,0,0),
    wxDC:setPen(Dc, Pen),
    Brush = wxBrush:new(),
    wxBrush:setStyle(Brush,?wxTRANSPARENT),
    wxDC:setBrush(Dc,Brush),

%%    ---- draw axes and circle
    draw_circle(Dc),
    draw_axes(Dc),

%%    ---- draw squares
    wxPen:setColour(Pen, 0,0,255),
    wxDC:setPen(Dc, Pen),

    Tuplelist_LB_Quadrant1 = [{0,0,1},{1,0,3},{2,1,5},{3,1,6},{4,2,8},{5,2,8},{6,3,8},{7,4,7},{8,4,6}],
    draw_squares(Dc,Tuplelist_LB_Quadrant1),

    Tuplelist_LB_Quadrant2 = [{-1,0,1},{-2,0,3},{-3,1,5},{-4,1,6},{-5,2,9},{-6,2,8},{-7,3,8},{-8,4,7},{-9,4,6}],
    draw_squares(Dc,Tuplelist_LB_Quadrant2),

    Tuplelist_LB_Quadrant3 = [{-1,-2,-1},{-2,-4,-1},{-3,-6,-2},{-4,-7,-2},{-5,-9,-3},{-6,-9,-3},{-7,-9,-4},{-8,-8,-5},{-9,-7,-5}],    
    draw_squares(Dc,Tuplelist_LB_Quadrant3),

    Tuplelist_LB_Quadrant4 = [{0,-2,-1},{1,-4,-1},{2,-6,-2},{3,-7,-2},{4,-10,-3},{5,-9,-3},{6,-9,-4},{7,-8,-5},{8,-7,-5}],
    draw_squares(Dc,Tuplelist_LB_Quadrant4),

%%    ---- draw the border of the sector
    wxPen:setColour(Pen, 255,0,0),
    wxDC:setPen(Dc, Pen),
    draw_lines(Dc),

%%    wxFrame:destroy(Frm).

    ok.

%%    ================================================

start() ->
    canvas_mini(),
    ok.

%%    ================================================
