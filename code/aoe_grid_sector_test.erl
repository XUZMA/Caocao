%% author: xzm
%% date: 2014-11-22

%% the module draw test figures for aoe_grid_sector.erl

-module(aoe_grid_sector_test).

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

draw_squares(Dc,N)->
 
TL1 = [{0,0},
       {40,0},
       {40,40},
       {80,0},
       {80,40},
       {120,0},
       {120,40},
       {120,80},
       {160,0},
       {160,40},
       {160,80},
       {200,0},
       {200,40},
       {200,80},
       {200,120},
       {240,0},
       {240,40},
       {240,80},
       {240,120},
       {240,160},
       {280,0},
       {280,40},
       {280,80},
       {280,120},
       {280,160},
       {320,0},
       {320,40},
       {320,80},
       {320,120},
       {320,160},
       {320,200},
       {360,0},
       {360,40},
       {360,80},
       {360,120},
       {360,160}],

TL2 = [{0,0},
       {0,40},
       {40,0},
       {40,40},
       {40,80},
       {40,120},
       {80,40},
       {80,80},
       {80,120},
       {80,160},
       {80,200},
       {120,40},
       {120,80},
       {120,120},
       {120,160},
       {120,200},
       {120,240},
       {160,80},
       {160,120},
       {160,160},
       {160,200},
       {160,240},
       {160,280},
       {160,320},
       {200,80},
       {200,120},
       {200,160},
       {200,200},
       {200,240},
       {200,280},
       {200,320},
       {200,360},
       {200,400},
       {200,440},
       {200,480},
       {200,520},
       {200,560},
       {200,600},
       {200,640},
       {200,680},
       {240,120},
       {240,160},
       {240,200},
       {240,240},
       {240,280},
       {240,320},
       {280,160},
       {280,200},
       {280,240},
       {280,280},
       {320,160},
       {320,200},
       {320,240}],

    TL = 
        case N of
            1 ->
                TL1;
            2 ->
                TL2;
	 _ -> []
        end.
    lists:map(fun({P_x,P_y}) -> wxDC:drawRectangle(Dc, {?center_x+P_x,?center_y+P_y}, {?square_side,?square_side}) end,TL);



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

    draw_squares(Dc,1),

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