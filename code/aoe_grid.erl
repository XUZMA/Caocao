%% author: xzm
%% date: 2014-11-14, 11-17

-module(aoe_grid).
-export([start/0]).

%% 
%% pre-assumption: 
%%        let cell be a square.
%% ?cell_side is the length of cell side, in pixel.
%%        the user should modify the macro according to the actual situation.
-define(cell_side, 40).

%% Pre-requisite:
%%    All of X, Angle, Radius appear in quadrant I.
%%    X is an integer, in the uint of ?cell_side.
%%    The return value is also an integer, in the uint of ?cell_side.
y_point_on_line(Ox, Angle,Radius) ->
    Half_pi = math:pi()/2,
    if
        Angle == Half_pi -> erlang:trunc(Radius/?cell_side) ;
	true -> erlang:trunc(Ox * ?cell_side * math:tan(Angle)/?cell_side) 
    end.

%% Pre-requisite:
%%    All of X, Angle, Radius appear in quadrant I.
%%    X is an integer, in the uint of ?cell_side.
%%    The return value is also an integer, in the uint of ?cell_side.
y_point_on_arc(Ox, Radius)->
    erlang:trunc(math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side) .

%% pre-requisite:
%%    All of Angle_s,Angle_e,Radius are in quadrant I, i.e.,
%%    0 =< Angle_s < Angle_e =< math:pi()/2, and Radius > 0.
%% Reture vales:
%%    in the grid, every cell is represented by its LB(Left Bottom corner point).
aoe_grids_quadrant1(Angle_s,Angle_e,Radius)->
    List_angle = [Angle_s, Angle_e],
    [X_s,X_e] = lists:map(fun(X)->Radius*math:cos(X) end,List_angle),
    [Ox_s, Ox_e] =lists:map(
        fun(Z) -> erlang:trunc(Z/?cell_side) end,
        [X_s, X_e]),

    %% from 0 to Ox_e -1
    lists:map(
        fun(Ox)-> {Ox,
            y_point_on_line(Ox,Angle_s,Radius),
            y_point_on_line(Ox+1,Angle_e,Radius)}
        end,
        lists:seq(0,Ox_e-1,1))
    ++
    %% from Ox_e to Ox_s
    lists:map(
        fun(Ox)-> {Ox,
            y_point_on_line(Ox,Angle_s,Radius),
            y_point_on_arc(Ox,Radius)}
        end,
        lists:seq(Ox_e,Ox_s,1)).

aoe_grids_quadrant1_test()->
    Angle_s = math:pi()/6,
    Angle_e = math:pi()/3,
    Radius = 400,
    List_LB = aoe_grids_quadrant1(Angle_s,Angle_e,Radius),
    io:format("~w~n",[List_LB]).

%% ====================TEST PART==================

test() ->
    aoe_grids_quadrant1_test(),
    ok.

start() ->
    test().

%% ========================END=======================
