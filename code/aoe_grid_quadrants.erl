%% author: xzm
%% date: 2014-11-19

%% assume:
%%     the attack region is a sector
%%    cell be a square.
%% 
%% exports:
%%     the function to determin whether a point is in the attack region.

-module(aoe_grid_quadrants).
-export([start/0]).

%% ?cell_side is the length of cell side, in pixel.
%%        the user should modify the macro according to the actual situation.
-define(cell_side, 40).

-define(half_pi, math:pi()/2).
-define(pi, math:pi()).
-define(half_3pi, 3*math:pi()/2).
-define(two_pi, 2*math:pi()).

floor_neg(Neg)->
    Trunc = erlang:trunc(Neg),
    if
        Neg < Trunc -> Trunc -1;
	true -> Trunc
    end.

angle_normal(Angle)->
    Round = erlang:trunc(Angle /?two_pi),
    Angle_t = Angle - Round * ?two_pi,
    if
        Angle_t >=0 -> Angle_t;
        true -> Angle_t + ?two_pi
    end.

%% angle_axis(Angle_n)->
%%     Half_pi = math:pi()/2,
%%     case Angle of 
%%         0 -> 0;
%% 	Half_pi  -> 1;
%% 	Half_pi * 2 -> 2;
%% 	Half_pi * 3 -> 3;
%%         _ -> off_axis
%%     end.

angle_quadrant(Angle_n)->
    erlang:trunc(2 * Angle_n / ?pi).

yp_point_on_ray(Ox, Angle_n) ->
    case Ox of
        0 -> 0;
	_ -> erlang:trunc(Ox * math:tan(Angle_n)) 
    end.

yn_point_on_ray(Ox, Angle_n) ->
    case Ox of
        0  -> -1;
	_  -> floor_neg(Ox * math:tan(Angle_n))
    end.

points_on_ray(Angle_n, Radius)->
    Ox_n = Radius * math:cos(Angle_n)/?cell_side,
    case angle_quadrant(Angle_n) of
        0 ->
            Ox0 = erlang:trunc(Ox_n),
            lists:map(fun(Ox) -> {Ox,yp_point_on_ray(Ox, Angle_n)} end,lists:seq(0,X0,1));
        1 ->
            Ox1 = floor_neg(Ox_n),
            lists:map(fun(Ox) -> {Ox,yp_point_on_ray(Ox, Angle_n)} end,lists:seq(Ox1,-1,1));
        2 ->
            Ox2 = floor_neg(Ox_n),
            lists:map(fun(Ox) -> {Ox,yn_point_on_ray(Ox, Angle_n)} end,lists:seq(Ox2,-1,1));
        3 ->
            Ox3 = erlang:trunc(Ox_n),
            lists:map(fun(Ox) -> {Ox,yn_point_on_ray(Ox, Angle_n)} end,lists:seq(0,Ox3,1))
    end.

yp_point_on_arc(Ox, Radius)->
    erlang:trunc(math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side) .

yn_point_on_arc(Ox, Radius)->
    floor_neg(-math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side).

%% Angle_n_s, Angle_n_e are in the same quadrant.
%% Angle_n_s < Angle_n_e.
points_on_arc(Angle_n_s, Angle_n_e, Radius)->
    Ox_s_n = Radius * math:cos(Angle_s)/?cell_side,
    Ox_e_n = Radius * math:cos(Angle_e)/?cell_side,
    case angle_quadrant(Angle_n_s) of
        0 ->
            Ox_0_s = erlang:trunc(Ox_s_n),
            Ox_0_e = erlang:trunc(Ox_e_n),
            lists:map(fun(Ox) -> {Ox,yp_point_on_arc(Ox, Radius)} end,lists:seq(Ox_0_e,Ox_0_s,1));
        1 ->
            Ox_1_s = floor_neg(Ox_s_n),
            Ox_1_e = floor_neg(Ox_e_n),
            lists:map(fun(Ox) -> {Ox,yp_point_on_arc(Ox, Radius)} end,lists:seq(Ox_1_e,Ox_1_s,1));
        2 ->
            Ox_2_s = floor_neg(Ox_s_n),
            Ox_2_e = floor_neg(Ox_e_n),
            lists:map(fun(Ox) -> {Ox,yn_point_on_arc(Ox, Radius)} end,lists:seq(Ox_2_s,Ox_2_e,1));
        3 ->
            Ox_3_s = erlang:trunc(Ox_s_n),
            Ox_3_e = erlang:trunc(Ox_e_n),
            lists:map(fun(Ox) -> {Ox,yn_point_on_arc(Ox, Radius)} end,lists:seq(Ox_3_s,Ox_3_e,1))
    end.

%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n_s.
sector_partition({Q,Angle_n_s,0,Radius})->
    [];
sector_partition({Q,Angle_n_s,Angle_amplitude,Radius})->
    Angle_n_e = Angle_n_s + Angle_amplitude,
    case Q of
        0 ->
            if
                Angle_n_e =< ?half_pi -> [{0,Angle_n_s,Angle_n_e,Radius}];
		true -> [{0,Angle_n_s,Angle_n_e,Radius}] ++ sector_partition({Angle_n_s,Angle_amplitude,Radius})
            end;
        1 ->
            ;
        2 ->
            ;
        3 ->
            gohere
    end.

%% pre-requisite:
%%    All of Angle_s,Angle_e are in quadrant I, i.e.,
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
            y_point_on_ray(Ox,Angle_s,Radius),
            y_point_on_ray(Ox+1,Angle_e,Radius)}
        end,
        lists:seq(0,Ox_e-1,1))
    ++
    %% from Ox_e to Ox_s
    lists:map(
        fun(Ox)-> {Ox,
            y_point_on_ray(Ox,Angle_s,Radius),
            y_point_on_arc(Ox,Radius)}
        end,
        lists:seq(Ox_e,Ox_s,1)).

%% pre-requisite:
%%    All of Angle_s,Angle_e are in quadrant II, i.e.,
%%    math:pi()/2 =< Angle_s < Angle_e =< math:pi(), and Radius > 0.
%% Reture vales:
%%    in the grid, every cell is represented by its LB(Left Bottom corner point).
aoe_grids_quadrant2(Angle_s,Angle_e,Radius)->
   lists:map(fun({Ox,Oy1,Oy2})->{-Ox-1,Oy1,Oy2} end, aoe_grids_quadrant1(math:pi()-Angle_e,math:pi()-Angle_s,Radius)).

%% pre-requisite:
%%    All of Angle_s,Angle_e are in quadrant III, i.e.,
%%    math:pi() =< Angle_s < Angle_e =< math:pi()/2*3, and Radius > 0.
%% Reture vales:
%%    in the grid, every cell is represented by its LB(Left Bottom corner point).
aoe_grids_quadrant3(Angle_s,Angle_e,Radius)->
    lists:map(fun({Ox,Oy1,Oy2})->{-Ox-1,-Oy2-1,-Oy1-1} end, aoe_grids_quadrant1(Angle_s - math:pi(),Angle_e - math:pi(),Radius)).

%% pre-requisite:
%%    All of Angle_s,Angle_e are in quadrant IV, i.e.,
%%    math:pi()/2*3 =< Angle_s < Angle_e =< math:pi()*2, and Radius > 0.
%% Reture vales:
%%    in the grid, every cell is represented by its LB(Left Bottom corner point).
aoe_grids_quadrant4(Angle_s,Angle_e,Radius)->
    lists:map(fun({Ox,Oy1,Oy2})->{Ox,-Oy2-1,-Oy1-1} end, aoe_grids_quadrant1(2*math:pi()-Angle_e,2*math:pi()-Angle_s,Radius)).

aoe_grids_quadrant_test()->
    Radius = 400,

    Angle_s1 = math:pi()/6,
    Angle_e1 = math:pi()/3,
    Tuplelist_LB_Quadrant1 = aoe_grids_quadrant1(Angle_s1,Angle_e1,Radius),
    io:format("Tuplelist_LB_Quadrant1 = ~w~n",[Tuplelist_LB_Quadrant1]),

    io:format("1/2-1/6 = ~50.48f~n",[1/2-1/6]),
    io:format("       1/3 = ~50.48f~n",[1/3]),
    io:format("pi/2-pi/6 = ~50.48f~n",[math:pi()/2-math:pi()/6]),
    io:format("       pi/3 = ~50.48f~n",[math:pi()/3]),
    io:format("Tuplelist_LB_Quadrant* = ~w~n",[aoe_grids_quadrant1(math:pi()/6,math:pi()/3,Radius)]),
    io:format("Tuplelist_LB_Quadrant% = ~w~n",[aoe_grids_quadrant1(math:pi()*(1/6),math:pi()*(1/3),Radius)]),
    io:format("Tuplelist_LB_Quadrant^ = ~w~n",[aoe_grids_quadrant1(math:pi()/2-math:pi()/3,math:pi()/2-math:pi()/6,Radius)]),
    io:format("Tuplelist_LB_Quadrant! = ~w~n",[aoe_grids_quadrant1(math:pi()*(1/2-1/3),math:pi()*(1/2-1/6),Radius)]),

    Angle_s2 = Angle_s1 + math:pi()/2,
    Angle_e2 = Angle_e1 + math:pi()/2,
    Tuplelist_LB_Quadrant2 = aoe_grids_quadrant2(Angle_s2,Angle_e2,Radius),
    io:format("Tuplelist_LB_Quadrant2 = ~w~n",[Tuplelist_LB_Quadrant2]),

    Angle_s3 = Angle_s1 + math:pi(),
    Angle_e3 = Angle_e1 + math:pi(),
    Tuplelist_LB_Quadrant3 = aoe_grids_quadrant3(Angle_s3,Angle_e3,Radius),
    io:format("Tuplelist_LB_Quadrant3 = ~w~n",[Tuplelist_LB_Quadrant3]),

    Angle_s4 = Angle_s2 + math:pi(),
    Angle_e4 = Angle_e2 + math:pi(),
    Tuplelist_LB_Quadrant4 = aoe_grids_quadrant4(Angle_s4,Angle_e4,Radius),
    io:format("Tuplelist_LB_Quadrant4 = ~w~n",[Tuplelist_LB_Quadrant4]).

%% the output of the above test:
%% Tuplelist_LB_Quadrant1 = [{0,0,1},{1,0,3},{2,1,5},{3,1,6},{4,2,8},{5,2,8},{6,3,8},{7,4,7},{8,4,6}]
%% Tuplelist_LB_Quadrant2 = [{-1,0,1},{-2,0,3},{-3,1,5},{-4,1,6},{-5,2,9},{-6,2,8},{-7,3,8},{-8,4,7},{-9,4,6}]
%% Tuplelist_LB_Quadrant3 = [{-1,-2,-1},{-2,-4,-1},{-3,-6,-2},{-4,-7,-2},{-5,-9,-3},{-6,-9,-3},{-7,-9,-4},{-8,-8,-5},{-9,-7,-5}]
%% Tuplelist_LB_Quadrant4 = [{0,-2,-1},{1,-4,-1},{2,-6,-2},{3,-7,-2},{4,-10,-3},{5,-9,-3},{6,-9,-4},{7,-8,-5},{8,-7,-5}]
%%
%% the discrepency is caused by the float precision:
%% 1/2-1/6 = 0.333333333333333370341000000000000000000000000000
%%        1/3 = 0.333333333333333314830000000000000000000000000000
%% pi/2-pi/6 = 1.047197551196597853360000000000000000000000000000
%%        pi/3 = 1.047197551196597631320000000000000000000000000000

%% ====================TEST PART==================

test() ->
    aoe_grids_quadrant_test(),
    ok.

start() ->
    test().

%% ========================END=======================
