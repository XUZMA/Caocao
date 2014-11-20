%% author: xzm
%% date: 2014-11-19

%% assume:
%%    1.the attack region is a sector;
%%    2.cell be a square;
%% 
%% the modules output the cell LB points of the sector.
%% 

-module(aoe_grid_sector).
-export([start/0]).

%%    ================ predefined macros ================
%% ?cell_side is the length of cell side, in pixel.
%%        the user should modify the macro according to the actual situation.
-define(cell_side, 40).

-define(half_pi, (math:pi()/2)).
-define(pi, (math:pi())).
-define(half_3pi, (3*math:pi()/2)).
-define(two_pi, (2*math:pi())).

%%    ================ auxilliary functions ================

floor_neg(Neg)->
    Trunc = erlang:trunc(Neg),
    if
        Neg < Trunc -> Trunc -1;
	true -> Trunc
    end.

%% the normalized angle should fall into the interval [0, 2*math:pi()).
angle_normal(Angle)->
    Round = erlang:trunc(Angle /?two_pi),
    Angle_t = Angle - Round * ?two_pi,
    if
        Angle_t >=0 -> Angle_t;
        true -> Angle_t + ?two_pi
    end.

%% the quadrant ordinal should be any of 0,1,2,3.
angle_quadrant(Angle_n)->
    erlang:trunc(2 * Angle_n / ?pi).

%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n_s.
%%     Angle_amplitude =< ?two_pi.
%%     Angle_e = Angle_n_s + Angle_amplitude,
%%    notice: Angle_e may not be normal.
sector_quadrant({Q,Angle_n_s,Angle_e,Radius})->
    Half_pi = ?half_pi,
    Pi = ?pi,
    Half_3pi = ?half_3pi,
    Two_pi = ?two_pi,

    case Q of
        0 ->
            if
                Angle_e =< Half_pi -> 
                    [{0,Angle_n_s,Angle_e,Radius}];
		true -> 
                    [{0,Angle_n_s,Half_pi,Radius}] 
                    ++ 
                    sector_quadrant({1,Half_pi,Angle_e,Radius})
            end;
        1 ->
            if
                Angle_e =< Pi -> 
                    [{1,Angle_n_s,Angle_e,Radius}];
		true -> 
                    [{1,Angle_n_s,Pi,Radius}] 
                    ++ 
                    sector_quadrant({2,Pi,Angle_e,Radius})
            end;
        2 ->
            if
                Angle_e =< Half_3pi -> 
                    [{2,Angle_n_s,Angle_e,Radius}];
		true -> 
                    [{2,Angle_n_s,Half_3pi,Radius}] 
                    ++ 
                    sector_quadrant({3,Half_3pi,Angle_e,Radius})
            end;
        3 ->
            if
                Angle_e =< Two_pi -> 
                    [{3,Angle_n_s,Angle_e,Radius}];
		true -> 
                    [{3,Angle_n_s,Two_pi,Radius}] 
                    ++ 
                    sector_quadrant({0,0,Angle_e-Two_pi,Radius})
            end
    end.

%%    ================ cell (representation) points on rays and arcs ================

yp_point_on_ray(Ox, Angle_n,Radius) ->
    Half_pi = math:pi()/2,
    if
        Angle_n == Half_pi ->
            erlang:trunc(Radius/?cell_side) ;
        true -> 
            erlang:trunc(Ox * math:tan(Angle_n)) 
    end.

yn_point_on_ray(Ox, Angle_n,Radius) ->
    Half_3pi = 3*math:pi()/2,
    if
        Angle_n == Half_3pi ->
            floor_neg(-Radius/?cell_side) ;
        true -> 
            floor_neg(Ox * math:tan(Angle_n))
    end.

%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n,i.e.,
%%    Q = angle_quadrant(Angle_n);
%% Ox_n is the X cell ordinal real for Angle_n, i.e.,
%%    Ox_n = Radius * math:cos(Angle_n)/?cell_side.
points_on_ray(Q, Ox_n, Angle_n,Radius)->
    case Q of
        0 ->
            Ox0 = erlang:trunc(Ox_n),
            lists:map(fun(Ox) -> {Ox,yp_point_on_ray(Ox, Angle_n,Radius)} end,lists:seq(0,Ox0,1));
        1 ->
            Ox1 = floor_neg(Ox_n),
            lists:map(fun(Ox) -> {Ox,yp_point_on_ray(Ox, Angle_n,Radius)} end,lists:seq(Ox1,-1,1));
        2 ->
            Ox2 = floor_neg(Ox_n),
            lists:map(fun(Ox) -> {Ox,yn_point_on_ray(Ox, Angle_n,Radius)} end,lists:seq(Ox2,-1,1));
        3 ->
            Ox3 = erlang:trunc(Ox_n),
            lists:map(fun(Ox) -> {Ox,yn_point_on_ray(Ox, Angle_n,Radius)} end,lists:seq(0,Ox3,1))
    end.

yp_point_on_arc(Ox, Radius)->
    erlang:trunc(math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side) .

yn_point_on_arc(Ox, Radius)->
    floor_neg(-math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side).

%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n_s.
%% Angle_n_s, Angle_n_e are in the same quadrant.
%% Angle_n_s < Angle_n_e.
%% Ox_n_s, Ox_n_e are the X cell ordinal reals for Angle_n_s < Angle_n_e respectively, i.e.,
%%    Ox_n_s = Radius * math:cos(Angle_s)/?cell_side,
%%    Ox_n_e = Radius * math:cos(Angle_e)/?cell_side,
points_on_arc(Q, Ox_n_s, Ox_n_e, Radius)->
    case Q of
        0 ->
            Ox_s_0 = erlang:trunc(Ox_n_s),
            Ox_e_0 = erlang:trunc(Ox_n_e),
            lists:map(fun(Ox) -> {Ox,yp_point_on_arc(Ox, Radius)} end,lists:seq(Ox_e_0,Ox_s_0,1));
        1 ->
            Ox_s_1 = floor_neg(Ox_n_s),
            Ox_e_1 = floor_neg(Ox_n_e),
            lists:map(fun(Ox) -> {Ox,yp_point_on_arc(Ox, Radius)} end,lists:seq(Ox_e_1,Ox_s_1,1));
        2 ->
            Ox_s_2 = floor_neg(Ox_n_s),
            Ox_e_2 = floor_neg(Ox_n_e),
            lists:map(fun(Ox) -> {Ox,yn_point_on_arc(Ox, Radius)} end,lists:seq(Ox_s_2,Ox_e_2,1));
        3 ->
            Ox_s_3 = erlang:trunc(Ox_n_s),
            Ox_e_3 = erlang:trunc(Ox_n_e),
            lists:map(fun(Ox) -> {Ox,yn_point_on_arc(Ox, Radius)} end,lists:seq(Ox_s_3,Ox_e_3,1))
    end.


%%    ================ grid points list ================

%% converge the cell reprentative points in the arc in a quadrant
zip_pointlist(Point_list_lower, Point_list_upper) ->
    lists:append(lists:zipwith(fun({Ox,Y_l},{Ox,Y_u})->lists:map(fun(Y)->{Ox,Y} end,lists:seq(Y_l,Y_u,1))end, Point_list_lower, Point_list_upper)).

%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n_s.
%% Angle_n_s, Angle_n_e are in the same quadrant.
%% Angle_n_s < Angle_n_e.
%% Ox_n_s, Ox_n_e are the X cell ordinal reals for Angle_n_s < Angle_n_e respectively, i.e.,
%%    Ox_n_s = Radius * math:cos(Angle_s)/?cell_side,
%%    Ox_n_e = Radius * math:cos(Angle_e)/?cell_side,
points_in_region_quadrant(Q, Ox_n_s, Ox_n_e, Angle_n_s, Angle_n_e, Radius)->
    Point_list_ray_s = points_on_ray(Q, Ox_n_s, Angle_n_s,Radius),
    Point_list_ray_e = points_on_ray(Q, Ox_n_e, Angle_n_e,Radius),
    Point_list_arc = points_on_arc(Q, Ox_n_s, Ox_n_e, Radius),

    case Q of
        0 ->
            zip_pointlist(Point_list_ray_s,lists:umerge(Point_list_arc,Point_list_ray_e));
        1 ->
            zip_pointlist(Point_list_ray_e,lists:umerge(Point_list_arc,Point_list_ray_s));
        2 ->
            zip_pointlist(lists:umerge(Point_list_arc,Point_list_ray_e),Point_list_ray_s);
        3 ->
            zip_pointlist(lists:umerge(Point_list_arc,Point_list_ray_s),Point_list_ray_e)
    end.

%%     Angle_amplitude =< ?two_pi.
cell_LBs_sector(Angle_s, Angle_amplitude, Radius)->
    Two_pi = 2*math:pi(),
    Angle_n_s =
        if
            Angle_amplitude == Two_pi -> 0;
	    true -> angle_normal(Angle_s)
        end,
    Angle_e = Angle_n_s + Angle_amplitude,
    Q = angle_quadrant(Angle_n_s),
    lists:append(
        lists:map(
            fun({Q_,A_n_s,A_n_e,Radius_}) ->
                Ox_n_s = Radius_ * math:cos(A_n_s)/?cell_side,
                Ox_n_e = Radius_ * math:cos(A_n_e)/?cell_side,
                points_in_region_quadrant(Q_, Ox_n_s, Ox_n_e, A_n_s, A_n_e, Radius_)
            end,
            sector_quadrant({Q,Angle_n_s,Angle_e,Radius}))).

%% ================================================

cell_LBs_sector_test()->
    Radius = 400,

    As1 = math:pi()/3,    %% As, Angle_s
    AA1 = math:pi()/6,   %% AA, Angle_amplitude
    TL1 = cell_LBs_sector(Angle_s, Angle_amplitude, Radius), %% TL, Tuple List of points
    io:format("TL = ~p~n",[TL1]),

%%    Angle_amplitude = math:pi()/3,
%%    Angle_amplitude = 2*math:pi(),
	ok.

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
    cell_LBs_sector_test(),
    ok.

start() ->
    test().

%% ========================END=======================
