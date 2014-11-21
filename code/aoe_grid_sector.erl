%% author: xzm
%% date: 2014-11-19/20/21

%% Algorithm description:
%% assume the attack region is a sector, and every cell is a square;
%% let the LB(left bottom) point be the representive of the grid cell respectively,
%% the aoe is the set of the LBs.
%% partition the X-axis in a series of holes according to the cell side size.
%% for ervery hole on the x-axis, find the aoe range in the Y direction.
%% process these problems for each quadrant.

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

is_integer(X) ->
    T = erlang:trunc(X),
    if
        T == X -> true;
	true -> false
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

%%    ================ cell (representative) points around rays and arcs ================
yp_zenith(Radius) ->
    erlang:trunc(Radius / ?cell_side) .

yn_zenith(Radius) ->
    floor_neg(-Radius / ?cell_side) .

%% Angle_n /= math:pi()/2 or 3*math:pi()/2.
yp_endpoint(Angle_n,Radius) ->
    erlang:trunc(Radius * math:tan(Angle_n) / ?cell_side) .

%% Angle_n /= math:pi()/2 or 3*math:pi()/2.
yn_endpoint(Angle_n,Radius) ->
    floor_neg(Radius * math:tan(Angle_n) / ?cell_side).

%% Angle_n /= math:pi()/2 or 3*math:pi()/2.
%% Ox /= 0.
yp_steppoint_on_ray(Ox, Angle_n) ->
    erlang:trunc(Ox * math:tan(Angle_n)) .

%% Angle_n /= math:pi()/2 or 3*math:pi()/2.
%% Ox /= 0.
yn_steppoint_on_ray(Ox, Angle_n) ->
    floor_neg(Ox * math:tan(Angle_n)).

yp_steppoint_on_arc(Ox, Radius)->
    erlang:trunc(math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side) .

yn_steppoint_on_arc(Ox, Radius)->
    floor_neg(-math:sqrt(Radius*Radius - Ox*Ox*?cell_side*?cell_side)/?cell_side).

%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n,i.e.,
%%    Q = angle_quadrant(Angle_n);
%% X_n is the X coordinate for the endpoint, i.e.,
%%    X_n = Radius * math:cos(Angle_n);
%% Dx_n is the Division of X_n by ?cell_side, i.e.,
%%    Dx_n = X_n/?cell_side;
%% Tx_n is the Trunc of Dx_n, i.e.,
%%    Tx_n = erlang:trunc(Dx_n);
stepboxes_around_ray(Q, Tx_n, Dx_n, X_n, Angle_n, Radius)->
    Half_pi = ?half_pi,
    Half_3pi = ?half_3pi,

    if
        X_n ==0 ->
            case Angle_n of
                Half_pi ->
                    case Q of
                        0 -> [{0,0,yp_zenith(Radius)}];
        		1 -> [{-1,0,yp_zenith(Radius)}]
                    end;
                Half_3pi ->
                    case Q of
                        2 -> [{-1,yn_zenith(Radius),-1}];
        		3 -> [{0,yn_zenith(Radius),-1}]
                    end
            end;
        Tx_n == 0 ->
            case Q of
                0 -> [{0,0,yp_endpoint(Angle_n,Radius) }];
		1 -> [{-1,0,yp_endpoint(Angle_n,Radius) }];
		2 -> [{-1,yn_endpoint(Angle_n,Radius) ,-1}];
		3 -> {0,yn_endpoint(Angle_n,Radius),-1}		    
            end;
        Tx_n == Dx_n ->
            case Q of
                0 -> 
                    L0 = [{0,0}]++lists:map(fun(Ox) -> {Ox,yp_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(1,Tx_n,1)),
                    L1 = lists:sublist(L0,1,Tx_n),
                    L2 = lists:sublist(L0,2,Tx_n),
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,L1,L2);
		1 -> 
                    L0 = lists:map(fun(Ox) -> {Ox,yp_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(Tx_n,-1,1))++[{-1,0}],
                    L1 = lists:sublist(L0,1,-Tx_n),
                    L2 = lists:sublist(L0,2,-Tx_n),
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,L2,L1);
		2 -> 
                    L0 = lists:map(fun(Ox) -> {Ox,yn_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(Tx_n,-1,1))++[{-1,-1}],
                    L1 = lists:sublist(L0,1,-Tx_n),
                    L2 = lists:sublist(L0,2,-Tx_n),
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,L1,L2);
		3 -> 
                    L0 = [{0,-1}]++lists:map(fun(Ox) -> {Ox,yn_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(1,Tx_n,1)),
                    L1 = lists:sublist(L0,1,Tx_n),
                    L2 = lists:sublist(L0,2,Tx_n),
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,L2,L1)
            end;
        true ->
%% ??? it's taken for granted to taken some outer x into the range.
%% CHECK!!!
            case Q of
                0 ->
                    Lst1 = [{0,0}]++lists:map(fun(Ox) -> {Ox,yp_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(1,Tx_n,1)),
                    Lst2 = lists:sublist(Lst1,2,Tx_n) ++[{Tx_n, yp_endpoint(Angle_n,Radius)}],
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,Lst1,Lst2);
		1 -> 
                    Lst1 = lists:map(fun(Ox) -> {Ox,yp_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(Tx_n,-2,1))++[{-1,0}],
                    Lst2 = [{Tx_n, yp_endpoint(Angle_n,Radius)}]++lists:sublist(Lst1,1,-Tx_n-1),
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,Lst1,Lst2);
		2 -> 
                    Lst1 = lists:map(fun(Ox) -> {Ox,yn_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(Tx_n,-2,1))++[{-1,-1}],
                    Lst2 = [{Tx_n, yn_endpoint(Angle_n,Radius)}]++lists:sublist(Lst1,1,-Tx_n-1),
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,Lst2,Lst1);
		3 -> 
                    Lst1 = [{0,-1}]++lists:map(fun(Ox) -> {Ox,yn_steppoint_on_ray(Ox, Angle_n)} end,lists:seq(1,Tx_n,1)),
                    Lst2 = lists:sublist(Lst1,2,Tx_n)++[{Tx_n, yn_endpoint(Angle_n,Radius)}],
                    lists:zipwith(fun({Ox,Yp1},{_,Yp2})->{Ox,Yp1,Yp2}end,Lst2,Lst1)
            end
    end.

%% Angle_n_s, Angle_n_e are in the same quadrant.
%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n,i.e.,
%%    Q = angle_quadrant(Angle_n_s);
%% X_s, X_e are the X coordinates for the endpoint, i.e.,
%%    X_s = Radius * math:cos(Angle_n_s),
%%    X_e = Radius * math:cos(Angle_n_e);
%% Dx_s, Dx_e are the Divisions of X_s, X_e by ?cell_side, i.e.,
%%    Dx_s = X_s/?cell_side,
%%    Dx_e = X_e/?cell_side;
%% Tx_s, Tx_e are the Truncs of Dx_s, Dx_e, i.e.,
%%    Tx_s = erlang:trunc(Dx_s),
%%    Tx_e = erlang:trunc(Dx_e);
stepboxes_around_arc(Q,  Tx_s, Tx_e, Dx_s, Dx_e, X_s, X_e, Angle_n_s, Angle_n_e, Radius)->
    if
        Tx_s == Tx_e ->
            case Q of
                0 -> [{Tx_s, yp_endpoint(Angle_n_s,Radius), yp_endpoint(Angle_n_e,Radius)}];
		1 -> [{Tx_s - 1, yp_endpoint(Angle_n_e,Radius), yp_endpoint(Angle_n_s,Radius)}];
		2 -> [{Tx_s - 1, yn_endpoint(Angle_n_e,Radius), yn_endpoint(Angle_n_s,Radius)}];
		3 -> [{Tx_s, yn_endpoint(Angle_n_s,Radius), yn_endpoint(Angle_n_e,Radius)}]
            end;
        true ->
                case Q of
                    0 ->
                        {Ret_l,Left_end} =
                            if
                                Tx_e == Dx_e -> {[],Tx_e};
                                true ->{[{Tx_e, yp_steppoint_on_arc(Tx_e + 1, Radius), yp_endpoint(Angle_n_e,Radius)}],Tx_e+1}
                            end,
                        {Ret_r,Right_end} =
                            if
                                Tx_s == Dx_s -> {[],Tx_s};
        		        true -> {[{Tx_s, yp_endpoint(Angle_n_s,Radius), yp_steppoint_on_arc(Tx_s, Radius)}],Tx_s}
                            end;
		    1 ->
                        {Ret_l,Left_end} =
                            if
                                Tx_e == Dx_e -> {[],Tx_e};
                                true ->{[{Tx_e - 1, yp_endpoint(Angle_n_e,Radius), yp_steppoint_on_arc(Tx_e, Radius)}],Tx_e}
                            end,
                        {Ret_r,Right_end} =
                            if
                                Tx_s == Dx_s -> {[],Tx_s};
        		        true -> {[{Tx_s - 1, yp_steppoint_on_arc(Tx_s-1, Radius), yp_endpoint(Angle_n_s,Radius)}],Tx_s-1}
                            end;
		    2 ->
                        {Ret_l,Left_end} =
                            if
                                Tx_s == Dx_s -> {[],Tx_s};
                                true ->{[{Tx_s - 1, yn_steppoint_on_arc(Tx_s, Radius), yn_endpoint(Angle_n_s,Radius)}],Tx_s}
                            end,
                        {Ret_r, Right_end} =
                            if
                                Tx_e == Dx_e -> {[],Tx_e};
        		        true -> {[{Tx_e - 1, yn_steppoint_on_arc(Tx_e - 1, Radius), yn_endpoint(Angle_n_e,Radius)}],Tx_e-1}
                            end;
		    3 ->
                        {Ret_l,Left_end} =
                            if
                                Tx_s == Dx_s -> [];
                                true ->[{Tx_s, yn_endpoint(Angle_n_s,Radius), yn_steppoint_on_arc(Tx_s+1, Radius)}]
                            end,
                        {Ret_r, Right_end} =
                            if
                                Tx_e == Dx_e -> {[],Tx_e};
        		        true -> {[{Tx_e, yn_steppoint_on_arc(Tx_e, Radius), yn_endpoint(Angle_n_e,Radius)}],Tx_e}
                            end,
                end,

                Ret_m =
                    if
                        Left_end == Right_end -> [];
			true ->
                            Span = Right_end - Left_end,
                            Lst0 = lists:seq(Left_end,Right_end-1,1),
                            Lst1 = lists:map(fun(Ox)->yp_steppoint_on_arc(Ox, Radius) end,lists:seq(Left_end,Right_end,1)),
                            case Q of
                                0 ->
                                    Lst2 = lists:sublist(Lst1,1,Span),
                                    Lst3 = lists:sublist(Lst1,2,Span),
				1 ->[todo-xzm-gohere];
				2 ->[todo-xzm-gohere];
				3 ->[todo-xzm-gohere]
                            end
                    end,

                Ret_l ++ Ret_m ++ Ret_r
    end.

%% Fx_s, Fx_e are the floors of Dx_s, Dx_e, i.e.,
%%    Fx_s = if end,
, Tx_s, Tx_e, Dx_s, Dx_e
%% NOTICE: at the joining point, the stepbox for ray may not be indentical to that for arc.
umerge_stepboxes_around_arc_ray(Q, Fx_s, Fx_e, Tx_s, Tx_e, Dx_s, Dx_e, X_s, X_e, Angle_n_s, Angle_n_e, Radius)->
    [].

%%    ================ grid points list ================

%% converge the cell reprentative points in the arc in a quadrant
%% zip_pointlist(Point_list_lower, Point_list_upper) ->
%%
%%     io:format("test - Point_list_lower = ~p~n",[Point_list_lower]),
%%     io:format("test - Point_list_upper = ~p~n",[Point_list_upper]),
%%
%%     lists:append(lists:zipwith(fun({Ox,Y_l},{Ox,Y_u})->lists:map(fun(Y)->{Ox,Y} end,lists:seq(Y_l,Y_u,1))end, Point_list_lower, Point_list_upper)).




%% Q = 0,1,2,3 is the quadrant ordinal of Angle_n_s.
%% Angle_n_s, Angle_n_e are in the same quadrant.
%% Angle_n_s < Angle_n_e.
%% Ox_n_s, Ox_n_e are the X cell ordinal reals for Angle_n_s < Angle_n_e respectively, i.e.,
%%    Ox_n_s = Radius * math:cos(Angle_s)/?cell_side,
%%    Ox_n_e = Radius * math:cos(Angle_e)/?cell_side,
%%
%%
%% points_in_region_quadrant(Q, Ox_n_s, Ox_n_e, Angle_n_s, Angle_n_e, Radius)->
%%     Point_list_ray_s = points_on_ray(Q, Ox_n_s, Angle_n_s,Radius),
%%     Point_list_ray_e = points_on_ray(Q, Ox_n_e, Angle_n_e,Radius),
%%     Point_list_arc = points_on_arc(Q, Ox_n_s, Ox_n_e, Radius),
%%
%%     io:format("test - Point_list_ray_s = ~p~n",[Point_list_ray_s]),
%%     io:format("test - Point_list_ray_e = ~p~n",[Point_list_ray_e]),
%%     io:format("test - Point_list_arc = ~p~n",[Point_list_arc]),
%%
%%     case Q of
%%         0 ->
%%             zip_pointlist(Point_list_ray_s,lists:umerge(Point_list_arc,Point_list_ray_e));
%%         1 ->
%%             zip_pointlist(Point_list_ray_e,lists:umerge(Point_list_arc,Point_list_ray_s));
%%         2 ->
%%             zip_pointlist(lists:umerge(Point_list_arc,Point_list_ray_e),Point_list_ray_s);
%%         3 ->
%%             zip_pointlist(lists:umerge(Point_list_arc,Point_list_ray_s),Point_list_ray_e)
%%     end.

%%     Angle_amplitude =< ?two_pi.
%%
%% cell_LBs_sector(Angle_s, Angle_amplitude, Radius)->
%%     Two_pi = 2*math:pi(),
%%     Angle_n_s =
%%         if
%%             Angle_amplitude == Two_pi -> 0;
%% 	    true -> angle_normal(Angle_s)
%%         end,
%%     Angle_e = Angle_n_s + Angle_amplitude,
%%     Q = angle_quadrant(Angle_n_s),
%%     lists:append(
%%         lists:map(
%%             fun({Q_,A_n_s,A_n_e,Radius_}) ->
%%                 Ox_n_s = Radius_ * math:cos(A_n_s)/?cell_side,
%%                 Ox_n_e = Radius_ * math:cos(A_n_e)/?cell_side,
%%                 points_in_region_quadrant(Q_, Ox_n_s, Ox_n_e, A_n_s, A_n_e, Radius_)
%%             end,
%%             sector_quadrant({Q,Angle_n_s,Angle_e,Radius}))).

%% ================================================

stepboxes_around_ray_test()->
    ok.

stepboxes_around_arc()->
    ok.

umerge_stepboxes_around_arc_ray_test()->
    ok.

cell_LBs_sector_test()->
    %% Radius = 400,
    %% AA = math:pi()/6,   %% AA, Angle_amplitude
%%
    %% As1 = 0,
    %% TL1 = cell_LBs_sector(As1, AA, Radius), %% TL, Tuple List of points
    %% io:format("TL1 = ~p~n",[TL1]),
%%
%%    As2 = math:pi()/3,    %% As, Angle_s
%%    TL2 = cell_LBs_sector(As1, AA, Radius),
%%    io:format("TL2 = ~p~n",[TL2]),

    ok.

%% ====================TEST PART==================

test() ->
    stepboxes_around_ray_test(),
    stepboxes_around_arc(),
    umerge_stepboxes_around_arc_ray_test(),
    cell_LBs_sector_test(),

    ok.

start() ->
    test().

%% ========================END=======================
