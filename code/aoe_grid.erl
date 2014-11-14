%% author: xzm
%% date: 2014-11-13

%% 
%% pre-assumption: 
%%        let cell be a square.
%% ?cell_side is the length of cell side, in pixel.
%%        the user should modify the macro according to the actual situation.
-define(cell_side, 10).

%%
%% Pre-assumption:
%%        if X, Y are 0 simultaneously, let radian(0,0) = 0.
radian(X,Y) ->
    RetVal =
    if
        X > 0 ->
            if
		Y == 0 ->
                    0;
		Y > 0 ->
                    math:atan2(Y, X);
		Y < 0 ->
                    2*math:pi() - math:atan2(-Y, X)
            end;
	X < 0 ->
            if
                Y == 0 ->
                    math:pi();
                Y > 0 ->
                    math:pi() - math:atan2(Y,-X);
		Y < 0 ->
                    math:pi() + math:atan2(-Y,-X)
            end;
        X ==0 ->
            if
		Y > 0 ->
		    math:pi()/2;
		 Y < 0 ->
                    3*math:pi()/2;
                Y == 0 ->
                    0
            end
    end,
    RetVal.

%% Pre-assumption: 
%%        if {X1,Y1} =:= {X0,Y0}, let RetVal = true.
%% Prerequisite:
%%        1.  {X0,Y0} is the coordinates of the attacker.
%%        2. Angle_amplitude =< 2*math:pi().
point_in_sector(X1,Y1,X0,Y0,Radius,Angle_s,Angle_amplitude) ->
    X = X1 - X0,
    Y = Y1 - Y0,
    Dist = math:sqrt(X*X+Y*Y),
    Rad = radian(X,Y),

    Angle_e = Angle_s + Angle_amplitude,
    Two_pi = 2 * math:pi(),

    RetVal = 
        if
            Dist > Radius -> false;
            Dist == 0 -> true; %% i.e., {X1,Y1} =:= {X0,Y0} -> true;
	    true      ->
                if
                    Angle_e =< Two_pi  ->
                        if
                            (Rad < Angle_s) or (Rad > Angle_e) -> false;
                            true -> true
                        end;
                    true ->
                        if
                            (Angle_s =< Rad) or (Rad+Two_pi =< Angle_e)  -> true;
			    true -> false
                        end
                end
        end,
    RetVal.

%% Parameters:
%%    1.  {X0,Y0} is the coordinates of the attacker.
%%    2. Angle_amplitude =< 2*math:pi().
%%    3. rect is represented by (X1,Y1,L_r,W_r), where (X1,Y1) is the left bottom corner with horizontal length L_r and vertical width W_r.
%%    4. L_r, and W_r must be non-negative.
rect_sector_intersect(X1,Y1,L_r,W_r,X0,Y0,Radius,Angle_s,Angle_amplitude) ->
    X2 = X1+L_r,
    Y2 = Y1+W_r,

    %% ---- corner points
    %% LB - {X1,Y1}
    point_in_sector(X1,Y1,X0,Y0,Radius,Angle_s,Angle_amplitude) orelse
    %% LT - {X1,Y2}
    point_in_sector(X1,Y2,X0,Y0,Radius,Angle_s,Angle_amplitude) orelse
    %% RB - {X2,Y1}
    point_in_sector(X2,Y1,X0,Y0,Radius,Angle_s,Angle_amplitude) orelse
    %% RT - {X2,Y2}
    point_in_sector(X2,Y2,X0,Y0,Radius,Angle_s,Angle_amplitude) orelse

    %% ---- vertical points
    %% L - {X1,Y0} and  R - {X2,Y0}
    (
        (Y1 < Y0 andalso Y0 < Y2)  andalso
        (point_in_sector(X1,Y0,X0,Y0,Radius,Angle_s,Angle_amplitude) orelse point_in_sector(X2,Y0,X0,Y0,Radius,Angle_s,Angle_amplitude) )
    ) orelse
    %% B - {X0,Y1} and T - {X0,Y2}
    (
        (X1 < X0 andalso X0 < X2)  andalso
        (point_in_sector(X0,Y1,X0,Y0,Radius,Angle_s,Angle_amplitude) orelse point_in_sector(X0,Y2,X0,Y0,Radius,Angle_s,Angle_amplitude) )
    ).

%% Pre-requisite:
%%    All of X, Angle, Radius appear in quadrant I.
point_on_line(X, Angle,Radius)->
    if
        Angle == math:pi()/2 -> erlang:trunc(Radius/?cell_side) ;
	true -> erlang:trunc(X * ?cell_side * math:tan(Angle)/?cell_side) 
    end.

%% Pre-requisite:
%%    All of X, Angle, Radius appear in quadrant I.
point_on_arc(X, Radius)->
    erlang:trunc(math:sqrt(Radius*Radius - X*X)/?cell_side) .

%% pre-requisite:
%%    All of Angle_s,Angle_e,Radius are in quadrant I, i.e.,
%%    0 =< Angle_s < Angle_e =< math:pi()/2, and Radius > 0.
%% Reture vales:
%%    every grid is represented by its LB(Left Bottom corner point).
aoe_grids_quadrant1(Angle_s,Angle_e,Radius)->
    List_angle = [Angle_s, Angle_e],
    [X_s,X_e] = lists:map(fun(X)->math:cos(X) end,List_angle),
    [Y_s,Y_e] = lists:map(fun(Y)->math:sin(Y) end,List_angle),
    [Ox_s, Ox_e,Oy_s,Oy_e] =lists:map(
        fun(Z) -> erlang:trunc(Z/?cell_side) end,
        [X_s, X_e,Y_s,Y_e]),

    %% from 0 to Ox_e to Ox_s
    lists:map(
        fun(X)-> {X,point_on_line(X,Angle_s,Radius),point_on_line(X,Angle_e,Radius)}end,
        lists:seq(0,Ox_e,1))
    ++
    lists:map(
        fun(X)-> {X,point_on_line(X,Angle_s,Radius),point_on_arc(X,Radius)}end,
        lists:seq(Ox_e+1,Ox_s,1)),

    todo.

%% ====================TEST PART==================

test() ->
    ok.

start() ->
    test().

%% ========================END=======================
