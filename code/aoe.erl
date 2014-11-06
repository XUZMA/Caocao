%% author: xzm
%% date: 2014-11-05


%% ------------------------------------------------************************************************------------------------------------------------

%% in game terminology, 
%% aoe is the  abbreviation of "area of effect",
%% and aoi is the that of "area of interest".
%% aoe can be treated as a special case of aoi.
%% generally speaking, aoi is a special case of collision detection in computational graphics.
%% this module gives one algotihm to determine aoe and aoi.

%% algorithmic cue:
%%
%% 1. treat the display screen as an infinite Cartesian plane.
%%     (1) the display screen is a 2d infinite plane, denoted as plane_2d;
%%     (2) establish a Cartesian coordinates system(X-Y axes) to represent the display screen;
%%     (2) the center of display screen has coordinates (0,0);
%%     (3) the axes are parallel to the frame borders of the physical display screen respectively.
%%
%% 2. strictly partition the plane into a series of cells.
%%     (1) the plane is partitioned into square cells according to screen resolution and pixel size;
%%     (2) roughly speaking, each cell is surrounded by four edges: 
%%                edge_l(left-side edge), 
%%                edge_r(right-side edge), 
%%                edge_b(bottom edge),
%%                edge_t(top edge);
%%     (3) the above four edges intersect at four vertexes:
%%                vertex_lb(cross point of edge_l and edge_b), 
%%                vertex_lt(cross point of edge_l and edge_t), 
%%                vertex_rb(cross point of edge_r and edge_b), 
%%                vertex_rt(cross point of edge_r and edge_t);
%%     (4) to guarantee a strictly mathematical partition, 
%%                only vertex_lb, edge_l without vertex_lt, and edge_b without vertex_rb are included in the cell;
%%                while vertex_lt, vertex_rb, vertex_rt, edge_r, edge_t are not included in the cell;
%%     (5) point(x_lb, y_lb) can be the representive of the cell whose vertex_lb coordinates is (x_lb, y_lb);
%%                hence, the cell is represented as cell(x_lb, y_lb).
%%     (6) define the cell's dimension sizes as cl(the cell length in X-axis direction), cw(the cell width in Y-axis direction);
%%     (7) Now, we get a partition of the 2d plane as follows:
%%                plane_2d = {cell(m*cl, n*cw) | m, n are two arbitrary integer}.
%%
%% 3. refine cell resolution.
%%     (1) to improve the resolution of cell, partition the above 2d infinite plane with refined cells
%%        whose dimension sizes as rcl=cl/2, and rcw=cw/2.
%%     (2) denote the refined cell whose vertex_lb is point(x_lb, y_lb) as rcell(x_lb, y_lb).
%%     (3) now we have:
%%                plane_2d = {rcell(m*rcl, n*rcw) | m, n are two arbitrary integer},
%%     (4) partition a cell into 4 refined cells. for arbitrary integers m, n, 
%%                cell(m*cl, n*cw) = cell_lb(m*cl, n*cw) +cell_lt(m*cl, n*cw) +cell_rb(m*cl, n*cw) +cell_rt(m*cl, n*cw) ,
%%        where
%%                cell_lb(m*cl, n*cw)  = rcell((2m)*rcl, (2n)*rcw) ,
%%                cell_lt(m*cl, n*cw)   = rcell((2m)*rcl, (2n + 1)*rcw) ,
%%                cell_rb(m*cl, n*cw)  = rcell((2m+1)*rcl, (2n)*rcw) ,
%%                cell_rt(m*cl, n*cw)   = rcell((2m+1)*rcl, (2n+1)*rcw) .
%%     (5) furtherly, define
%%                rrcl=rcl/2, and rrcw=rcw/2;
%%                plane_2d = {rrcell(m*rrcl, n*rrcw) | m, n are two arbitrary integer},
%%                and for any arbitrary integers m,n, 
%%                rcell(m*rcl, n*rcw) = rcell_lb(m*rcl, n*rcw) +rcell_lt(m*rcl, n*rcw) +rcell_rb(m*rcl, n*rcw) +rcell_rt(m*rcl, n*rcw) ,
%%                where
%%                    rcell_lb(m*rcl, n*rcw) = rrcell(2m*rrcl, 2n*rrcw) ,
%%                    rcell_lt(m*rcl, n*rcw)  = rrcell(2m*rrcl, (2n+1)*rrcw) ,
%%                    rcell_rb(m*rcl, n*rcw) = rrcell((2m+1)*rrcl, 2n*rrcw) ,
%%                    rcell_rt(m*rcl, n*rcw)  = rrcell((2m+1)*rrcl, (2n+1)*rrcw) .
%%     (6) the refinement can proceed to pixel level.
%%
%% 4. define the center of cell and refined cell.
%%     (1) for cell(x_lb, y_lb), define
%%                center(cell(x_lb, y_lb)) = point(x_lb + rcl, y_lb + rcw);
%%     (2) for rcell(x_lb, y_lb), define
%%                center(rcell(x_lb, y_lb)) = point(x_lb + rcl/2, y_lb + rcw/2);
%%     (3) for rrcell(x_lb, y_lb), define
%%                center(rrcell(x_lb, y_lb)) = point(x_lb + rrcl/2, y_lb + rrcw/2).
%%
%% 5. define the affiliated cell, vertexes, centers for a plane point,  i.e. the player's position.
%%     (1) suppose the player is place at point(xp, yp), denoted as pos(player) = point(xp,yp);
%%     (2) calculate the affiliated vertexes gradually as follows:
%%                 (2.1)    let x = floor(xp/cl)*cl,     y = floor(yp/cw)*cw
%%                                NOTICE: beware of the cases when xp==floor(xp/cl)*cl, or yp == floor(yp/cw)*cw.
%%                 (2.2)    dimension: (cl, cw),
%%                                cell vertexes:  
%%                                    {(x,y )}
%%                                center:        (x+rcl, y+rcw)
%%                 (2.3)    dimension: (rcl, rcw),
%%                                cell vertexes: 
%%                                   {(x,y ),    (x+rcl, y), 
%%                                    (x, y+rcw),    (x+rcl, y+rcw)}
%%                 (2.4)    dimension: (rrcl,rrcw),
%%                                cell vertexes: 
%%                                   {(x, y), (x+rrcl, y), (x+rcl, y), (x+3*rrcl, y);
%%                                    (x, y+rrcw), (x+rrcl, y+rrcw), (x+rcl, y+rrcw), (x+3*rrcl, y+rrcw);
%%                                    (x, y+rcw), (x+rrcl, y+rcw), (x+rcl, y+rcw), (x+3*rrcl, y+rcw);
%%                                    (x, y+3*rrcw), (x+rrcl, y+3*rrcw), (x+rcl, y+3*rrcw), (x+3*rrcl, y+3*rrcw)}.
%%     (3) obviously, further refinement from level n to level n+1,
%%                cell vertexes at level n+1 =
%%                        cell vertexes at level n                                    +
%%                        centers of cells at level n                                +
%%                        center of horizontal side of cells at level n     +
%%                        center of vertical side of cells at level n.
%%
%% 6. refinement is down-sampling(shrink).  neighboring is up-sampling(expansion).
%%     compliant with the notations in 5.(1) and 5.(2.1),
%%     player in cell(x,y),
%%     the neighboring cells constitue of the following 9 cells, including cell(x, y) itself:
%%
%%                neighbor_region(x,y)
%%                        =
%%                                cell(x-cl,y+cw)     + cell(x,y+cw)      + cell(x+cl,y+cw)       +
%%                                cell(x-cl,y)           + cell(x,y)            + cell(x+cl,y)              +
%%                                cell(x-cl,y-cw)      + cell(x,y-cw)      + cell(x+cl,y-cw).
%%
%% 7. define the attack field.
%%    when a player or the antagonist release an attack, the attack takes effect only in a special field,
%%    which is a subset of the 2d plane.
%%    we call it the attack field, and denote it as attack_field.
%%    different points in attack_field may have different effect tension.
%%    for example,
%%                just as shown in Newton's law of universal gravitation,
%%                F = G * m_1 * m_2 / r^2.
%%
%% 8. coefficient between attack_field and neighbor_region(x,y).
%%     (1)  Expansion: get neighbor_region(x,y);
%%     (2)  Refinement: get the 9*16(=12^2) cell vertexes in neighbor_region(x,y) to dimenison(rrcl, rrcw);
%%     (3)  calculate the coefficient between attack_field and neighbor_region(x,y),
%%                coeff = kronecker_delta(P, attack_field)  * weight(P),
%%                where 
%%                    kronecker_delta(P, attack_field)  takes 1 if P is an element of attack_field, else 0;
%%                    weight(P) can be set by various policy.
%%
%% 9. a policy for calculating weight(P).
%%
%%    positions of the 9*16(=12^2) cell vertexes arelisted as follows:
%%
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%                .    .    .    .    .    .    .    .    .    .    .    .
%%
%%     use a policy(called as  cicle policy) to group the vertexes, illustrated in the following figure:
%%
%%                6   6   6    6    6    6    6    6    6    6    6    6
%%                6   5   5    5    5    5    5    5    5    5    5    6
%%                6   5   4    4    4    4    4    4    4    4    5    6
%%                6   5   4    3    3    3    3    3    3    4    5    6
%%                6   5   4    3    2    2    2    2    3    4    5    6
%%                6   5   4    3    2    1    1    2    3    4    5    6
%%                6   5   4    3    2    1    1    2    3    4    5    6
%%                6   5   4    3    2    2    2    2    3    4    5    6
%%                6   5   4    3    3    3    3    3    3    4    5    6
%%                6   5   4    4    4    4    4    4    4    4    5    6
%%                6   5   5    5    5    5    5    5    5    5    5    6
%%                6   6   6    6    6    6    6    6    6    6    6    6
%%
%%    in circle 1, there are 4*1 vertexes, the side length is 2;
%%    in circle 2, there are 2*4+4 = 4*3 vertexes, the side length is 2+2=4;
%%    in circle 3, there are 4*4+4 = 4*5 vertexes, the side length is 4+2=6;
%%    in circle 4, there are 6*4+4 = 4*7 vertexes, the side length is 6+2=8;
%%    in circle 5, there are 8*4+4 = 4*9 vertexes, the side length is 8+2=10;
%%    in circle 6, there are 10*4+4 = 4*11 vertexes, the side length is 10+2=12,
%%    i.e.,
%%    in circle n, there are  4*(2*n - 1) vertexes, the side length is 2 * n.
%%
%%    let
%%                weight(P) = 1/(4*11*6)    = wt_6, if P is a vertex in circle 6;
%%                weight(P) = 1/(4*9*6)      = wt_5, if P is a vertex in circle 5;
%%                weight(P) = 1/(4*7*6)      = wt_4, if P is a vertex in circle 4;
%%                weight(P) = 1/(4*5*6)      = wt_3, if P is a vertex in circle 3;
%%                weight(P) = 1/(4*3*6)      = wt_2, if P is a vertex in circle 2;
%%                weight(P) = 1/(4*1*6)      = wt_1, if P is a vertex in circle 1.
%%    i.e.
%%                                               wt_k  = 1/24  * 1/(2*k - 1),     for k = 1,2,3,4,5,6.
%%
%% 10. define the strategies for determing position collision index and influence effect index.
%%        using the above ring weight policy, in the vertex matrix, calculate weight sum falling in the diagonal trigonums gradually.
%%
%%                6
%%                6   5
%%                6   5   4
%%                6   5   4    3
%%                6   5   4    3    2
%%                6   5   4    3    2    1
%%                6   5   4    3    2    1    1
%%                6   5   4    3    2    2    2    2
%%                6   5   4    3    3    3    3    3    3
%%                6   5   4    4    4    4    4    4    4    4
%%                6   5   5    5    5    5    5    5    5    5    5
%%                6   6   6    6    6    6    6    6    6    6    6    6
%%                |                                                        |    |
%%                |<-11th diagonal trigonum: trig11 ->|     the 12th diagonal line: diag12
%%                |                                                        the 11th diagonal line: diag11
%%                |
%%                |
%%                the 1st diagonal line: diag1
%%
%%                                               
%%        wt_k  = 1/24  * 1/(2*k - 1),     for k = 1,2,3,4,5,6.
%%
%%        wtd_01 = weight(diag01) = wt_6;
%%        wtd_02 = weight(diag02) = wtd_01 + wt_6;
%%        wtd_03 = weight(diag03) = wtd_02 + wt_5;
%%        wtd_04 = weight(diag04) = wtd_03 + wt_5;
%%        wtd_05 = weight(diag05) = wtd_04 + wt_4;
%%        wtd_06 = weight(diag06) = wtd_05 + wt_4;
%%        wtd_07 = weight(diag07) = wtd_06 + wt_3;
%%        wtd_08 = weight(diag08) = wtd_07 + wt_3;
%%        wtd_09 = weight(diag09) = wtd_08 + wt_2;
%%        wtd_10 = weight(diag10)  =wtd_09 + wt_2;
%%        wtd_11 = weight(diag11) = wtd_10 + wt_1;
%%        wtd_12 = weight(diag12) = wtd_11 + wt_1;
%%
%%        wtt_01 = wtd_01;
%%        wtt_02 = wtt_01 + wtd_02;
%%        wtt_03 = wtt_02 + wtd_03;
%%        wtt_04 = wtt_03 + wtd_04;
%%        wtt_05 = wtt_04 + wtd_05;
%%        wtt_06 = wtt_05 + wtd_06;
%%        wtt_07 = wtt_06 + wtd_07;
%%        wtt_08 = wtt_07 + wtd_08;
%%        wtt_09 = wtt_08 + wtd_09;
%%        wtt_10 = wtt_09 + wtd_10;
%%        wtt_11 = wtt_10 + wtd_11;
%%        wtt_12 = wtt_11 + wtd_12;
%%
%%     by the following function weight_trigonum/0, we have the following weight values, 
%%        ------------------------
%%            wt_1 through to wt_6
%%     wt_1 = 0.0037878787878787876,
%%     wt_2 = 0.004629629629629629,
%%     wt_3 = 0.005952380952380952,
%%     wt_4 = 0.008333333333333333,
%%     wt_5 = 0.013888888888888888,
%%     wt_6 = 0.041666666666666664
%%        ------------------------
%%            wtt_01 throught to wwt_12
%%     0.0037878787878787876,
%%     0.011363636363636362,
%%     0.023569023569023566,
%%     0.0404040404040404,
%%     0.06319143819143819,
%%     0.09193121693121692,
%%     0.12900432900432898,
%%     0.17441077441077438,
%%     0.23370610870610867,
%%     0.3068903318903319,
%%     0.42174122174122175,
%%     0.5782587782587782
%%
%%    based on the above calculation, take 0.15 as the threshold value of coefficient between attack_field and neighbor_region(x,y).
%%    if coefficient < 0.1,  the player can be regarded as out of danger;
%%    if coefficient > 0.2,  the player can be regarded as in high danger.
%%    0.1,     0.15,    0.2,     0.3,     0.4,    0.5 can be used as hp(hit point).
%%
%% 11. ripple algorithm to calculate.
%%     (1) calculate the rrcell where the player wander:
%%                let x = floor(xp/rrcl)*rrcl,     y = floor(yp/rrcw)*rrcw.
%%        Ripple_1 = {(x+m*rrcl,y+n*rrcw) | m,n = 0, 1};
%%                      = {(x, y+n*rrcw) | n = 0, 1} + {(x + rrcl, y+n*rrcw) | n = 0, 1};
%%    (2) Ripple_2
%%                      = {(x - rrcl, y+n*rrcw) | n = -1, 0, 1, 2} + {(x + 2*rrcl, y+n*rrcw) | n = -1, 0, 1, 2}
%%                      + {(x + m*rrcl, y - rrcw) | m =  0, 1} + {(x + m*rrcl, y + 2 * rrcw) | m =  0, 1};
%%    (3) general ripple generation algorithm:
%%        delete the factors: x, y and rrcl, rrcw,
%%        left bottom vertex's cooridinates: (0,0), (-1, -1), (-2,-2), (-3,-3), (-4,-4), (-5,-5).
%%        for n = 1,2,3,4,5,6,
%%        ripple_n's left bottom corner pointer is at LB_n =  (-n+1, -n+1),
%%        ripple_n's side length is Side_n = 2*n.
%%        from the above 2 piece of information, we can get all the vertexes on ripple_n:
%%        now, we have ripple_n's 4 corners:
%%                LB_n = (-n+1, -n+1),
%%                RB_n = ( n+1, -n+1),
%%                RT_n = ( n+1, n+1),
%%                LT_n = (-n+1, n+1),
%%        now, we have the four ripple sides(in counter-clockwise direction):
%%                SB_n = {LB_n + (x,0)| x = 0, 1, ..., 2n-2},
%%                SR_n = {RB_n + (0,y)| y = 0, 1, ..., 2n-2},
%%                ST_n = {RT_n - (x,0)| x = 0, 1, ..., 2n-2},
%%                ST_n = {LT_n - (0,y)| x = 0, 1, ..., 2n-2}.
%%
%% 12. calculating kronecker_delta(P, attack_field) .
%%     use polar coordinates to represent the topological relation between two positions;
%%     use polar coordinates to represent a player's aoe.
%%
%%
%%
%% ------------------------------------------------************************************************------------------------------------------------

%% erlang implementation:
%% 1. use modules(wx,...) to display the effect.

%% ------------------------------------------------************************************************------------------------------------------------

-module(aoe).

-export([start/0]).

%% ====================MAIN PART====================

weight_trigonum() ->
    List_k = lists:seq(6, 1, -1),
    List_wt = lists:map(fun(X) -> 1/24  * 1/(2*X - 1) end, List_k),
    List_wt2 = lists:flatmap(fun(X)->[X, X] end, List_wt),
    {List_wtd,_} = lists:mapfoldl(fun(X, Sum)->{Sum+X, Sum+X} end, 0, List_wt2),
    {List_wtt,_} = lists:mapfoldl(fun(X, Sum)->{Sum+X, Sum+X} end, 0, List_wtd),
    io:format("List_k = ~p~n", [List_k]),
    io:format("List_wt = ~p~n", [List_wt]),
    io:format("List_wt2 = ~p~n", [List_wt2]),
    io:format("List_wtd = ~p~n", [List_wtd]),
    io:format("List_wtt = ~p~n", [List_wtt]),
    ok.


%% ====================TEST PART==================

test() ->
    weight_trigonum(),
    ok.

start() ->
    test().

%% ========================END=======================
