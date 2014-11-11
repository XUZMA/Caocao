%% author: xzm
%% date: 2014-11-11

%% the module demonstrates the usage of wxEvtHandler through  a series of menu operations.

-module( wxMenuEvent_demo).

%%-compile(export_all).
-export([start/0]).

%%    ================================================

%% in Eshell, run code:lib_dir(wx) to get wx lib dir: "/usr/local/lib/erlang/lib/wx-1.3.1"
-include("/usr/local/lib/erlang/lib/wx-1.3.1/include/wx.hrl").

%%    ----------------define event handlers----------------

gui_test(X) ->
    Diag_test = wxMessageDialog:new(wx:null(), X),
    wxMessageDialog:showModal(Diag_test),
    ok.

on_Sect(_,_) ->
    gui_test("Sect").

on_Rect(_,_) ->
    gui_test("Rect").

on_Quit(_,_) ->
    gui_test("Quit").

on_About(_,_) ->
    gui_test("About").

%%    ----------------------------------------------------------------

canvas_mini() ->
%%    ---- base window    --------
    Wx=wx:new(),
    Frm=wxFrame:new(Wx, -1, "canvas mini"),
    wxFrame:show(Frm),

%%    ----    status bar    ----
    wxFrame:createStatusBar(Frm),
    wxFrame:setStatusText(Frm, "mini canvas"),

%%    ----    Menu    ----
    Mb = wxMenuBar:new(),
    wxFrame:setMenuBar (Frm, Mb),
    Mn_Draw = wxMenu:new(),
    wxMenuBar:append(Mb, Mn_Draw, "&Draw"),
    Mni_Sector = wxMenuItem:new ([{id,100},{text, "&Sector"}]),
    wxMenu:append (Mn_Draw, Mni_Sector),
    Mni_Rect = wxMenuItem:new ([{id,200},{text, "&Rect"}]),
    wxMenu:append (Mn_Draw, Mni_Rect),
    Mni_Quit = wxMenuItem:new ([{id,300},{text, "&Quit"}]),
    wxMenu:append (Mn_Draw, Mni_Quit),

    Mn_Help = wxMenu:new(),
    wxMenuBar:append(Mb, Mn_Help, "&Help"),
    Mni_About = wxMenuItem:new ([{id,400},{text, "&About"}]),
    wxMenu:append (Mn_Help, Mni_About),

%%    ----    install event handlers   ----
    wxFrame:connect(Frm, command_menu_selected,  [{id,100},{lastId,100},{callback, on_Sect}]),
    wxFrame:connect(Frm, command_menu_selected,  [{id,200},{lastId,200},{callback, on_Rect}]),
    wxFrame:connect(Frm, command_menu_selected,  [{id,300},{lastId,300},{callback, on_Quit}]),
    wxFrame:connect(Frm, command_menu_selected,  [{id,400},{lastId,400},{callback, on_About}]),

%%    ----
%%    wxFrame:destroy(Frm).
    
    ok.

%%    ================================================

start() ->
    canvas_mini(),
    ok.

%%    ================================================
