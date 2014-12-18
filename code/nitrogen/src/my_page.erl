%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et

%% nitrogen example from
%% http://nitrogenproject.com/doc/tutorial.html
%% 

-module (my_page).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello World!".

%% body() -> "Hello World!".

%% body() -> #label { text="Hello World!" }.

%% body() -> 
%%     [
%%         #panel { style="margin: 50px 100px;", body=[
%%             #span { text="Hello World!" },

%%             #p{},
%%             #button { text="Click me!", postback=click },

%%             #p{},
%%             #panel { id=placeholder }
%%         ]}
%%     ].

%% body() -> 
%% [
%%     #h1 { text="My Simple Application" },
%%     #label { text="What is your name?" },
%%     #textbox { },
%%     #button { text="Submit" }
%% ].

%% body() -> 
%%     #panel { 
%%         style="margin: 50px;", 
%%         body=[
%%             #h1 { text="My Page" },
%%             #label { text="Enter Your Name:" },
%%             #textbox { },
%%             #button { text="Submit" }]
%%     }.

%% body() ->
%%     [
%%         #button {
%%              text="Submit", 
%%             actions=[#event{type=click,actions="alert('hello');" }]}
%%     ].

%% body() ->
%%     [
%%         #button{
%%             text="Submit",
%%             actions=[#event{type=click, actions=#alert{text="Hello"}]
%%         }
%%     ].

%% body() -> 
%%     wf:wire(mybutton, #effect { effect=pulsate }),
%%     [
%%         #button { id=mybutton, text="Submit" }
%%     ].

body() -> 
    wf:wire(
        mybutton,
        #event{ 
            type=click, 
            actions=#effect { effect=pulsate}}),
    [
        #button { id=mybutton, text="Submit" }
    ].
	
%% event(click) ->
%%     wf:insert_top(placeholder, "<p>You clicked the button!").
