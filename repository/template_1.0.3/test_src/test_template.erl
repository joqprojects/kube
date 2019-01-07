%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_template).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%-export([start/0]).
-export([]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 1. Initial set up
start_test()->
    ok=application:start(template),   
    ok.

add_1_test()->
    42=if_template:add(20,22),
    ok.

divi_1_test()->
    42.0=if_template:divi(420,10),
    ok.

divi_2_test()->
    {badrpc,_}=if_template:divi(420,0),
    ok.


stop_test()->    
    ok=application:stop(template),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
