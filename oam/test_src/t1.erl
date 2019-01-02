%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(t1).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
%-include_lib("eunit/include/eunit.hrl").
-include("services/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------
%-export([start/0]).
-export([s1/1,
	s2/1]).

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
%% -------------------------------------------------------------------

s1(N)->
    s10(N).

s10(N)->
    R0=if_dns:call("oam",oam,start_app,["subtract","1.0.0"]),
    io:format("Started  ~p~n",[R0]),
    R1=if_dns:call("subtract",subtract,sub,[20,22]),    
    io:format("First result ~p~n",[R1]),
    do_s1(N-1).
	      
do_s1(0)->	       
    R3=if_dns:call("oam",oam,stop_app,["subtract","1.0.0"]),
    io:format("Stopped subtract ~p~n",[R3]);
do_s1(N) ->	
    if_log:call(debug,['>>>>>>>>  ','N = ',N,'   <<<<<<<<<<<<<<<<<<<<<']),
    PrListMaster=tcp:call(?NFVI_MASTER_PUBLIC_IP,?NFVI_MASTER_PUBLIC_PORT,
			  ?NFVI_MASTER_LOCAL_IP,?NFVI_MASTER_LOCAL_PORT,
			  {erlang,processes,[]}),
    io:format("Master Processes ~p~n",[{?MODULE,?LINE,lists:flatlength(PrListMaster)}]),
    io:format("test_oam Processes ~p~n",[{?MODULE,?LINE, lists:flatlength(erlang:processes())}]),
    case if_dns:call("subtract",subtract,sub,[20,22]) of
	-2 ->
	   ok;
	Err->
	    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
    end,	   
    do_s1(N-1).

s2(N)->
    s20(N).

s20(N)->
    R0=if_dns:call("oam",oam,start_app,["subtract","1.0.0"]),
    io:format("Started  ~p~n",[R0]),
    R1=if_dns:call("subtract",subtract,sub,[20,22]),    
    io:format("First result ~p~n",[R1]),
    do_s2(N-1).
	      
do_s2(0)->	       
    R3=if_dns:call("oam",oam,stop_app,["subtract","1.0.0"]),
    io:format("Stopped subtract ~p~n",[R3]);
do_s2(N) ->	
    if_log:call(debug,['>>>>>>>>  ','N = ',N,'   <<<<<<<<<<<<<<<<<<<<<']),
    case if_dns:call("subtract",subtract,sub,[20,22]) of
	-2 ->
	   ok;
	Err->
	    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
    end,	   
    do_s1(N-1).

