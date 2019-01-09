%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_controller).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
%% --------------------------------------------------------------------
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
%% --------------------------------------------------------------------
%% Requires that nfvi_1-nfvi_4 and vim are started 
extract_joscafiles_test()->

    [{specification,calc_app},
     {description,"calculator application"},
     {vsn,"1.0.0"},
     {services,[]},
     {num_instances,1},
     {zone,[]},
     {geo_red,[]},
     {needed_capabilities,[]},
     {josca_files,[{Adder,AdderVsn},{Dvider,DividerVsn}]}
    ]=if_dns:call("repo",repo,read_josca_file,["calc_app","1.0.0"]),
    {ok,"adder","1.0.0",AdderIp,AdderPort}=if_dns:call("vim",vim,start_service,[Adder,AdderVsn,[disc],["lgh.joq_room"]]),
    42=if_dns:call("adder",adder,add,[20,22]),
    {ok,"divider","1.0.0",DividerIp,DividerPort}=if_dns:call("vim",vim,start_service,[Dvider,DividerVsn,[disc],[]]),
    42=if_dns:call("adder",adder,add,[20,22]),
    
    42.0=if_dns:call("divider",divider,divi,[420,10]),

    {error,[vim,_Line,'service already started',
	    [[{ip_addr,"localhost"},
	      {port,30010},
	      {service_id,"adder"},
	      {vsn,"1.0.0"}]]]
    }=if_dns:call("vim",vim,start_service,[Adder,AdderVsn,[disc],["lgh.joq_room"]]),
    42=if_dns:call("adder",adder,add,[20,22]),
    {error,[vim,_Line,'service already started',
	    [[{ip_addr,"localhost"},
	      {port,30011},
	      {service_id,"divider"},
	      {vsn,"1.0.0"}]]]
    }=if_dns:call("vim",vim,start_service,[Dvider,DividerVsn,[disc],[]]),
    42.0=if_dns:call("divider",divider,divi,[420,10]),

    ok=if_dns:call("vim",vim,stop_service,[AdderIp,AdderPort]),
    ok=if_dns:call("vim",vim,stop_service,[DividerIp,DividerPort]),
    {error,{econnrefused,tcp,_59}}=if_dns:call("adder",adder,add,[20,22]),
   % {error,{econnrefused,tcp,_59}}=if_dns:call("divider",divider,divi,[420,10]),
    ok.

kill_nodes_test()->
    if_dns:call("vim",vim,stop_service,["localhost",30010]),
    if_dns:call("vim",vim,stop_service,["localhost",30011]),

    ok.

stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
