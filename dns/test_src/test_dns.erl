%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_dns).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("../include/dns.hrl").
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

%% Build and release a service and application josca

start_test()->
    dns:app_start(?DNS_IP,?DNS_PORT,dns,"1.0.0"),
    ok.

check_if_dns_node_alive_test()->
    D=date(),
    D=tcp:call(?DNS_IP,?DNS_PORT,[erlang,date,[]]),
    ok.

register_0_test()->
  %  []=tcp:call("localhost",20200,[dns,get_all_instances,[]]),
    ok.

register_1_test()->
    ServiceIp="Servicie_ip_Address",
    ServicePort=10010,
    ServiceId="service_1",
    Vsn="1.0.0",
    InitArgs=addr_mgr:update_init_args(ServiceIp, ServicePort,ServiceId,Vsn),
    tcp:call(?DNS_IP,?DNS_PORT,[dns,register,[InitArgs]]),
    [[{public_ip,"Servicie_ip_Address"},
      {public_port,10010},
      {service_id,"service_1"},
      {vsn,"1.0.0"}]]=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_all_instances,[]]),
    ok.

de_register_1_test()->
    ServiceIp="Servicie_ip_Address",
    ServicePort=10010,
    ServiceId="service_1",
    Vsn="1.0.0",
    InitArgs=addr_mgr:update_init_args(ServiceIp, ServicePort,ServiceId,Vsn),
    tcp:call(?DNS_IP,?DNS_PORT,[dns,de_register,[InitArgs]]),
    []=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_all_instances,[]]),
    ok.

register_2_test()->
    ServiceIp_1="Servicie_ip_Address",
    ServiceIp_2="Servicie_ip_Address_2", 
    ServicePort=10010,
    ServiceId="service_1",
    Vsn="1.0.0",
    InitArgs_1=addr_mgr:update_init_args(ServiceIp_1, ServicePort,ServiceId,Vsn),
    tcp:call(?DNS_IP,?DNS_PORT,[dns,register,[InitArgs_1]]),
    InitArgs_2=addr_mgr:update_init_args(ServiceIp_2, ServicePort,ServiceId,Vsn),
    tcp:call(?DNS_IP,?DNS_PORT,[dns,register,[InitArgs_2]]),
    [[{public_ip,"Servicie_ip_Address_2"},
      {public_port,10010},
      {service_id,"service_1"},
      {vsn,"1.0.0"}],
     [{public_ip,"Servicie_ip_Address"},
      {public_port,10010},
      {service_id,"service_1"},
      {vsn,"1.0.0"}]]=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_all_instances,[]]),
    ok.

get_instances_test()->
    ServiceId="service_1",
    Vsn="1.0.0",
    [[{public_ip,"Servicie_ip_Address_2"},{public_port,10010}],
     [{public_ip,"Servicie_ip_Address"},{public_port,10010}]
    ]=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,[ServiceId,Vsn]]),
    [[{public_ip,"Servicie_ip_Address_2"},{public_port,10010}],
     [{public_ip,"Servicie_ip_Address"},{public_port,10010}]
    ]=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,[ServiceId]]),
    []=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,[glurk]]),
    []=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,[ServiceId,glurk]]),
    []=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,[glurk,Vsn]]),
    []=tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,[glurk,glurk]]),
    ok.
stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
