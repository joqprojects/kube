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
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
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
    % This will be executed by kubelet
    ok=application:set_env(dns,ip_addr,?DNS_PUBLIC_IP),
    ok=application:set_env(dns,port,?DNS_PUBLIC_PORT),
    ok=application:set_env(dns,service_id,"dns"),
    ok=application:set_env(dns,vsn,"1.0.0"),
    ok=application:load(dns),
    ok=application:start(dns),
    ok.

empty_dns_list_test()->
    []=dns:get_all_instances(),
    ok.

register_instances_1_test()->
    S0=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = "s0",
			vsn = "1.0.0",
			ip_addr="80.10.20.00",
			port=20000
		       },
    ok=dns:dns_register(S0),
    S1=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = "s1",
			vsn = "1.0.1",
			ip_addr="80.10.20.01",
			port=20001
		       },
    ok=dns:dns_register(S1),
    [{dns_info,_,"s1","1.0.1","80.10.20.01",20001},
     {dns_info,_,"s0","1.0.0","80.10.20.00",20000}
    ]=dns:get_all_instances(),
    ok=dns:dns_register(S0),
    [{dns_info,_,"s0","1.0.0","80.10.20.00",20000},
     {dns_info,_,"s1","1.0.1","80.10.20.01",20001}
    ]=dns:get_all_instances(),
    ok.

de_register_test()->
    S0=#dns_info{time_stamp="not_initiaded_time_stamp",
		 service_id = "s0",
		 vsn = "1.0.0",
		 ip_addr="80.10.20.00",
		 port=20000
		},
    ok=dns:de_dns_register(S0),   
    [{dns_info,_,"s1","1.0.1","80.10.20.01",20001}
    ]=dns:get_all_instances(),
    ok.

register_instances_2_test()->
    S0=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = "s0",
			vsn = "1.0.0",
			ip_addr="80.10.20.00",
			port=20000
		       },
    ok=dns:dns_register(S0),
    S1=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = "s1",
			vsn = "1.0.1",
			ip_addr="80.10.20.01",
			port=20001
		       },
    ok=dns:dns_register(S1),
    S2=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = "s2",
			vsn = "1.0.2",
			ip_addr="80.10.20.02",
			port=20002
		       },
    ok=dns:dns_register(S2),
    ok.

stop_test()->    
    ok=application:stop(dns),    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
