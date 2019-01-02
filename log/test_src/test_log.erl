%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_log).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("../include/dns.hrl").
-include("../include/tcp.hrl").
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
    log:app_start(?LOG_IP,?LOG_PORT,"log","1.0.0"),
    ok.

add_event_1_test()->
    Event=[{public_ip,"80.xx"},{public_port,20100},{service_id,"S1"},
	   {vsn,"1.0.0"},{event_type,error},
	   {event_info,[?MODULE,?LINE,'already_defined',"file.txt"]}],
    tcp:call(?LOG_IP,?LOG_PORT,[log,add_event,[Event]]),
    [[{public_ip,"80.xx"},
      {public_port,20100},
      {service_id,"S1"},
      {vsn,"1.0.0"},
      {event_type,error},
      {event_info,[test_log,_50,already_defined,"file.txt"]},
      {date,_},
      {time,_}],
     [{public_ip,"localhost"},
      {public_port,20100},
      {service_id,"log"},
      {vsn,"1.0.0"},
      {event_type,error},
      {event_info,[log,_121,'already started',log]},
      {date,_},
      {time,_}],
     [{public_ip,?LOG_IP},
      {public_port,?LOG_PORT},
      {service_id,"log"},
      {vsn,"1.0.0"},
      {event_type,ok},
      {event_info,[log,_116,'service started',log]},
      {date,_},
      {time,_}
     ]]=tcp:call(?LOG_IP,?LOG_PORT,[log,read_events,[3]]),
    ok.
    
filter_events_test()->
    Events=tcp:call(?LOG_IP,?LOG_PORT,[log,read_events,[3]]),
    Key=error,
    L=[Event||Event<-Events,lists:keyfind(event_type,1,Event) == {event_type,Key}],
    [[{public_ip,"80.xx"},
      {public_port,20100},
      {service_id,"S1"},
      {vsn,"1.0.0"},
      {event_type,error},
      {event_info,[test_log,_50,already_defined,"file.txt"]},
      {date,_},
      {time,_}],
     [{public_ip,"localhost"},
      {public_port,20100},
      {service_id,"log"},
      {vsn,"1.0.0"},
      {event_type,error},
      {event_info,[log,_121,'already started',log]},
      {date,_},
      {time,_}
     ]]=L,
    ok.

filter_events_2_test()->
    Events=tcp:call(?LOG_IP,?LOG_PORT,[log,read_events,[3]]),
    Key=ok,
    L=[Event||Event<-Events,lists:keyfind(event_type,1,Event) == {event_type,Key}],
    [[{public_ip,"localhost"},
      {public_port,20100},
      {service_id,"log"},
      {vsn,"1.0.0"},
      {event_type,ok},
      {event_info,[log,_116,'service started',log]},
      {date,_},
      {time,_}
     ]]=L,
    ok.

stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
