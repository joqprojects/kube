%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(if_log).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").
%% --------------------------------------------------------------------



%% External exports
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
call(InitArgs,Type,Info)->
    Event=[{public_ip,addr_mgr:init_args_public_ip(InitArgs)},
	   {public_port,addr_mgr:init_args_public_port(InitArgs)},
	   {local_ip,addr_mgr:init_args_local_ip(InitArgs)},
	   {local_port,addr_mgr:init_args_local_port(InitArgs)},
	   {service_id,addr_mgr:init_args_service_id(InitArgs)},
	   {vsn,addr_mgr:init_args_vsn(InitArgs)},
	   {event_type,Type},
	   {event_info,Info}],
 %   io:format("~p~n",[{?MODULE,?LINE,Event}]),
 %  R= tcp:call(?DNS_IP,?DNS_PORT,[log,add_event,Event]),
    R=if_dns:call("log",log,add_event,[Event]),
 %   io:format("~p~n",[{?MODULE,?LINE,R}]).
    R.

call(Type,Info)->
    R=if_dns:call("log",log,print_event,[Type,Info]),
 %   io:format("~p~n",[{?MODULE,?LINE,R}]).
    R.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

