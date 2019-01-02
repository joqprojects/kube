%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dns_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").
-include("kube/include/data.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
boot()->
    dns:app_start(?DNS_PUBLIC_IP,?DNS_PUBLIC_PORT,na,na,"dns","1.0.0").




get_all_instances(DnsList)->
    [DnsInfo || {_TimeStamp,DnsInfo}<-DnsList].

get_instances(WantedServiceStr,DnsList)->
    Reply=[{PublicIpAddr,PublicPort,LocalIp,LocalPort} || {_TimeStamp,[{public_ip,PublicIpAddr},{public_port,PublicPort},
									       {local_ip,LocalIp},{local_port,LocalPort},
									       {service_id,ServiceIdStr},{vsn,_VsnStr}]
								  }<-DnsList,
								  WantedServiceStr==ServiceIdStr
	  ],
   % io:format(" get instances ~p~n",[{?MODULE,?LINE,Reply}]),    
    Reply.

get_instances(WantedServiceStr,WantedVsnStr,DnsList)->
    Reply=[{PublicIpAddr,PublicPort,LocalIp,LocalPort} || {_TimeStamp,[{public_ip,PublicIpAddr},{public_port,PublicPort},
									       {local_ip,LocalIp},{local_port,LocalPort},
									       {service_id,ServiceIdStr},{vsn,VsnStr}]
								  }<-DnsList,
								  {WantedServiceStr,WantedVsnStr}=={ServiceIdStr,VsnStr}
	  ],
  %  io:format(" get instances ~p~n",[{?MODULE,?LINE,Reply}]), 
    Reply.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
register(InitArgs, DnsList) ->
 %   io:format("~p~n",[{?MODULE,?LINE,InitArgs}]),
  %  io:format("~p~n",[{?MODULE,?LINE,DnsList}]),
 %  [{public_ip,Ip},{public_port,Port},{service_id,ServiceId},{vsn,Vsn}]
  
    Elem=[{TimeStamp,DnsInfoElem}||{TimeStamp,DnsInfoElem}<-DnsList,
				   InitArgs==DnsInfoElem
	 ],			   
    NewDnsList=case Elem of
		   [] ->
		       local_log_call(InitArgs,ok,[?MODULE,?LINE,'service registrated',
						   addr_mgr:init_args_service_id(InitArgs),
						   addr_mgr:init_args_vsn(InitArgs)
						  ],
				      DnsList),
		       [{erlang:timestamp(),InitArgs}|DnsList];
			   
		   [{_,DnsInfoElem}] ->
		       lists:keyreplace(DnsInfoElem,2,DnsList,{erlang:timestamp(),DnsInfoElem})
		end,
    NewDnsList.


local_log_call(InitArgs,Type,Info,DnsList)->
    ServiceId=addr_mgr:init_args_service_id(InitArgs),
    Vsn=addr_mgr:init_args_vsn(InitArgs),
    Event=[{public_ip,addr_mgr:init_args_public_ip(InitArgs)},
	   {public_port,addr_mgr:init_args_public_port(InitArgs)},
	   {local_ip,addr_mgr:init_args_local_ip(InitArgs)},
	   {local_port,addr_mgr:init_args_local_port(InitArgs)},
	   {service_id,ServiceId},
	   {vsn,Vsn},
	   {event_type,Type},
	   {event_info,Info}
	  ],
    case dns_lib:get_instances("log","1.0.0",DnsList) of
	[{PublicIp,PublicPort,LocalIp,LocalPort}]->
	    ok=tcp:call(PublicIp,PublicPort,LocalIp,LocalPort,{log,add_event,[Event]});
	Err ->
	    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
    end.


de_register(InitArgs, DnsList)->
    NewDnsList=[{TimeStamp,DnsInfo}||{TimeStamp,DnsInfo}<-DnsList,
				      (DnsInfo==InitArgs)==false
	       ],
    local_log_call(InitArgs,ok,[?MODULE,?LINE,'service de-registrated',
						   addr_mgr:init_args_service_id(InitArgs),
						   addr_mgr:init_args_vsn(InitArgs)],
		   DnsList),
    NewDnsList.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
