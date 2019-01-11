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
-include("kube/include/dns_data.hrl").
-include("kube/include/data.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
dns_register(DnsInfo, DnsList) ->
   TimeStamp=erlang:now(),
    NewDnsInfo=DnsInfo#dns_info{time_stamp=TimeStamp},
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    
    X1=[X||X<-DnsList,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList=[NewDnsInfo|X1],
    NewDnsList.

de_dns_register(DnsInfo,DnsList)->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    NewDnsList=[X||X<-DnsList,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
local_log_call(DnsInfo,Type,Info,_DnsList)->
    ServiceId=DnsInfo#dns_info.service_id,
    Vsn=DnsInfo#dns_info.vsn,
    IpAddr=DnsInfo#dns_info.ip_addr,
    Port=DnsInfo#dns_info.port,	
    Event=[{ip_addr,IpAddr},
	   {port,Port},
	   {service_id,ServiceId},
	   {vsn,Vsn},
	   {event_type,Type},
	   {event_info,Info}
	  ],
    %case dns_lib:get_instances("log","1.0.0",DnsList) of
%	[{PublicIp,PublicPort,LocalIp,LocalPort}]->
%	    ok=tcp:call(PublicIp,PublicPort,LocalIp,LocalPort,{log,add_event,[Event]});
%	Err ->
%	    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
 %   end.
    Event.



get_instances(WantedServiceStr,DnsList)->
    Reply=[DnsInfo||DnsInfo<-DnsList, WantedServiceStr=:=DnsInfo#dns_info.service_id], 
    Reply.

get_instances(WantedServiceStr,WantedVsnStr,DnsList)->
    Reply=[DnsInfo||DnsInfo<-DnsList, {WantedServiceStr,WantedVsnStr}=:={DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}], 
    Reply.


