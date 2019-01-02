%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(vim_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("services/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
boot()->
    vim:app_start(?VIM_PUBLIC_IP,?VIM_PUBLIC_PORT,?VIM_LOCAL_IP,?VIM_LOCAL_PORT,"vim","1.0.0").



check_capa_zone(NeededCapa,NeededZoneList,NfviList)->
    ActiveWorkers=vim_lib:get_all_instances(NfviList),
%    io:format("~p~n",[{?MODULE,?LINE,NeededCapa,NeededZoneList,ActiveWorkers}]),
  %  Glurk=[[{init_args,InitArgs},{zone,_Zone},{capabilities,Capabilites}]||[{init_args,InitArgs},{zone,_Zone},{capabilities,Capabilites}]<-ActiveWorkers],
     W = case NeededZoneList of
	    []->
		[InitArgs||[{init_args,InitArgs},{zone,_Zone},{capabilities,Capabilites}]<-ActiveWorkers,
			   lists:flatlength([true||Z<-NeededCapa,lists:member(Z,Capabilites)==true])==lists:flatlength(NeededCapa)];
	    [NeededZone]->
		[InitArgs||[{init_args,InitArgs},{zone,Zone},{capabilities,Capabilites}]<-ActiveWorkers,
			   Zone==NeededZone,
			   lists:flatlength([true||Z<-NeededCapa,lists:member(Z,Capabilites)==true])==lists:flatlength(NeededCapa)]
	end,
 %  io:format("~p~n",[{?MODULE,?LINE,W}]),
    Workers=[{lists:keyfind(public_ip,1,InitArgs),lists:keyfind(public_port,1,InitArgs),
	      lists:keyfind(local_ip,1,InitArgs),lists:keyfind(local_port,1,InitArgs)}||InitArgs<-W],
 %   io:format("glurk  ~p~n",[{?MODULE,?LINE,Workers}]),
    Workers.
    


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
update_nfvis()->  %glurk 
    AllInstances=if_dns:call("dns",dns,get_all_instances,[]),
     io:format("glurk  ~p~n",[{?MODULE,?LINE,AllInstances}]),
    Nfvis=[[{public_ip,PublicIp},{public_port,PublicPort},{local_ip,LocalIp},{local_port,LocalPort},{service_id,Id},{vsn,Vsn}]||
	      [{public_ip,PublicIp},{public_port,PublicPort},{local_ip,LocalIp},{local_port,LocalPort},
	       {service_id,Id},{vsn,Vsn}]<-AllInstances,Id=="nfvi"],
    Nfvis.
									  

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_availible_ports(Start,Stop)->
    N=Stop-Start,
    create_availible_ports(N,Start,[]).

create_availible_ports(-1,_Start,Acc)->
    Acc;
create_availible_ports(N,Start,Acc) ->
    NewAcc=[Start+N|Acc],
    create_availible_ports(N-1,Start,NewAcc).





%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_all_instances(NfviList)->
    AllNfvi=[NfviInfo||{_TimeStamp,NfviInfo}<-NfviList],
    AllNfvi.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


get_local_info(IpAddrNfviStop,PortNfviStop,NfviList)->
    get_local_info(IpAddrNfviStop,PortNfviStop,NfviList,{error,not_exist}).

get_local_info(_,_,_,{ok,LIp,LPort})->
    {ok,LIp,LPort};
get_local_info(_,_,[],{error,not_exist})->
    {error,not_exist};
get_local_info(IpAddrNfviStop,PortNfviStop,[{_,VnfiInfo}|T],Result) ->

    {init_args,InitArgs}=lists:keyfind(init_args,1,VnfiInfo),
    PIp=addr_mgr:init_args_public_ip(InitArgs),
    PPort=addr_mgr:init_args_public_port(InitArgs),
    case {IpAddrNfviStop,PortNfviStop}=={PIp,PPort} of
	true->
	    LIp=addr_mgr:init_args_local_ip(InitArgs),
	    LPort=addr_mgr:init_args_local_port(InitArgs),
	    NewResult={ok,LIp,LPort};
	false ->
	    NewResult=Result
    end,
    get_local_info(IpAddrNfviStop,PortNfviStop,T,NewResult).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
register(Info, VnfiList) ->
    Elem=[{TimeStamp,VnfiInfoElem}||{TimeStamp,VnfiInfoElem}<-VnfiList,
				   Info==VnfiInfoElem
	 ],			   
    NewVnfiList=case Elem of
		   [] ->
			[{erlang:timestamp(),Info}|VnfiList];
		   [{_,VnfiInfoElem}] ->
		       lists:keyreplace(VnfiInfoElem,2,VnfiList,{erlang:timestamp(),VnfiInfoElem})
		end,
    NewVnfiList.


de_register(InitArgs, VnfiList)->
    NewVnfiList=[{TimeStamp,VnfiInfo}||{TimeStamp,VnfiInfo}<-VnfiList,
				      (lists:keyfind(init_args,1,VnfiInfo)=={init_args,InitArgs})==false
	       ],
    NewVnfiList.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_nfvi_candidates(NeededCapa,NeededZoneList,AvailibleNfvis)->
    case check_capa_zone(NeededCapa,NeededZoneList,AvailibleNfvis) of
	[]->
	    {error,no_availible_candidates};
	NfviCandidates->
	    {ok,NfviCandidates}
    end.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_already_started(ServiceIdStart,VsnStart,StartedServices)->
    check_already_started(ServiceIdStart,VsnStart,StartedServices,{ok,not_started}).

check_already_started(_,_,[],Acc)->
    Acc;

check_already_started(_,_,_,{error,already_started})->
    {error,already_started};

check_already_started(ServiceIdStart,VsnStart,[{_InstanceId,ServiceId,Vsn,_StartedServices}|T],Acc)->
    case {ServiceIdStart,VsnStart}=={ServiceId,Vsn} of
	true->
	    NewAcc={error,already_started};
	false ->
	    NewAcc=Acc
    end,
    check_already_started(ServiceIdStart,VsnStart,T,NewAcc).

