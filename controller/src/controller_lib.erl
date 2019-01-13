%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/kubelet_data.hrl").
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
needed_services(ApplicationList)->
    needed_services(ApplicationList,[]).

needed_services([],NeededServices)->
    NeededServices;
needed_services([{{AppId,Vsn},JoscaFile}|T],Acc)->
    {dependencies,ServiceList}=lists:keyfind(dependencies,1,JoscaFile),
    NewAcc=check_services(ServiceList,Acc),
    needed_services(T,NewAcc).

check_services([],Acc)->
    Acc;
check_services([{Id,Vsn}|T],Acc) ->
    NewAcc=case josca:start_order(Id,Vsn) of
	       {error,Err}->
		   io:format("error~p~n",[{?MODULE,?LINE,Err}]),
		   Acc;
	       Services ->
		   case lists:member({Id,Vsn},Acc) of
		       true->
			   Acc;
		       false->
			   lists:append(Services,Acc)
		   end
	   end,
    check_services(T,NewAcc).

missing_services(NeededServices,DnsList)->
    AvailibleServices=[{DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}||DnsInfo<-DnsList],
    [{Id,Vsn}||{Id,Vsn}<-NeededServices, 
	       lists:member({Id,Vsn},AvailibleServices)=:=false].


start_services([],Nodes)->
    ok;
start_services([{ServiceId,Vsn}|T],Nodes)->
 %   io:format("~p~n",[{?MODULE,?LINE,ServicesId,Vsn,Nodes}]),
    case if_dns:call("catalog",catalog,read,[ServiceId,Vsn]) of
	{error,Err}->
	    io:format("~p~n",[{?MODULE,?LINE,'error',Err}]);
	{ok,_,JoscaInfo}->
%	    io:format("~p~n",[{?MODULE,?LINE,JoscaInfo}]),
	    {zone,WantedZone}=lists:keyfind(zone,1,JoscaInfo),
	    {needed_capabilities,WantedCapabilities}=lists:keyfind(needed_capabilities,1,JoscaInfo),
	    NodesFullfilledNeeds=get_nodes_fullfills_needs(WantedZone,WantedCapabilities,Nodes),
	 %   io:format("~p~n",[{?MODULE,?LINE,ServiceId,WantedZone,WantedCapabilities,'=>>',NodesFullfilledNeeds}]),
	    case NodesFullfilledNeeds of
		[]->
		    io:format("~p~n",[{?MODULE,?LINE,'error no availible nodes'}]);
		NodesFullfilledNeeds->
		    R=schedule_start(ServiceId,Vsn,NodesFullfilledNeeds),
		    io:format("~p~n",[{?MODULE,?LINE,'Service start result =',R,ServiceId,Vsn}])
	    end
    end,
    start_services(T,Nodes).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
schedule_start(ServicesId,Vsn,NodesFullfilledNeeds)->
    [KubeleteInfo|_]=NodesFullfilledNeeds,
    IpAddr=KubeleteInfo#kubelet_info.ip_addr,
    Port=KubeleteInfo#kubelet_info.port,

    R=tcp:call(IpAddr,Port,{kubelet,start_service,[ServicesId,Vsn]}),
    R.



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_nodes_fullfills_needs(WantedZone,WantedCapabilities,AvailibleNodes)->
    % Which nodes is in needed zone
    RightZone = case WantedZone of
		    []->
			AvailibleNodes;
		    Zone ->
		%	io:format("Zone=  ~p~n",[{?MODULE,?LINE,Zone}]), 
			[Node||Node<-AvailibleNodes,
				Node#kubelet_info.zone=:=Zone]
		end,
   % io:format("RightZone  ~p~n",[{?MODULE,?LINE,RightZone}]),    
    NodesFullfilledNeeds=case WantedCapabilities of
			     []->
				 RightZone;
			     WantedCapabilities->
				 [Node||Node<-RightZone,
					check_capbility(WantedCapabilities,Node)]
			 end,
    
    NodesFullfilledNeeds.


check_capbility(WantedCapabilities,Node)->
    check_capbility(WantedCapabilities,Node,false).
    
check_capbility([],_,Boolean)->
    Boolean;
check_capbility([WCap|T],Node,_)->    
    case lists:member(WCap,Node#kubelet_info.capabilities) of
	false->
	    Tail=[],  % Stop searching
	    R=false;  % Failed
	true->
	    Tail=T,   % Continue search
	    R=true    % Succeded 
    end,
    check_capbility(Tail,Node,R).    
	   
				
    % Which nodes in needed zone has the right capabilities



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

start_application(AppId,Vsn,InitArgs)->    
    ServicesToStart=josca:start_order(AppId,Vsn),
    StartResult=start_application_1(ServicesToStart,InitArgs,[]),
  %  io:format(" ~p~n",[{?MODULE,?LINE,StartResult}]),
    case [{error,Info}||{error,Info}<-StartResult] of
	[]-> % Succeded no errors
	  %  io:format(" ~p~n",[{?MODULE,?LINE,StartResult}]),
	    {ok,StartResult};
	Failed ->
	   % io:format(" ~p~n",[{?MODULE,?LINE,Failed}]),
	    {error,Failed}
    end.
    
start_application_1([],_InitArgs,Acc)->
 %   io:format(" ~p~n",[{?MODULE,?LINE,Acc}]),
    Acc;
start_application_1([{{service,ServiceId,Vsn},{zone,Zone},{needed_capabilities,Capability}}|Tail],InitArgs,Acc)->
  %  io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,Vsn}]),
    NumTries=4,
    Delay=40*1000, 
    R=deploy_service(ServiceId,Vsn,Capability,Zone,NumTries,Delay,InitArgs,false,glurk),
    NewAcc=[R|Acc],
    start_application_1(Tail,InitArgs,NewAcc).
    
deploy_service(_ServiceId,_Vsn,_Capability,_Zone,_NumTries,_Delay,_InitArgs,true,Result)->
    Result;
deploy_service(_ServiceId,_Vsn,_Capability,_Zone,0,_Delay,_InitArgs,_Quit,Result)->
    Result;
deploy_service(ServiceId,Vsn,Capability,Zone,NumTries,Delay,InitArgs,Quit,_Result)->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,Vsn}]),
    case nfv_mgr_lib:start_service(ServiceId,Vsn,Capability,Zone,InitArgs) of
	{error,Err}->
	    NewNumTries=NumTries-1,
	    NewQuit=Quit,
	    NewResult={error,[ServiceId,Vsn,Err]},
	    if_log:call(InitArgs,error,[?MODULE,?LINE,'couldnt start service',Err]),
	    timer:sleep(Delay);
	{ok,[{public_ip,PublicIp},
	     {public_port,PublicPort},
	     {local_ip,LocalIp},
	     {local_port,PortService},
	     {service_id,ServiceId},
	     {vsn,Vsn}]}->
	    ServiceInitArgs=addr_mgr:update_init_args(PublicIp,PublicPort,LocalIp,PortService,
					       ServiceId,Vsn),
	    ok=if_dns:call("dns",dns,register,[ServiceInitArgs]),
	    NewNumTries=NumTries,
	    NewQuit=true,
	    NewResult=ServiceInitArgs
    end,
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,Vsn,NewNumTries,NewQuit,NewResult}]),
    deploy_service(ServiceId,Vsn,Capability,Zone,NewNumTries,Delay,InitArgs,NewQuit,NewResult).			  

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop_application(ServiceList,InitArgs)->
    StoppedServices=stop_service(ServiceList,InitArgs,[]),
    Reply=case lists:keymember(error,1,StoppedServices) of
	      true->
		  {error,[?MODULE,?LINE,StoppedServices]};
	      false->
		  {ok,StoppedServices}
	  end,
    Reply.

stop_service([],_InitArgs,Acc)->
    Acc;
stop_service([ServiceInitArgs|T],InitArgs,Acc)->
    IpAddrNfvi=addr_mgr:init_args_public_ip(ServiceInitArgs),
    PortNfvi=addr_mgr:init_args_public_port(ServiceInitArgs),
    IpAddrService=addr_mgr:init_args_local_ip(ServiceInitArgs) ,
    PortService=addr_mgr:init_args_local_port(ServiceInitArgs), 

    case nfv_mgr_lib:stop_service(IpAddrNfvi,PortNfvi,IpAddrService,PortService,InitArgs) of
	{ok,stopped}->
	    NewAcc=[{ok,ServiceInitArgs}|Acc],
	    ok=if_dns:call("dns",dns,de_register,[ServiceInitArgs]);
	{error,Err}->
	    if_log:call(InitArgs,error,[?MODULE,?LINE,'failed to stop',ServiceInitArgs,Err]),
	    NewAcc=[{error,[ServiceInitArgs,Err]}|Acc]
    end,
    stop_service(T,InitArgs,NewAcc).
	

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_all_applications(ApplicationList)->
    
    ApplicationList.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_app_started(AppId,Vsn,StartedApps)->
    check_app_started(AppId,Vsn,StartedApps,{ok,not_started}).
check_app_started(_,_,[],Acc)->
    Acc;
check_app_started(_,_,_,{error,already_started})->
    {error,already_started};
check_app_started(AppIdStart,VsnStart,[{{AppId,Vsn},_InitArgs}|T],Acc)->
    case {AppIdStart,VsnStart}=={AppId,Vsn} of
	true->
	    NewAcc={error,already_started};
	false ->
	    NewAcc=Acc
    end,
    check_app_started(AppId,Vsn,T,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_app_exists(InstanceIdStop,ApplicationList)->
    check_app_exists(InstanceIdStop,ApplicationList,false).

check_app_exists(_,_,true)->
    true;
check_app_exists(_,[],Exists)->
    Exists;

check_app_exists(InstanceIdStop,[{InstanceId,_AppId,_Vsn,_StartedServices}|T],Exists)->
    case InstanceIdStop==InstanceId of
	true->
	    NewExists=true;
	false ->
	    NewExists=Exists
    end,
    check_app_exists(InstanceIdStop,T,NewExists).
    

%%-------------- MANAGE SERVICES 
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_service(ServiceId,Vsn,Capability,Zone,InitArgs) ->
 %   io:format(" ~p~n",[{?MODULE,?LINE,State#state.service_list}]),
    Reply=case if_dns:call("vim",vim,get_nfvi_candidates,[Capability,Zone]) of
	      {ok,NfviCandidates}->
		  {IpAddrNfvi,PortNfvi,LocalIpNfvi,LocalPortNfvi}=nfv_mgr_lib:schedule_nfvi(ServiceId,Vsn,NfviCandidates),
%		  io:format(" NfviCandidates ~p~n",[{?MODULE,?LINE,{IpAddrNfvi,PortNfvi,LocalIpNfvi,LocalPortNfvi}}]),
		  case tcp:call(IpAddrNfvi,PortNfvi,{nfvi,deploy_service,[IpAddrNfvi,PortNfvi,ServiceId,Vsn]}) of
		      {ok,[LocalIp,LocalPort]}->
%			  io:format(" nfvi,deploy_service ~p~n",[{?MODULE,?LINE,LocalIp,LocalPort}]),
			  NewService=[{public_ip,IpAddrNfvi},{public_port,PortNfvi},
				      {local_ip,LocalIp},{local_port,LocalPort},
				      {service_id,ServiceId},{vsn,Vsn}],
		      	%  if_log:call(State#state.init_args,ok,[?MODULE,?LINE,'Succesfully Started Service',NewService]),
			  {ok,NewService};
		      {error,Err}->
			  io:format(" Error ~p~n",[{?MODULE,?LINE,'error when starting Service',ServiceId,Vsn,Err}]),
			  if_log:call(InitArgs,error,[?MODULE,?LINE,'error when starting Service',ServiceId,Vsn,Err]),
			  {error,Err};
		      Err ->
			  io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
			  {error,Err}
		  end;
	      {error,no_availible_candidates}->
		  if_log:call(InitArgs,error,[?MODULE,?LINE,'no availible resources that matches requirements',ServiceId,Vsn,Capability,Zone]),
		  {error,[?MODULE,?LINE,'no availible resources that matches requirements',Capability,Zone]};
	      Err ->
		  io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

stop_service(IpAddrNfvi,PortNfvi,IpAddrService,PortService,InitArgs)->
    Reply=case if_dns:call("vim",vim,get_nfvi_addr,[IpAddrNfvi,PortNfvi]) of
	      {ok,LocalIpNfvi,LocalPortNfvi}->
		  case tcp:call(IpAddrNfvi,PortNfvi,LocalIpNfvi,LocalPortNfvi,{nfvi,remove_service,[IpAddrService,PortService]}) of
		      {ok,stopped}->
			  {ok,stopped};
		      {error,Err}->
			  if_log:call(InitArgs,error,[?MODULE,?LINE,Err,IpAddrNfvi,PortNfvi,IpAddrService,PortService]),
			  {error,[?MODULE,?LINE,IpAddrNfvi,PortNfvi,IpAddrService,PortService]};
		      Err->
			  if_log:call(InitArgs,error,[?MODULE,?LINE,Err]),
			  {error,[?MODULE,?LINE,Err]}
		  end;
	      {error,not_exist}->
		  if_log:call(InitArgs,error,[?MODULE,?LINE,'nfvi doesnt exists',IpAddrNfvi,PortNfvi]),
		  {error,[?MODULE,?LINE,'nfvi doesnt exists',IpAddrNfvi,PortNfvi]};
	      Err->
		  if_log:call(InitArgs,error,[?MODULE,?LINE,Err]),
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

schedule_nfvi(_ServiceId,_Vsn,NfviCandidates)->
    [{{public_ip,PIp},{public_port,PPort},{local_ip,LIp},
      {local_port,LPort}}|_]=NfviCandidates,
    {PIp,PPort,LIp,LPort}.

check_service_exists(_IpAddrNfvi,_PortNfvi,_IpAddrService,_PortService)->
    {ok,exists}.   %Glurk - shall be updated
