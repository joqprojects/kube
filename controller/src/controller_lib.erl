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
stop_services([],DnsList)->
  %  io:format("ok  ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
    ok;
stop_services([{ServiceId,Vsn}|T],DnsList)->
 %   io:format("ServiceId,Vsn, Tail ~p~n",[{time(),?MODULE,?LINE,ServiceId,Vsn,'Tail',T}]),
 %   io:format("DnsList,Vsn ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
    ListWithIp=[{IpAddr,Port,ServiceId,Vsn}||#dns_info{service_id=X_Id,
							      vsn=X_Vsn,
							      ip_addr=IpAddr,
							      port=Port}<-DnsList,
						    {ServiceId,Vsn}=:={X_Id,X_Vsn}],
  %  io:format("ListWithIp,Vsn ~p~n",[{time(),?MODULE,?LINE,ListWithIp}]),

   R= [{IpAddr,Port,ServiceId,Vsn,rpc:cast(node(),tcp,call,[IpAddr,Port,{kubelet,stop_service,[ServiceId]}])}||{IpAddr,Port,ServiceId,Vsn}<-ListWithIp],
  %  io:format("result stop_service ~p~n",[{?MODULE,?LINE,R}]),

    stop_services(T,DnsList).
						  
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
