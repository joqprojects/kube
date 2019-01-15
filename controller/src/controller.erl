%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%% Created : 10 dec 2012
%%% VNFM:
%%% The VNFM is a key component of the NFV-MANO that helps standardize the functions 
%%% of virtual networking and increases interoperability of software-defined networking elements. §
%%% The VNFM is responsible for the lifecycle management of VNFs under the control of the NFVO, 
%%% which it achieves by instructing the VIM. 
%%% VNFM operations include:
%%% --Instantiation of VNFs
%%% --Scaling of VNFs
%%% --Updating and/or upgrading VNFs
%%% --Termination of VNFs
%%% All VNF instances are assumed to have an associated VNF manager.
%%% A VNFM may be assigned the management of a single VNF instance or multiple VNF instances. 
%%% The managed VNFs can be of the same or different types. 
%%% VNF manager functions are assumed to be generic and can be applied to any VNF.
%%% VNFM’s Importance
%%% VNFs are critical to realizing the business benefits outlined by the NFV architecture. 
%%% They deliver the actual network functions that create value. But they aren’t autonomous. 
%%% They require VNFMs. VNFMs are critical for scaling, changing operations, adding new resources, 
%%% and communicating the states of VNFs to other functional blocks in the NFV-MANO architecture.
%%% An example of the importance of a VNFM is key performance indicator (KPI) monitoring.
%%%  During the lifecycle of a VNF, the VNF management functions may monitor defined KPIs of a VNF. 
%%% The management functions can use this information for scaling operations.
%%% Ultimately, the VNFM maintains the virtualized resources that support the VNF functionality
%%% without interfering with the logical functions performed by the VNFs. 
%%% The services provided by the VNFM can be employed by authenticated and properly authorized 
%%% NFV management and orchestration functions (e.g., functions that manage network services).
%%% 
%%%
%%% What is an NFV Orchestration?
%%% Network functions virtualization (NFV) Orchestration (or NFV Orchestration) is used 
%%% to coordinate the resources and networks needed to set up cloud-based services and applications.
%%% This process uses a variety of virtualization software and industry standard hardware.
%%% Cloud service providers (CSPs) or global telecom operators use NFV orchestration to quickly
%%% deploy services, or virtual network functions (VNFs), using cloud software rather than specialized hardware networks.
%%%
%%% -------------------------------------------------------------------
-module(controller).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/data.hrl").
-include("kube/include/kubelet_data.hrl").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
%-record(state, {applications,services,cluster_status}).
-record(state,{dns_info,dns_list,node_list,application_list}).
%%---------------------------------------------------------------------

-export([campaign/0,
	 add/2,remove/2,
%	 start_application/2,stop_application/2,% get_services/0,
	 get_all_applications/0,get_all_services/0,
	 all_nodes/0,
	 dns_register/1,de_dns_register/1,
	 node_register/1,de_node_register/1
	]).

-export([start/0,
	 stop/0,
	 heart_beat/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).
% Test


%% end test
   
all_nodes()->
    gen_server:call(?MODULE, {all_nodes},infinity).

get_all_applications()->
    gen_server:call(?MODULE, {get_all_applications},infinity).

get_all_services()->
    gen_server:call(?MODULE, {get_all_services},infinity).
    
%%-----------------------------------------------------------------------
    
add(AppId,Vsn)->
    gen_server:call(?MODULE, {add,AppId,Vsn},infinity).  
remove(AppId,Vsn)->
    gen_server:call(?MODULE, {remove,AppId,Vsn},infinity). 
%%-----------------------------------------------------------------------
campaign()->
    gen_server:cast(?MODULE, {campaign}).
node_register(KubeletInfo)->
    gen_server:cast(?MODULE, {node_register,KubeletInfo}).
de_node_register(KubeletInfo)->
    gen_server:cast(?MODULE, {de_node_register,KubeletInfo}).

dns_register(DnsInfo)->
    gen_server:cast(?MODULE, {dns_register,DnsInfo}).
de_dns_register(DnsInfo)->
    gen_server:cast(?MODULE, {de_dns_register,DnsInfo}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%
%% --------------------------------------------------------------------
init([]) ->
    {ok,MyIp}=application:get_env(ip_addr),
    {ok,Port}=application:get_env(port),
    {ok,ServiceId}=application:get_env(service_id),
    {ok,Vsn}=application:get_env(vsn),
    MyDnsInfo=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = ServiceId,
			vsn = Vsn,
			ip_addr=MyIp,
			port=Port
		       },
    spawn(fun()-> local_heart_beat(?HEARTBEAT_INTERVAL) end),     
    io:format("Started Service  ~p~n",[{?MODULE}]),
    {ok, #state{dns_info=MyDnsInfo,dns_list=[],node_list=[],application_list=[]}}.  
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({add,AppId,Vsn}, _From, State) ->
    Reply=case lists:keyfind({AppId,Vsn},1,State#state.application_list) of
	      false->
		  case if_dns:call("catalog",catalog,read,[AppId,Vsn]) of
		      {error,Err}->
			  NewState=State,
			  {error,[?MODULE,?LINE,AppId,Vsn,Err]};
		      {ok,_,JoscaInfo}->
			  NewAppList=[{{AppId,Vsn},JoscaInfo}|State#state.application_list],
			  NewState=State#state{application_list=NewAppList},
			  ok
		  end;
	      _->
		  NewState=State,
		  {error,[?MODULE,?LINE,'already exists',AppId,Vsn]}
	  end,
    {reply, Reply,NewState};

handle_call({remove,AppId,Vsn}, _From, State)->
    Reply=case lists:keyfind({AppId,Vsn},1,State#state.application_list) of
	      false ->
		  NewState=State,
		  {error,[?MODULE,?LINE,'eexists',AppId,Vsn]};
	      {{AppId,Vsn},JoscaInfo}->
		  NewAppList=lists:keydelete({AppId,Vsn},1,State#state.application_list),
		%  io:format("NewAppList ~p~n",[{time(),NewAppList,?MODULE,?LINE}]),
		  AllServices=rpc:call(node(),controller_lib,needed_services,[NewAppList]),
		%  io:format("AllServices ~p~n",[{time(),AllServices,?MODULE,?LINE}]),
		  AppIdServices=rpc:call(node(),controller_lib,needed_services,[[{{AppId,Vsn},JoscaInfo}]]),
		%  io:format("AppIdServices ~p~n",[{time(),AppIdServices,?MODULE,?LINE}]),
		  ServicesToStop=[{ServiceId,Vsn}||{ServiceId,Vsn}<-AppIdServices,
						   false==lists:member({ServiceId,Vsn},AllServices)],
		 % io:format("ServicesToStop ~p~n",[{time(),ServicesToStop,?MODULE,?LINE}]),
		  rpc:call(node(),controller_lib,stop_services,[ServicesToStop,State#state.dns_list]),
		  NewState=State#state{application_list=NewAppList},
		  ok
	  end,
    {reply, Reply,NewState};

handle_call({get_all_applications},_From, State) ->
    Reply=State#state.application_list,
    {reply, Reply, State};

handle_call({get_all_services},_From, State) ->
    Reply=State#state.dns_list,
    {reply, Reply, State};

handle_call({all_nodes},_From, State) ->
    Reply=State#state.node_list,
    {reply, Reply, State};


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

handle_call({heart_beat}, _From, State) ->
    if_dns:call("dns",dns,dns_register,[State#state.dns_info]),
    rpc:call(node(),kubelet,dns_register,[State#state.dns_info]),
    Now=erlang:now(),
    NewDnsList=[DnsInfo||DnsInfo<-State#state.dns_list,
		      (timer:now_diff(Now,DnsInfo#dns_info.time_stamp)/1000)<?INACITIVITY_TIMEOUT],
   
    NewNodeList=[KubeletInfo||KubeletInfo<-State#state.node_list,
		      (timer:now_diff(Now,KubeletInfo#kubelet_info.time_stamp)/1000)<?INACITIVITY_TIMEOUT],
    
    NewState=State#state{dns_list=NewDnsList,node_list=NewNodeList},
  %  io:format(" ~p~n",[{time(),'before campaign',?MODULE,?LINE}]),
    rpc:call(node(),controller,campaign,[]),
   % io:format(" ~p~n",[{time(),'after campaign',?MODULE,?LINE}]),   
Reply=ok,
   {reply, Reply,NewState};
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    %if_log:call(State#state.dns_info,error,[?MODULE,?LINE,'unmatched signal',Request]),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({campaign}, State) ->
    NeededServices=rpc:call(node(),controller_lib,needed_services,[State#state.application_list]),
%    io:format("NeededServices ~p~n",[{?MODULE,?LINE,NeededServices}]),
    MissingServices=rpc:call(node(),controller_lib,missing_services,[NeededServices,State#state.dns_list]),
    case MissingServices of
	[]->
	    io:format("System is in preferred state  ~p~n",[{date(),time(),MissingServices}]);
	MissingServices->
	    io:format("MissingServices ~p~n",[{date(),time(),MissingServices}])
    end,
    rpc:call(node(),controller_lib,start_services,[MissingServices,State#state.node_list]),
    {noreply, State};


handle_cast({dns_register,DnsInfo}, State) ->
    Service=DnsInfo#dns_info.service_id,
    Ip=DnsInfo#dns_info.ip_addr,
    P=DnsInfo#dns_info.port,
 %   io:format("~p~n",[{time(),Service,Ip,P,?MODULE,?LINE}]),
    TimeStamp=erlang:now(),
    NewDnsInfo=DnsInfo#dns_info{time_stamp=TimeStamp},
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    
    X1=[X||X<-State#state.dns_list,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList=[NewDnsInfo|X1],
    NewState=State#state{dns_list=NewDnsList},
    {noreply, NewState};

handle_cast({de_dns_register,DnsInfo}, State) ->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    NewDnsList=[X||X<-State#state.dns_list,
		   false==({IpAddr,Port,ServiceId,Vsn}=={X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    
    NewState=State#state{dns_list=NewDnsList},
    {noreply, NewState};

handle_cast({node_register,KubeletInfo}, State) ->
    Service=KubeletInfo#kubelet_info.service_id,
    Ip=KubeletInfo#kubelet_info.ip_addr,
    P=KubeletInfo#kubelet_info.port,
  %  io:format("~p~n",[{time(),Service,Ip,P,?MODULE,?LINE}]),

    TimeStamp=erlang:now(),
    NewKubeletInfo=KubeletInfo#kubelet_info{time_stamp=TimeStamp},
    #kubelet_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn,
		  max_workers=_MaxWorkers,zone=_Zone,capabilities=_Capabilities
		 }=KubeletInfo,
%    io:format("~p~n",[{?MODULE,?LINE,State#state.node_list}]),
    X1=[X||X<-State#state.node_list,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#kubelet_info.ip_addr,X#kubelet_info.port,X#kubelet_info.service_id,X#kubelet_info.vsn})],
    NewKubeletList=[NewKubeletInfo|X1],
   % io:format("~p~n",[{?MODULE,?LINE,NewKubeletList}]),
    NewState=State#state{node_list=NewKubeletList},
    {noreply, NewState};

handle_cast({de_node_register,KubeletInfo}, State) ->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=KubeletInfo,
    NewKubeletList=[X||X<-State#state.node_list,
		       false==({IpAddr,Port,ServiceId,Vsn}==
				   {X#kubelet_info.ip_addr,X#kubelet_info.port,X#kubelet_info.service_id,X#kubelet_info.vsn})],
    
    NewState=State#state{node_list=NewKubeletList},
    {noreply, NewState};

handle_cast(Msg, State) ->
    if_log:call(State#state.dns_info,error,[?MODULE,?LINE,'unmatched signal',Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info(Info, State) ->
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
    if_log:call(State#state.dns_info,error,[?MODULE,?LINE,'unmatched signal',Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
local_heart_beat(Interval)->
%    io:format(" ~p~n",[{?MODULE,?LINE}]),
    timer:sleep(50),
    ?MODULE:heart_beat(),
    timer:sleep(Interval),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
