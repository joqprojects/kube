%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%% Created : 10 dec 2012
%%% The NFV architecture comprises major components –
%%  including virtualized network functions (VNFs),
%%%  NFVmanagement and orchestration (MANO), and NFV Infrastructure (NFVI) – 
%%%  that work with traditional network components like OSS/BSS.
%%%  NFVI is composed of NFV infrastructure points-of-presence (NFVI-PoPs) 
%%%  which are where the VNFs, including resources for computation, storage, 
%%%  and networking, are deployed by a network operator. 
%%%  NFVI networks interconnect the computing and storage resources contained in an NFVI-PoP. 
%%%  This may include specific switching and routing devices to allow external connectivity.
%%%
%%% -------------------------------------------------------------------
-module(nfvi).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
-include("services/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
% -record ??

-export([get_zone/0,get_capabilities/0,
	 deploy_service/4,remove_service/2

	]).

-export([start/1,
	 stop/0,
	 app_start/6,
	 heart_beat/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {init_args,zone,capabilities,vm_list,availible_ports,local_ip}).
%% ====================================================================
%% External functions
%% ====================================================================
app_start(PublicIp,PublicPort,LocalIp,LocalPort,Service,Vsn)->
    ok=application:set_env(?MODULE,public_ip,PublicIp),
    ok=application:set_env(?MODULE,public_port,PublicPort),
    ok=application:set_env(?MODULE,local_ip,LocalIp),
    ok=application:set_env(?MODULE,local_port,LocalPort),
    ok=application:set_env(?MODULE,service,Service),
    ok=application:set_env(?MODULE,vsn,Vsn),
    R1=application:load(?MODULE),
    R2=application:start(?MODULE),
    {R1,R2}.


%% Gen server functions

start(InitArgs)-> gen_server:start_link({local, ?MODULE}, ?MODULE, [InitArgs], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).


get_zone()->
    gen_server:call(?MODULE, {get_zone},infinity).
get_capabilities()->
    gen_server:call(?MODULE, {get_capabilities},infinity).

deploy_service(Ip,Port,ServiceId,Vsn)->
    gen_server:call(?MODULE, {deploy_service,Ip,Port,ServiceId,Vsn},infinity).    

remove_service(IpAddrService,PortService)->
    gen_server:call(?MODULE, {remove_service,IpAddrService,PortService},infinity).    

%%-----------------------------------------------------------------------


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
% dict:fetch(oam_rpi3,D1).
% [{brd_ip_port,"80.216.90.159"},
% {port,6001},
% {worker_ip_port,"80.216.90.159"},
%  {port,6002}]
%
%% --------------------------------------------------------------------
init([InitArgs]) ->
    {ok,L}=file:consult("nfvi.config"),
    {zone,Zone}=lists:keyfind(zone,1,L),
    {capabilities,CapaList}=lists:keyfind(capabilities,1,L),
    PublicPort=addr_mgr:init_args_local_port(InitArgs),
    LocalIp=addr_mgr:init_args_local_ip(InitArgs),
    {ok, LSock}=gen_tcp:listen(PublicPort,?SERVER_SETUP),
    spawn(fun()-> tcp:par_connect(LSock) end),
    timer:sleep(100),   
 % upnpc update - dont have to do portforwarding: check if already allocated if remove
    case lists:keyfind(brd_id,1,L) of
	{brd_id,"master"}->
	    % start dns,log, vnf_mgr, vim 
	    {ok,ok}=dns:app_start(?DNS_PUBLIC_IP,PublicPort,
				      ?DNS_LOCAL_IP,?DNS_LOCAL_PORT,"dns","1.0.0"),
	    timer:sleep(100),
	    {ok,ok}=log:app_start(?LOG_PUBLIC_IP,PublicPort,
				  ?LOG_LOCAL_IP,?LOG_LOCAL_PORT,"log","1.0.0"),
	    timer:sleep(100),
	    {ok,ok}=nfv_mgr:app_start(?NFV_MGR_PUBLIC_IP,PublicPort,
				      ?NFV_MGR_LOCAL_IP,?NFV_MGR_LOCAL_PORT,"nfv_mgr","1.0.0"),
	    timer:sleep(100),
	    {ok,ok}=vim:app_start(?VIM_PUBLIC_IP,PublicPort,
				      ?VIM_LOCAL_IP,?VIM_LOCAL_PORT,"vim","1.0.0"),
	    timer:sleep(100),
	    {ok,ok}=repo:app_start(?REPO_PUBLIC_IP,PublicPort,
				      ?REPO_LOCAL_IP,?REPO_LOCAL_PORT,"repo","1.0.0"),
	    timer:sleep(100),	    
	    AvailiblePorts=[], % Master shall not run other services
	    VmList=[], % Master shall not run other services
	    ok;
	_->
	    {ports,Start,Stop}=lists:keyfind(ports,1,L),
	    AvailiblePorts=nfvi_lib:create_availible_ports(Start,Stop),
	    VmList=[],
	    % Set the nfvi into a inital state -> Remove all loaded and started services 
	    {ok,DirFiles}=file:list_dir("."),
	    L1=[rpc:call(node(),erlang,list_to_integer,[Dir])||Dir<-DirFiles],  
	    [nfvi_lib:clean_up(X)||X<-L1,is_integer(X)==true,X>Start-1,X<Stop+1], 
	    if_dns:call("vim",vim,register,[[{init_args,InitArgs},
					     {zone,nfvi_lib:zone()},
					     {capabilities,nfvi_lib:capabilities()}]]),	
	    spawn(fun()->local_heart_beat(?HEARTBEAT_INTERVAL) end),
	    ok
    end,
    if_log:call(InitArgs,event,[?MODULE,?LINE,'service started',?MODULE]),
    {ok, #state{init_args=InitArgs,zone=Zone,
		capabilities=CapaList,availible_ports=AvailiblePorts,vm_list=VmList,
		local_ip=LocalIp}}.  
    
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
handle_call({deploy_service,PublicIp,PublicPort,ServiceId,Vsn}, _From, State) ->
    Reply=case nfvi_lib:allocate_port(State#state.availible_ports) of
	      {ok,PortService,NewAvailiblePorts}->
		 % io:format("  nfvi_lib:allocate_port ~p~n",[{?MODULE,?LINE,PortService}]),
		  case nfvi_lib:load_image(ServiceId,Vsn,PortService) of
		      {ok,image_loaded}->
		%	  io:format("nfvi_lib:load_image ~p~n",[{?MODULE,?LINE,ok}]),
			  case nfvi_lib:start_vm(PortService) of
			      {ok,NodeService}->
		%		  io:format("nfvi_lib:start_vm ~p~n",[{?MODULE,?LINE,ok,NodeService}]),
				  LocalIp=State#state.local_ip,  %hard coded
				  case nfvi_lib:start_service(NodeService,PublicIp,PublicPort,LocalIp,PortService,
							      ServiceId,Vsn) of
				      ok->
		%			  io:format("nfvi_lib:start_service ~p~n",[{?MODULE,?LINE,ok}]),
					  NewVm={{addr,LocalIp,PortService},NodeService},
					  NewVmList=[NewVm|State#state.vm_list],
					  NewState=State#state{vm_list=NewVmList,availible_ports=NewAvailiblePorts},
				%	  if_log:call(State#state.init_args,ok,[?MODULE,?LINE,'SERVICE started',ServiceId,Vsn,PublicIp,PublicPort,LocalIp,PortService]),
					  {ok,[LocalIp,PortService]};
				      {error,Err}->
					  io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
					  nfvi_lib:clean_up(NodeService,PortService), %? glurk
					  NewState=State,
					  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to start SERVICE',Err,ServiceId,Vsn,PublicIp,PublicPort,LocalIp,PortService]),
					  {error,[?MODULE,?LINE,'failed to start SERVICE',Err,ServiceId,Vsn,PublicIp,PublicPort,LocalIp,PortService]};
				      Err ->
					  io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
					  nfvi_lib:clean_up(PortService),
					  NewState=State,
					  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to start SERVICE',Err,ServiceId,Vsn,PublicIp,PublicPort,PortService]),
					  {error,[?MODULE,?LINE,'failed to start SERVICE',Err,ServiceId,Vsn,PublicIp,PublicPort,PortService]}  
				  end;
			      
			      {error,Err}->
				  io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
				  nfvi_lib:clean_up(PortService),
				  NewState=State,
				  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to start SERVICE',Err,ServiceId,Vsn,PublicIp,PublicPort,PortService]),
				  {error,[?MODULE,?LINE,'failed to start SERVICE',Err,ServiceId,Vsn,PublicIp,PublicPort,PortService]}
			  end;
		      {error,Err}->
			  io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
			  nfvi_lib:clean_up(PortService), %? glurk
			  NewState=State,
			  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'couldnt load image',Err,ServiceId,Vsn,PublicIp,PublicPort,PortService]),
			  {error,[?MODULE,?LINE,Err,ServiceId,Vsn,PublicIp,PublicPort,PortService]}
		  end;
	      {error,Err}->
		  io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
		  NewState=State,
		  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'no ports availible',ServiceId,Vsn,Err]),
		  {error,[?MODULE,?LINE,ServiceId,Vsn,Err]}
	  end,
    {reply, Reply, NewState};



handle_call({remove_service,IpAddrService,PortService}, _From, State) ->
    nfvi_lib:clean_up(PortService),
    Reply=case lists:member(PortService,State#state.availible_ports) of
	      false-> % Port is in use
		  NewAvailiblePorts=lists:append(State#state.availible_ports,[PortService]),
		  case lists:keyfind({addr,IpAddrService,PortService},1,State#state.vm_list) of
		      []->
			  NewState=State#state{availible_ports=NewAvailiblePorts},
			  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'service doesnt exists',IpAddrService,PortService]),
			  {error,[?MODULE,?LINE,'service doesnt exists',IpAddrService,PortService]};   
		      {{addr,IpAddrService,PortService},Node}->
			  stopped=cmn:stop_node(Node),
			  os:cmd("rm -r "++integer_to_list(PortService)), 
			  NewVmList=rpc:call(node(),lists,keydelete,[{addr,IpAddrService,PortService},1,State#state.vm_list]),
			  NewState=State#state{vm_list=NewVmList,availible_ports=NewAvailiblePorts},
			  if_log:call(State#state.init_args,ok,[?MODULE,?LINE,'service stopped',IpAddrService,PortService]),
			  {ok,stopped}
		  end;
	      true-> % port is not in use
		  NewState=State,
		  if_log:call(State#state.init_args,error,[?MODULE,?LINE,'port is not in use',IpAddrService,PortService]),
		  {error,[?MODULE,?LINE,'service not exist',IpAddrService,PortService]}
	  end,
    {reply, Reply, NewState};

handle_call({get_zone}, _From, State) ->
    Reply=State#state.zone,
    {reply, Reply, State};
handle_call({get_capabilities}, _From, State) ->
    Reply=State#state.capabilities,
    {reply, Reply, State};

handle_call({heart_beat}, _From, State) ->
    InitArgs=State#state.init_args,
    R=if_dns:call("vim",vim,register,[[{init_args,InitArgs},
				  {zone,nfvi_lib:zone()},
				  {capabilities,nfvi_lib:capabilities()}]]),	
   % io:format(" **************************  -->> ~p~n",[{?MODULE,?LINE,R}]),
    Reply=R,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    Reply=glurk,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    if_log:call(State#state.init_args,error,[?MODULE,?LINE,'unmatched match cast',Msg]),
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info(Info, State) ->
    if_log:call(State#state.init_args,error,[?MODULE,?LINE,'unmatched info cast',Info]),
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
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
    timer:sleep(Interval),
    nfvi:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
