%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Pool =[{Pid1,Ref1,Module},{Pid2,Ref2,Module}]
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/trace_debug.hrl").
-include("kube/include/tcp.hrl").
-include("certificate/cert.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/kubelet_data.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Define
%% --------------------------------------------------------------------
%-define(DEFINE,define).
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {kubelet_info,lSock,max_workers,active_workers,workers,dns_list}).
%% --------------------------------------------------------------------


%% External exports -gen_server functions 

-export([start_service/2,
	 stop_service/1,
	 upgrade/2,
	 loaded_services/0,
	 my_ip/0,
	 dns_register/1,
	 de_dns_register/1,
	 start_kubelet/0,
	 heart_beat/0
	]).
-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================
start_kubelet()->
    R1=application:load(kubelet),
    R2=application:start(kubelet),
    io:format("kubelet start result ~p~n",[{R1,R2}]),
    {R1,R2}.
    
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

start_service(ServiceId,Vsn)-> 
    gen_server:call(?MODULE, {start_service,ServiceId,Vsn},infinity).
stop_service(ServiceId)-> 
    gen_server:call(?MODULE, {stop_service,ServiceId},infinity).

my_ip()-> 
    gen_server:call(?MODULE, {my_ip},infinity).
loaded_services()-> 
    gen_server:call(?MODULE, {loaded_services},infinity).

dns_register(DnsInfo)-> 
    gen_server:cast(?MODULE, {dns_register,DnsInfo}).
de_dns_register(DnsInfo)-> 
    gen_server:cast(?MODULE, {de_dns_register,DnsInfo}).

upgrade(ServiceId,Vsn)-> 
    gen_server:cast(?MODULE, {ServiceId,Vsn}).


heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).
%%-----------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
   % os:cmd("rm service_ebin/*"),
    {ok,InitialInfo}=file:consult("kubelet.config"),
    {ip_addr,NodeIp}=lists:keyfind(ip_addr,1,InitialInfo),
    {port,NodePort}=lists:keyfind(port,1,InitialInfo),
    {service_id,ServiceId}=lists:keyfind(service_id,1,InitialInfo),
    {vsn,Vsn}=lists:keyfind(vsn,1,InitialInfo),
    {max_workers,MaxWorkers}=lists:keyfind(max_workers,1,InitialInfo),
    {zone,Zone}=lists:keyfind(zone,1,InitialInfo),
    {capabilities,Capabilities}=lists:keyfind(capabilities,1,InitialInfo),
    {pre_load_apps,PreLoadApps}=lists:keyfind(pre_load_apps,1,InitialInfo),   
    KubeletInfo=#kubelet_info{time_stamp="not_initiaded_time_stamp",
			service_id = ServiceId,
			vsn = Vsn,
			ip_addr=NodeIp,
			port=NodePort,
			max_workers=MaxWorkers,
			zone=Zone,
			capabilities=Capabilities
		       },    

 %   
 %   rpc:call(node(),kubelet_lib,start_app,[dns,"1.0.0"]),
   io:format("NodeIp,NodePort Port  ~p~n",[{?MODULE,?LINE,PreLoadApps,NodeIp,NodePort}]),
    _Result=rpc:call(node(),kubelet_lib,load_start_pre_loaded_apps,[PreLoadApps,NodeIp,NodePort]),
  %  StartedApps=[{ServiceId_X,Vsn_X}||{ServiceId_X,Vsn_X,ok}<-Result],
     io:format("Port  ~p~n",[{?MODULE,?LINE,NodePort}]),
    {ok, LSock} = gen_tcp:listen(NodePort,?SERVER_SETUP),
    timer:sleep(100),
    Workers=init_workers(LSock,MaxWorkers,[]), % Glurk remove?

    %------ send info to controller
    
    SenderInfo=#sender_info{ip_addr=NodeIp,
			    port=NodePort,
			    module=?MODULE,line=?LINE},
    io:format("  ~p~n",[{?MODULE,?LINE}]),
    rpc:cast(node(),if_dns,call,["controller",controller,node_register,[KubeletInfo],SenderInfo]),
    spawn(fun()-> local_heart_beat(?HEARTBEAT_INTERVAL) end), 

    io:format("Started Service  ~p~n",[{?MODULE}]),
    {ok, #state{kubelet_info=KubeletInfo,
		lSock=LSock,max_workers=MaxWorkers,
		active_workers=0,workers=Workers,dns_list=[]}}.

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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({my_ip},_From, State) ->
    Reply="localhost", % Test only glurk
    {reply, Reply, State};

handle_call({loaded_services},_From, State) ->
    Reply=State#state.dns_list,
    {reply, Reply, State};

handle_call({start_service,ServiceId,Vsn},_From, State) ->
    DnsList=State#state.dns_list,
    #kubelet_info{ip_addr=MyIp,port=Port}=State#state.kubelet_info,   
    Reply= case [DnsInfo||DnsInfo<-DnsList,DnsInfo#dns_info.service_id =:=ServiceId] of
	      []->
		   case rpc:call(node(),kubelet_lib,load_start_app,[ServiceId,Vsn,MyIp,Port]) of
		       ok->
			   ok;
		       Err->
			   {error,[?MODULE,?LINE,Err,ServiceId,Vsn]}
		   end;
	       [DnsInfo]->
		   {error,[?MODULE,?LINE,'already loaded and started',ServiceId,Vsn,DnsInfo]}
	   end,
    
    {reply, Reply, State};

handle_call({stop_service,ServiceId}, _From, State) ->
    DnsList=State#state.dns_list,  
    Reply= case [DnsInfo||DnsInfo<-DnsList,DnsInfo#dns_info.service_id =:=ServiceId] of
	       []->
		   NewState=State,
		   {error,[?MODULE,?LINE,'eexists',ServiceId]};
	       [DnsInfo]->   
		   NewDnsList=lists:delete(DnsInfo,State#state.dns_list),
		   NewState=State#state{dns_list=NewDnsList},
		   case rpc:call(node(),kubelet_lib,stop_unload_app,[DnsInfo]) of
		       ok->
			   ok;
		       Err->
			   NewState=State,
			   {error,[?MODULE,?LINE,Err,ServiceId]}
		   end
	   end,

    {reply, Reply, NewState};


handle_call({heart_beat}, _From, State) ->
    DnsList=State#state.dns_list,
    Now=erlang:now(),
    NewDnsList=[DnsInfo||DnsInfo<-DnsList,
		      (timer:now_diff(Now,DnsInfo#dns_info.time_stamp)/1000)<?INACITIVITY_TIMEOUT],

    % Send services registration to Controller
    NodeInfo=State#state.kubelet_info,
    NodeIp=NodeInfo#kubelet_info.ip_addr,
    NodePort=NodeInfo#kubelet_info.port,
    SenderInfo=#sender_info{ip_addr=NodeIp,
			    port=NodePort,
			    module=?MODULE,line=?LINE},
    [rpc:cast(node(),if_dns,call,["controller",controller,dns_register,[DnsInfo],SenderInfo])||DnsInfo<-NewDnsList],
    % Register node
    rpc:cast(node(),if_dns,call,["controller",controller,node_register,[State#state.kubelet_info],SenderInfo]),
    NewState=State#state{dns_list=NewDnsList},
    Reply=ok,
   {reply, Reply, NewState};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({upgrade,_ServiceId,_Vsn}, State) ->
	    % get tar file from SW repositroy
	    % create service_info record
	    % create temp dir 
	    % untar files 
	    % read app file -get all modules
            % de_register the service and remove it from service list
	    % copy modules and app file to service_ebin dir
	    % start the service
	    % remove temp dir
	    % add service to service_list
	    % service shall push info to dns and kubectroller     % 
    
    
    {noreply, State};

handle_cast({dns_register,DnsInfo}, State) ->
  %  io:format("~p~n",[{?MODULE,?LINE,register,DnsInfo}]),
    DnsList=State#state.dns_list,
    NewDnsList=kubelet_lib:dns_register(DnsInfo,DnsList),
    NewState=State#state{dns_list=NewDnsList},
  %  io:format("~p~n",[{?MODULE,?LINE,register,NewState}]),
    {noreply, NewState};

handle_cast({de_dns_register,DnsInfo}, State) ->
%    io:format("~p~n",[{?MODULE,?LINE,de_register,InitArgs}]),
    DnsList=State#state.dns_list,
    NewDnsList=kubelet_lib:de_dns_register(DnsInfo,DnsList),
    NewState=State#state{dns_list=NewDnsList},
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({_Pid,active}, State) ->
    ActiveWorkers=State#state.active_workers+1,
    NewState = State#state{active_workers=ActiveWorkers},
%   io:format("active  ~p~n",[{?MODULE,?LINE,NewState}]), 
   {noreply, NewState};

handle_info({'DOWN',Ref,process,Pid,normal},  #state{lSock = LSock,active_workers=ActiveWorkers,
						     max_workers=Max,workers=Workers} = State) ->
    %  io:format("DOWN Pid,Ref an workers  ~p~n",[{?MODULE,?LINE,Pid,Ref,Workers}]), 
    W1=lists:delete({Pid,Ref},Workers),
    NewActiveWorkers=ActiveWorkers-1,
    if
	ActiveWorkers<Max-> %Accept new 
	    ParentPid=self(),
	    {NewPid,NewRef}=spawn_monitor(fun()->start_worker(ParentPid,LSock) end),
	    NewWorkerList=[{NewPid,NewRef}|W1];
	ActiveWorkers==Max->
	    NewWorkerList=W1
    end,

    NewState=State#state{active_workers=NewActiveWorkers,workers=NewWorkerList},
   % io:format("DOWN  ~p~n",[{?MODULE,?LINE,NewState}]),
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,Info}]),
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
local_heart_beat(Interval)->
  %  io:format(" ~p~n",[{?MODULE,?LINE}]),
    timer:sleep(Interval),
    ?MODULE:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

init_workers(_,0,Workers)->
    Workers;
init_workers(LSock,N,Workers)->
    ParentPid=self(),
    {Pid,Ref}=spawn_monitor(fun()->start_worker(ParentPid,LSock) end),
    NewWorkers=[{Pid,Ref}|Workers],
    init_workers(LSock,N-1,NewWorkers).
    



%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
start_worker(ParentPid,LSock)->
    case gen_tcp:accept(LSock) of 
	{error,closed}->
	    {error,closed};
	{ok, Socket}->	       
	    ParentPid!{self(),active},
	    receive
		{tcp, Socket, RawData}->
		    case binary_to_term(RawData) of
			[{M,F,A},?KEY_MSG]->
			    Reply=rpc:call(node(),M,F,A),
			    gen_tcp:send(Socket,term_to_binary(Reply));
			[{call,{M,F,A}},?KEY_MSG]->
			    Reply=rpc:call(node(),M,F,A),
			    gen_tcp:send(Socket,term_to_binary(Reply));
			[{cast,{M,F,A}},?KEY_MSG]->
			    io:format(" ~p~n",[{?MODULE,?LINE,{cast,{M,F,A}}}]),
			    A=rpc:cast(node(),M,F,A),
			    io:format("Error ~p~n",[{?MODULE,?LINE,A}]);
			Err->
			    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
		    end;
		{tcp_closed,Socket} ->
		    exit
	    end
    end.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

