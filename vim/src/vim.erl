%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%% Created : 10 dec 2012
%%% VIM’s Functions
%%% The VIM is responsible for managing the virtualized infrastructure 
%%% of an NFV-based solution. VIM operations include:
%%% It keeps an inventory of the allocation of virtual resources to physical resources.
%%% This allows the VIM to orchestrate the allocation, upgrade, release, and reclamation
%%% of NFVI resources and optimize their use.
%%% It supports the management of VNF forwarding graphs by organizing 
%%% virtual links, networks, subnets, and ports. 
%%% The VIM also manages security group policies to ensure access control.
%%% It manages a repository of NFVI hardware resources (compute, storage, networking)
%%% and software resources (hypervisors), along with the discovery of the 
%%% capabilities and features to optimize the use of such resources.
%%% The VIM performs other functions as well – such as collecting performance
%%% and fault information via notifications; managing software images 
%%% (add, delete, update, query, copy) as requested by other NFV-MANO functional blocks;
%%% and managing catalogues of virtualized resources that can be consumed from the NFVI.
%%% In summary, the VIM is the management glue between hardware and software in the NFV world.
%%% VIM’s Importance
%%% Virtual infrastructure managers are critical to realizing the business benefits 
%%% enabled by the NFV architecture. They coordinate the physical resources necessary to deliver network services.
%%% This is particularly visible for infrastructure-as-a-service (IaaS) providers. 
%%% The IaaS providers have to ensure that their servers, networks, and storage work smoothly with those onsite.
%%% They must ensure that resources can be dynamically allocated based on requirements, which is a key feature of cloud computing.
%%% VIMs address this need. Some consider it a morphing of a traditional OS, but it is not.
%%% It doesn’t work with a single node, but collects information from many machines simultaneously and
%%% uses that information for management functions. So, even if multiple machines are working in concert 
%%% at the NFVI layer, applications and users are ensured of a good, uniform experience.
%%%
%%% -------------------------------------------------------------------
-module(vim).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
-include("services/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------

-define(VIM_DBASE,"vim.dbase").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
% -record ??

-export([availible_resources/0,availible_ports/0,
	 get_all_instances/0,
	 get_nfvi_addr/2,
	 get_nfvi_candidates/2,
	 register/1,de_register/1,
	 %test
	 check_capa_zone/2
	]).

-export([start/1,
	 stop/0,
	 app_start/6,
	 heart_beat/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {init_args,nfvi_list,service_list}).
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



%% Test

check_capa_zone(Capability,Zone)->
    gen_server:call(?MODULE, {check_capa_zone,Capability,Zone},infinity).

%% end test
   
get_all_instances()->
    gen_server:call(?MODULE, {get_all_instances},infinity).


availible_resources()->
    gen_server:call(?MODULE, {availible_resources},infinity).

availible_ports()->
    gen_server:call(?MODULE, {availible_ports},infinity).


get_nfvi_addr(IpAddrNfvi,PortNfvi)->
    gen_server:call(?MODULE, {get_nfvi_addr,IpAddrNfvi,PortNfvi},infinity).

get_nfvi_candidates(NeededCapa,NeededZoneList)->
    gen_server:call(?MODULE, {get_nfvi_candidates,NeededCapa,NeededZoneList},infinity).

%%-----------------------------------------------------------------------

register(InitArgs)->
    gen_server:cast(?MODULE, {register,InitArgs}).  

de_register(InitArgs)->  
    gen_server:cast(?MODULE, {de_register,InitArgs}). 

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
    case dbase_dets:create_dbase(set,?VIM_DBASE) of
	{ok,dbase_already_exsist}->
	    [{nfvi_list,NfviList}]=dbase_dets:read(nfvi_list,?VIM_DBASE);
	{ok,dbase_created}->
	    NfviList=[],
	    {ok,object_created}=dbase_dets:create(nfvi_list,NfviList,?VIM_DBASE)
    end,
    ServicePort=addr_mgr:init_args_local_port(InitArgs),
    {ok, LSock}=gen_tcp:listen(ServicePort,?SERVER_SETUP),
    spawn(fun()-> tcp:par_connect(LSock) end),
    if_dns:call("dns",dns,register,[InitArgs]),
    spawn(fun()->local_heart_beat(?HEARTBEAT_INTERVAL) end),
    if_log:call(InitArgs,event,[?MODULE,?LINE,'service started',?MODULE]),
    
    {ok, #state{init_args=InitArgs,nfvi_list=NfviList,service_list=[]}}. 
    
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
%% test
handle_call({get_nfvi_candidates,NeededCapa,NeededZoneList}, _From, State) ->
    Reply=vim_lib:get_nfvi_candidates(NeededCapa,NeededZoneList,State#state.nfvi_list),
    {reply, Reply,State};


handle_call({check_capa_zone,Capability,Zone}, _From, State) ->
    Reply=vim_lib:check_capa_zone(Capability,Zone,State#state.nfvi_list),
    {reply, Reply,State};

%% end test
handle_call({get_nfvi_addr,IpAddrNfvi,PortNfvi}, _From, State) ->
     Reply=vim_lib: get_local_info(IpAddrNfvi,PortNfvi,State#state.nfvi_list),
    {reply, Reply,State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

handle_call({availible_resources}, _From, State) ->
    Reply=State#state.nfvi_list,
    {reply, Reply,State};


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

handle_call({heart_beat}, _From, State) ->
    InitArgsVim=State#state.init_args,
    if_dns:call("dns",dns,register,[InitArgsVim]),
    
    NfviList=State#state.nfvi_list,
    Now=erlang:now(),
    RemovedNfvi=[{TimeStamp,InitArgs}||{TimeStamp,InitArgs}<-NfviList,
		      (timer:now_diff(Now,TimeStamp)/1000)>=?INACITIVITY_TIMEOUT],
    
    case RemovedNfvi of
	[]->
	    ok;
	X->
	    io:format("Removed Nfvi ~p~n",[{?MODULE,?LINE,X}])
    end,
    NewNfviList=[{TimeStamp,InitArgs}||{TimeStamp,InitArgs}<-NfviList,
		      (timer:now_diff(Now,TimeStamp)/1000)<?INACITIVITY_TIMEOUT],
    NfviListNfvMgr=[InitArgs||{TimeStamp,InitArgs}<-NewNfviList],
    if_dns:call("nfv_mgr",nfv_mgr,updated_nfvi_list,[NfviListNfvMgr]),
			      
    {ok,object_updated}=dbase_dets:update(nfvi_list,NewNfviList,?VIM_DBASE),  
    NewState=State#state{nfvi_list=NewNfviList},


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
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({register,Info}, State) ->
  %  io:format("~p~n",[{?MODULE,?LINE,InitArgs}]),
    NfviList=State#state.nfvi_list,
    NewNfviList=vim_lib:register(Info,NfviList),

 %   NfviListNfvMgr=[InitArgs||{TimeStamp,InitArgs}<-NewNfviList],
 %   if_dns:call("nfv_mgr",nfv_mgr,updated_nfvi_list,[NfviListNfvMgr]),

    {ok,object_updated}=dbase_dets:update(nfvi_list,NewNfviList,?VIM_DBASE),  
    NewState=State#state{nfvi_list=NewNfviList},
    {noreply, NewState};

handle_cast({de_register,InitArgs}, State) ->
  %  io:format("~p~n",[{?MODULE,?LINE,de_register,InitArgs}]),
    NfviList=State#state.nfvi_list,
    NewNfviList=vim_lib:de_register(InitArgs,NfviList),

    NfviListNfvMgr=[InitArgs||{TimeStamp,InitArgs}<-NewNfviList],
    if_dns:call("nfv_mgr",nfv_mgr,updated_nfvi_list,[NfviListNfvMgr]),

   {ok,object_updated}=dbase_dets:update(nfvi_list,NewNfviList,?VIM_DBASE),  
    NewState=State#state{nfvi_list=NewNfviList},
    {noreply, NewState};

handle_cast(Msg, State) ->
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
    vim:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
