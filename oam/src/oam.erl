%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%   Simple service discovery built as with dns functionality
%%%
%%% Data model:
%%%  PublicIpAddr, PublicPort,  % Address to loadbalancer in DMZ  
%%%  LocalIpAddr,LocalPort      % Local lan adress ex 192.168.0.120 . 1000
%%%  ServiceId, Vsn             % "load_balancer","1.0.0", "brd_mgr","1.0.0", "tellstick",2.3.4"
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam).

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

-export([start_app/2,stop_app/2,deployed_apps/0,
	 read_events/1,
	 heart_beat/0
	]).

-export([start/1,
	 stop/0,
	 app_start/6
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {init_args,deployed_apps}).
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

start_app(Id,Vsn)->
    gen_server:call(?MODULE, {start_app,Id,Vsn},infinity).

stop_app(Id,Vsn)->
    gen_server:call(?MODULE, {stop_app,Id,Vsn},infinity).

deployed_apps()->
    gen_server:call(?MODULE, {deployed_apps},infinity).


read_events(NumEvents)->
    gen_server:call(?MODULE, {read_events,NumEvents},infinity).
%%-----------------------------------------------------------------------

heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).



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
  %  ServiceIpAddr=addr_mgr:init_args_ip(InitArgs),
    ServicePort=addr_mgr:init_args_local_port(InitArgs),
    {ok, LSock}=gen_tcp:listen(ServicePort,?SERVER_SETUP),
    spawn(fun()-> tcp:par_connect(LSock) end),
    if_dns:call("dns",dns,register,[InitArgs]),
    spawn(fun()->local_heart_beat(?HEARTBEAT_INTERVAL) end),
    if_log:call(InitArgs,event,[?MODULE,?LINE,'service started',?MODULE]),

    {ok, #state{init_args=InitArgs,deployed_apps=[]}}.     
    
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

handle_call({start_app,Id,Vsn}, _From, State)->
    Reply = case if_dns:call("nfv_mgr",nfv_mgr,start_application,[Id,Vsn]) of
		{ok,Id,Vsn}->
		    if_log:call(State#state.init_args,ok,[?MODULE,?LINE,'started application',Id,Vsn]),
		    {ok,['started',Id,Vsn]};
		{error,Failed}->
		    if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to start',Id,Vsn,Failed]),
		    {error,[?MODULE,?LINE,'failed to start',Id,Vsn,Failed]};
		Err ->
		    if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to start',Id,Vsn,Err]),
		    {error,[?MODULE,?LINE,'failed to start',Id,Vsn,Err]}
	    end,
    {reply, Reply, State};

handle_call({stop_app,Id,Vsn}, _From, State) ->
    Reply = case if_dns:call("nfv_mgr",nfv_mgr,stop_application,[Id,Vsn]) of
		ok->
		    if_log:call(State#state.init_args,ok,[?MODULE,?LINE,'Stopped Application ',Id,Vsn]),
		    {ok,[]};
		{error,Failed}->
		    if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to stop',Id,Vsn,Failed]),
		    {error,[?MODULE,?LINE,'failed to stop',Id,Vsn,Failed]};
		Err ->
		    if_log:call(State#state.init_args,error,[?MODULE,?LINE,'failed to stop',Id,Vsn,Err]),
		    {error,[?MODULE,?LINE,'failed to start',Id,Vsn,Err]}
	    end,
    {reply, Reply, State};

handle_call({deployed_apps}, _From, State) ->
    Reply=State#state.deployed_apps,
    {reply, Reply, State};


handle_call({read_events,_NumEvents}, _From, State) ->
    Reply=glurk,
    {reply, Reply, State};



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_call({heart_beat}, _From, State) ->
    InitArgs=State#state.init_args,
    if_dns:call("dns",dns,register,[InitArgs]),
    Reply=ok,
   {reply, Reply, State};
    


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
    oam:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
