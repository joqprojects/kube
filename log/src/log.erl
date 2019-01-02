%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%% {event,[?MODULE,?LINE,info]}        % Normal event information
%%% {notification,[?MODULE,?LINE,info]} % Strange behaviour ex unmatched signal
%%% {error,[?MODULE,?LINE,info]}        % Execution error
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(log).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/data.hrl").
%% --------------------------------------------------------------------
-define(MAX_EVENTS,10).
%-define(CALL_TIMEOUT,120*1000).

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
% -record ??

-export([add_event/1,print_event/2,
	 read_events/1,
	 heart_beat/0
	]).

-export([start/1,
	 stop/0,
	 app_start/6
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {max_events,events,init_args,event_num}).
%% ====================================================================
%% External functions
%% ====================================================================
%% Gen server functions
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

start(InitArgs)-> gen_server:start_link({local, ?MODULE}, ?MODULE, [InitArgs], []).

stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
read_events(NumEvents)->
    gen_server:call(?MODULE, {read_events,NumEvents},infinity).

heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).

%%-----------------------------------------------------------------------

add_event(Event)->
    gen_server:cast(?MODULE, {add_event,Event}).  
print_event(Type,Info)->
    gen_server:cast(?MODULE, {print_event,Type,Info}).  
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

    Event=lists:append(InitArgs,[{event_type,ok},{event_info,[?MODULE,?LINE,'service started',?MODULE]}]),
    Events=log_lib:add_event(Event,?MAX_EVENTS,[]),
       
    {ok, #state{init_args=InitArgs,events=Events,event_num=0}}.    
    
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

handle_call({read_events,NumEvents}, _From, State) ->
    Reply=lists:sublist(State#state.events,NumEvents),
    {reply, Reply, State};

handle_call({heart_beat}, _From, State) ->
    InitArgs=State#state.init_args,
    Reply=if_dns:call("dns",dns,register,[InitArgs]),
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

%(dbase,IpAddrDbase,PortDbase)

handle_cast({print_event,Type, Info},State) ->
 
  io:format("-------------- Print Event ~p, ~p -------------------- ~n",[date(),time()]),
   % io:format(" ~p~n ",[date()]),
 
    io:format("~p ~p ~n ",[Type, Info]),
     {noreply, State};

handle_cast({add_event,Event},State) ->
    {event_info,Info}=lists:keyfind(event_info,1,Event),
    {event_type,Type}=lists:keyfind(event_type,1,Event),
    {public_ip,Ip}=lists:keyfind(public_ip,1,Event),
    {public_port,Port}=lists:keyfind(public_port,1,Event),
    {service_id,Id}=lists:keyfind(service_id,1,Event),
  
  io:format("-------------- New Event ~p, ~p -------------------- ~n",[date(),time()]),
    io:format(" ~p~n ",[date()]),
    NewEventNum=State#state.event_num+1,
    io:format("~p >>>> ~p ~p : ~p:  ~p ~p ~n~n",[NewEventNum,Type, Info,Id, Port, Ip]),
    
    Events=State#state.events,
    NewEvents=log_lib:add_event(Event,?MAX_EVENTS,Events),
    
    NewState=State#state{events=NewEvents, event_num=NewEventNum},
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
    log:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
