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
-module(dns).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/data.hrl").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Exported functions
%% --------------------------------------------------------------------

%% dns functions 
-export([get_instances/2,get_instances/1,
	 get_all_instances/0,
	 register/1,
	 de_register/1,
	 heart_beat/0
	]
      ).


-export([start/1,
	 stop/0,
	 app_start/6
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
	{
	  init_args,
	  dns_list
	}).
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


%%-----------------------------------------------------------------------
get_instances(ServiceStr)->
    gen_server:call(?MODULE, {get_instances,ServiceStr},infinity).

get_instances(ServiceStr,VsnStr)->
    gen_server:call(?MODULE, {get_instances,ServiceStr,VsnStr},infinity).
    
get_all_instances()->
    gen_server:call(?MODULE, {get_all_instances},infinity).

heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).
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
%
%% --------------------------------------------------------------------
init([InitArgs]) ->
    ServicePort=addr_mgr:init_args_local_port(InitArgs),
    {ok, LSock}=gen_tcp:listen(ServicePort,?SERVER_SETUP),
    spawn(fun()-> tcp:par_connect(LSock) end),
    spawn(fun()-> local_heart_beat(?HEARTBEAT_INTERVAL) end), 
    {ok, #state{dns_list=[]}}.
    
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

handle_call({get_all_instances},_From, State) ->
    DnsList=State#state.dns_list,
    Reply=dns_lib:get_all_instances(DnsList),
    {reply, Reply, State};


handle_call({get_instances,WantedServiceStr},_From, State) ->
    Reply=rpc:call(node(),dns_lib,get_instances,[WantedServiceStr,State#state.dns_list]),
    {reply, Reply, State};

handle_call({get_instances,WantedServiceStr,WantedVsnStr},_From, State) ->
    Reply=rpc:call(node(),dns_lib,get_instances,[WantedServiceStr,WantedVsnStr,State#state.dns_list]),
    {reply, Reply, State};

handle_call({heart_beat}, _From, State) ->
    DnsList=State#state.dns_list,
    Now=erlang:now(),
    NewDnsList=[{TimeStamp,InitArgs}||{TimeStamp,InitArgs}<-DnsList,
		      (timer:now_diff(Now,TimeStamp)/1000)<?INACITIVITY_TIMEOUT],
    NewState=State#state{dns_list=NewDnsList},
    Reply=ok,
   {reply, Reply, NewState};
    
handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    InitArgs=State#state.init_args,
    DnsList=State#state.dns_list,
    dns_lib:local_log_call(InitArgs,error,[?MODULE,?LINE,'unmatched_signal',Request,From],DnsList),	
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({register,InitArgs}, State) ->
   % io:format("~p~n",[{?MODULE,?LINE,register,InitArgs}]),
    DnsList=State#state.dns_list,
    NewDnsList=dns_lib:register(InitArgs,DnsList),
    NewState=State#state{dns_list=NewDnsList},
    {noreply, NewState};

handle_cast({de_register,InitArgs}, State) ->
%    io:format("~p~n",[{?MODULE,?LINE,de_register,InitArgs}]),
    DnsList=State#state.dns_list,
    NewDnsList=dns_lib:de_register(InitArgs,DnsList),
    NewState=State#state{dns_list=NewDnsList},
    {noreply, NewState};

handle_cast(Msg, State) ->
    InitArgs=State#state.init_args,
    DnsList=State#state.dns_list,
    dns_lib:local_log_call(InitArgs,error,[?MODULE,?LINE,'unmatched_signal',Msg],DnsList),	
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
  %  io:format(" ~p~n",[{?MODULE,?LINE}]),
    timer:sleep(Interval),
    dns:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%par_connect(LSock)->
 %  case gen_tcp:accept(LSock) of
%	{ok,Socket}->
%	    spawn(fun()-> par_connect(LSock) end),
%	    loop(Socket);
%	Err ->
%	    Err
 %   end.
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
%loop(Socket)->
 %   receive
%	{tcp, Socket, Bin} ->
%	    R=case binary_to_term(Bin) of
%		  [M,F,A]->
%		      case rpc:call(node(),erlang,apply,[M,F,A],?CALL_TIMEOUT) of
%			  {badrpc,Err}->
%			      {badrpc,Err};
%			  Result->
%			      Result
%		      end;
%		  Err->
%		      {error,[?MODULE,?LINE,'unmatched signal',Err]}
%	      end,
%	    gen_tcp:send(Socket, term_to_binary(R)),
%	    loop(Socket);
%	{tcp_closed, Socket} ->
%	    tcp_closed   
 %   end.
