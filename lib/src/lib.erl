%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/data.hrl").
-include("kube/include/dns_data.hrl").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{dns_info}).

%% --------------------------------------------------------------------
%% Exported functions
%% --------------------------------------------------------------------

%% dns functions 
-export([heart_beat/0
	]
      ).


-export([start/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------


%%-----------------------------------------------------------------------


heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).
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
%
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
    {ok, #state{dns_info=MyDnsInfo}}.
    
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

handle_call({heart_beat}, _From, State) ->
    DnsInfo=State#state.dns_info,
    if_dns:call("dns",dns,dns_register,[DnsInfo]),
    rpc:cast(node(),kubelet,dns_register,[DnsInfo]),
   % if_dns:call("contoller",controller,controller_register,[DnsInfo]),
    Reply=ok,
   {reply, Reply, State};
    
handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    DnsInfo=State#state.dns_info,
    if_log:call(DnsInfo,error,[?MODULE,?LINE,'unmatched_signal',Request,From]),	
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
    DnsInfo=State#state.dns_info,
    if_log:call(DnsInfo,error,[?MODULE,?LINE,'unmatched_signal',Msg]),	
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
    ?MODULE:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
