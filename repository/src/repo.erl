%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : genserver repo with jle embedded extensions 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(repo).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/repository_data.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/dns.hrl").
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
% -define(DEFINE,define).
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Data structures
%% --------------------------------------------------------------------
-record(state, {dns_info,dbase_id}).

%% --------------------------------------------------------------------



%% External exports
-export([build_artifact/2,update_artifact/1,read_artifact/2,
	 heart_beat/0
	 %all_artifacts/1,
	 
	]).



-export([start/0,stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



build_artifact(ServiceId,EbinDir)->
    gen_server:call(?MODULE, {build_artifact,ServiceId,EbinDir},infinity).

update_artifact(Artifact)->
    gen_server:call(?MODULE, {update_artifact,Artifact},infinity).
read_artifact(ServiceId,Vsn)-> 
    gen_server:call(?MODULE, {read_artifact,ServiceId,Vsn},infinity).

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
%% --------------------------------------------------------------------
init([]) ->
    file:delete("storage/glurk.dbase"),   
    Type=set,
    DbaseId="storage/glurk.dbase",
    dbase_dets:create_dbase(Type,DbaseId),
%--- just for test'    
    init_glurk([{"adder","../../ebin/adder_100/ebin"},
		{"divider","../../ebin/divider_100/ebin"},
		{"subtract","../../ebin/subtract_100/ebin"},
		{"multi","../../ebin/multi_100/ebin"},
	        {"lib","../../ebin/lib/ebin"},
		{"dns","../../ebin/dns/ebin"},
		{"controller","../../ebin/controller/ebin"},
		{"catalog","../../ebin/catalog/ebin"}		
	       ]),
%----
    {ok,MyIp}=application:get_env(ip_addr),
    {ok,Port}=application:get_env(port),
    {ok,ServiceId}=application:get_env(service_id),
    {ok,Vsn}=application:get_env(vsn),
    DnsInfo=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = ServiceId,
			vsn = Vsn,
			ip_addr=MyIp,
			port=Port
		       },
    rpc:cast(node(),if_dns,call,["dns",dns,dns_register,[DnsInfo]]),
    rpc:cast(node(),if_dns,call,["controller",controller,dns_register,[DnsInfo]]),
    rpc:cast(node(),kubelet,dns_register,[DnsInfo]),
    spawn(fun()-> local_heart_beat(?HEARTBEAT_INTERVAL) end), 
    io:format("~p~n",[{?MODULE,'  started ', ?LINE}]),
    {ok, #state{dns_info=DnsInfo,dbase_id=DbaseId}}. 

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
handle_call({build_artifact,ServiceId,EbinDir}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,build_artifact,[ServiceId,EbinDir]),
    {reply, Reply, State};

handle_call({update_artifact,Artifact}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply=rpc:call(node(),repo_lib,update_artifact,[Artifact,DbaseId]),
    {reply, Reply, State};

handle_call({read_artifact,ServiceId,Vsn}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply=rpc:call(node(),repo_lib,read_artifact,[ServiceId,Vsn,DbaseId]),
    {reply, Reply, State};


handle_call({heart_beat}, _From, State) ->
    DnsInfo=State#state.dns_info,
     rpc:cast(node(),if_dns,call,["dns",dns,dns_register,[State#state.dns_info]]),
    rpc:cast(node(),kubelet,dns_register,[DnsInfo]),
   % if_dns:call("contoller",controller,controller_register,[DnsInfo]),
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
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
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
local_heart_beat(Interval)->
%    io:format(" ~p~n",[{?MODULE,?LINE}]),
    timer:sleep(10),
    ?MODULE:heart_beat(),
    timer:sleep(Interval),
    spawn(fun()-> local_heart_beat(Interval) end).


%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
init_glurk([])->
    ok;
init_glurk([{ServiceId,Ebin}|T])->
   io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,Ebin}]),
    case repo_lib:build_artifact(ServiceId,Ebin) of
	{ok,Artifact}->
	    case repo_lib:update_artifact(Artifact,"storage/glurk.dbase") of
		{ok,artifact_updated}->
		    ok;
		Err->
		    io:format("Error ~p~n",[{?MODULE,?LINE,ServiceId,Ebin,Err}])
	    end;  
	Err ->
	       io:format("Error ~p~n",[{?MODULE,?LINE,ServiceId,Ebin,Err}])
    end,
    init_glurk(T).
    
