%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%% key value store
%% create,update,read,delete
%% Tar file:
%% key={tar_file,Service_id,Vsn}
%% value=[{tarfile,tarfilename,file.tar},{josca,file.josca},{app,file.app}]
%%  search:
%%  key={ServiceId,Vsn}
%% Value={tar_file,Service_id,Vsn}
%% key=ServiceId
%% Value=[{tar_file,Service_id,Vsn1},{tar_file,Service_id,Vsn2}
%%  Key={josca,Service_id,Vsn}
%%  Value=file.josca

%%% -------------------------------------------------------------------
-module(etcd).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/data.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {dns_info,dbase_id}).
%----------------------------------------------------------------------

-compile([export_all]).
-export([
	 
	]).

-export([start/1,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Gen server functions

start(Args)-> gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop()-> gen_server:call(?MODULE, {stop},infinity).


%%---------------- support----------------------------------------------

ping()->
    gen_server:call(?MODULE, {ping},infinity).  

%%------------- KvS ----------------------------------------------------
create(Key,InitValue)->
    gen_server:call(?MODULE, {create,Key,InitValue},infinity).  
read(Key)->
    gen_server:call(?MODULE, {read,Key},infinity).  
update(Key,NewValue)->
    gen_server:call(?MODULE, {update,Key,NewValue},infinity).  
delete(Key)->
    gen_server:call(?MODULE, {delete,Key},infinity).  
all_objects()->
    gen_server:call(?MODULE, {all_objects},infinity).  
member(Key)->
    gen_server:call(?MODULE, {member,Key},infinity).  

%%------------- Api ----------------------------------------------------
api_help()->
    gen_server:call(?MODULE, {api_help},infinity).  
api_delete_dbase()->
    gen_server:call(?MODULE, {api_delete_dbase},infinity). 

api_all()->
    gen_server:call(?MODULE, {api_all},infinity). 

%%------------- Catalog --------------------------------------------------
add_to_catalog(Id,Vsn)->
    gen_server:call(?MODULE, {add_to_catalog,Id,Vsn},infinity).  
remove_from_catalog(Id,Vsn)->
    gen_server:call(?MODULE, {remove_from_catalog,Id,Vsn},infinity).  
member_catalog(Id,Vsn)->
    gen_server:call(?MODULE, {member,Id,Vsn},infinity).      
read_catalog()->
    gen_server:call(?MODULE, {read_catalog},infinity).  

%%------------- Application --------------------------------------------------
create_josca(AppId,Vsn,JoscaInfo)->
    gen_server:call(?MODULE, {create_josca,AppId,Vsn,JoscaInfo},infinity).    
delete_app_artifact(AppId,Vsn)->
    gen_server:call(?MODULE, {delete_app_artifact,AppId,Vsn},infinity).

read_app_josca_file(AppId,Vsn)->
    gen_server:call(?MODULE, {read_app_josca_file,AppId,Vsn},infinity).

available_apps()->
    gen_server:call(?MODULE, {available_apps},infinity).
    

%%------------- SERVICE --------------------------------------------------

member_file(Type,Id,Vsn)->
    gen_server:call(?MODULE, {member_file,Type,Id,Vsn},infinity).
available_services()->
    gen_server:call(?MODULE, {available_services},infinity).

create_service_artifact(ServiceId,Vsn,RepoInfo)->
    gen_server:call(?MODULE, {create_service_artifact,ServiceId,Vsn,RepoInfo},infinity).    
delete_service_artifact(ServiceId,Vsn)->
    gen_server:call(?MODULE, {delete_service_artifact,ServiceId,Vsn},infinity).

read_service_artifact(ServiceId,Vsn)->
    gen_server:call(?MODULE, {read_service_artifact,ServiceId,Vsn},infinity).
read_service_tar_file(ServiceId,Vsn)->
    gen_server:call(?MODULE, {read_service_tar_file,ServiceId,Vsn},infinity).
read_josca_file(ServiceId,Vsn)->
    gen_server:call(?MODULE, {read_josca_file,ServiceId,Vsn},infinity).
read_service_app_file(ServiceId,Vsn)->
    gen_server:call(?MODULE, {read_service_app_file,ServiceId,Vsn},infinity).


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
init([ServiceInfo]) ->
    Type=set,
    DbaseId="storage/etcd.dbase",
    case dbase_dets:create_dbase(Type,DbaseId) of
	{ok,dbase_already_exsist}->
	    ok;
	 {ok,dbase_created} -> % Intial values
	    ok
    end,
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
    io:format("~p~n",[{?MODULE,'  started ', ?LINE}]),
    {ok, #state{dns_info=MyDnsInfo,dbase_id=DbaseId}}. 
    
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
handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call({api_delete_dbase}, _From, State) ->
    DbaseId=State#state.dbase_id,
    DnsInfo=State#state.dns_info,
    Reply=dbase_dets:delete_dbase(DbaseId),
    NewState=State#state{dns_info=DnsInfo,dbase_id=no_dase},
    {reply, Reply, NewState};

handle_call({api_all}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = dbase_dets:all_objects(DbaseId),
    {reply, Reply, State};

handle_call({api_help}, _From, State) ->
    io:format("p~n~",["api_create_etcd([Type, DbaseId,ServiceInfo]) -> Reply"]),
    io:format("p~n~",['api_delete_etcd([]) -> Reply,ServiceInfo,no_dbase}']),
    io:format("p~n~",['api_all() -> dbase info']),
    {reply, ok, State};

handle_call({create,Key,InitValue}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = rpc:call(node(),dbase_dets,create,[Key,InitValue,DbaseId]),
    {reply, Reply, State};

handle_call({read,Key}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = rpc:call(node(),dbase_dets,read,[Key,DbaseId]),
    {reply, Reply, State};

handle_call({update,Key,NewValue}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = rpc:call(node(),dbase_dets,update,[Key,NewValue,DbaseId]),
    {reply, Reply, State};


handle_call({delete,Key}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = rpc:call(node(),dbase_dets,delete,[Key,DbaseId]),
    {reply, Reply, State};

handle_call({all_objects}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = rpc:call(node(),dbase_dets,all_objects,[DbaseId]),
    {reply, Reply, State};

handle_call({member,Key}, _From, State) ->
    DbaseId=State#state.dbase_id,
    Reply = rpc:call(node(),dbase_dets,member,[Key,DbaseId]),
    {reply, Reply, State};


%%------------- Catalog --------------------------------------------------
%%
%%
%%---------------------------------------------------------------------------
handle_call({add_to_catalog,Id,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,add_to_catalog,[Id,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({remove_from_catalog,Id,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,remove_from_catalog,[Id,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({member,Id,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,member,[Id,Vsn,State#state.dbase_id]),
    {reply, Reply, State};   

handle_call({read_catalog}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,read_catalog,[State#state.dbase_id]),
    {reply, Reply, State};



%%--------------- Services --------------------------------------------------
%%
%%
%%---------------------------------------------------------------------------

handle_call({member_file,_Type,_Id,_Vsn}, _From, State) ->
    Reply = glurk,
    {reply, Reply, State};


handle_call({create_service_artifact,ServiceId,Vsn,RepoInfo}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,create_service_artifact,[ServiceId,Vsn,RepoInfo,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({read_service_artifact,ServiceId,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,read_service_artfact,[ServiceId,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({read_service_tar_file,ServiceId,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,read_service_tar_file,[ServiceId,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({read_service_app_file,ServiceId,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,read_service_app_file,[ServiceId,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({read_josca_file,ServiceId,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,read_josca_file,[ServiceId,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

handle_call({delete_service_artifact,ServiceId,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,delete_service_artifact,[ServiceId,Vsn,State#state.dbase_id]),
    {reply, Reply, State};

%%----------------- Applications -------------------------------------------
%%
%%
%%---------------------------------------------------------------------------
 

handle_call( {create_josca,AppId,Vsn,JoscaInfo}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,create_josca,[
							AppId,Vsn,JoscaInfo,State#state.dbase_id
						       ]),
    {reply, Reply, State};

handle_call({delete_app_artifact,AppId,Vsn}, _From, State) ->
    Reply=rpc:call(node(),repo_lib,delete_app_artifact,[AppId,Vsn,State#state.dbase_id]),
    {reply, Reply, State};


handle_call({heart_beat}, _From, State) ->
    InitArgs=State#state.dns_info,
    Reply=if_dns:call("dns",dns,dns_register,[InitArgs]),
   %  io:format(" ~p~n",[{?MODULE,?LINE,Reply}]),
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
    ?MODULE:heart_beat(),
    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
