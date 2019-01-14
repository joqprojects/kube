%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet_lib).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/trace_debug.hrl").
-include("kube/include/kubelet_data.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/repository_data.hrl").
%% --------------------------------------------------------------------
-define(NUM_TRIES_START_SERVICE,10).
-define(INTERVAL_START_SERVICE,1000).


%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
dns_register(DnsInfo, DnsList) ->
    TimeStamp=erlang:now(),
    NewDnsInfo=DnsInfo#dns_info{time_stamp=TimeStamp},
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    
    X1=[X||X<-DnsList,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList=[NewDnsInfo|X1],
    NewDnsList.

de_dns_register(DnsInfo,DnsList)->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    NewDnsList=[X||X<-DnsList,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList.


load_start_app(ServiceId,VsnInput,NodeIp,NodePort)->
    Module=list_to_atom(ServiceId),
    {ok,Artifact}=load_appfiles(ServiceId,VsnInput,NodeIp,NodePort),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={_,_},
	      modules=_
	     }=Artifact,
    ok=application:set_env(Module,ip_addr,NodeIp),
    ok=application:set_env(Module,port,NodePort),
    ok=application:set_env(Module,service_id,ServiceId),
    ok=application:set_env(Module,vsn,Vsn),
    R=application:start(Module).    

load_start_pre_loaded_apps(PreLoadApps,NodeIp,NodePort)->
  %  io:format("~p~n",[{?MODULE,?LINE,PreLoadApps}]),
    load_start_apps(PreLoadApps,NodeIp,NodePort,[]).
load_start_apps([],_,_,StartResult)->
    StartResult;
load_start_apps([repo|T],NodeIp,NodePort,Acc) -> %Has to be pre loaded
    ok=application:set_env(repo,ip_addr,NodeIp),
    ok=application:set_env(repo,port,NodePort),
    ok=application:set_env(repo,service_id,"repo"),
    EbinDir=?SERVICE_EBIN,
    Appfile=filename:join(EbinDir,"repo.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(repo,vsn,Vsn),
    R=application:start(repo),
    NewAcc=[{"repo",Vsn,R}|Acc],
    load_start_apps(T,NodeIp,NodePort,NewAcc);

load_start_apps([dns|T],NodeIp,NodePort,Acc) -> %Has to be pre loaded
    ok=application:set_env(dns,ip_addr,NodeIp),
    ok=application:set_env(dns,port,NodePort),
    ok=application:set_env(dns,service_id,"dns"),
    EbinDir=?SERVICE_EBIN,
    Appfile=filename:join(EbinDir,"dns.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(dns,vsn,Vsn),
    R=application:start(dns),
    NewAcc=[{"dns",Vsn,R}|Acc],
    load_start_apps(T,NodeIp,NodePort,NewAcc);
    
load_start_apps([Module|T],NodeIp,NodePort,Acc) ->
    {ok,Artifact}=load_appfiles(atom_to_list(Module),latest,NodeIp,NodePort),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={_,_},
	      modules=_
	     }=Artifact,
    ok=application:set_env(Module,ip_addr,NodeIp),
    ok=application:set_env(Module,port,NodePort),
    ok=application:set_env(Module,service_id,ServiceId),
    ok=application:set_env(Module,vsn,Vsn),
    R=application:start(Module),
    NewAcc=[{ServiceId,Vsn,R}|Acc],
    load_start_apps(T,NodeIp,NodePort,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop_unload_app(DnsInfo)->
    #dns_info{service_id=ServiceId,vsn=Vsn}=DnsInfo,
    Artifact=if_dns:call("repo",repo,read_artifact,[ServiceId,Vsn]),    
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,_},
	      modules=Modules
	     }=Artifact,
    Ebin=case ServiceId of
	     "lib"->
		 "lib_ebin";
	     "kubelet"->
		 "kubelet_ebin";
	     _->
		 ?SERVICE_EBIN
	 end,   
    Module=list_to_atom(ServiceId),
    application:stop(Module),
    application:unload(Module),

    Appfile=filename:join(Ebin,AppFileBaseName),
    ok=file:delete(Appfile),
    DeleteResult=[file:delete(filename:join(Ebin,ModuleName))||{ModuleName,_}<-Modules], 
    rpc:cast(node(),if_dns,call,["dns",dns,de_dns_register,[DnsInfo]]),
    rpc:cast(node(),if_dns,call,["controller",dns,de_dns_register,[DnsInfo]]),
    Reply=case [Y||Y<-DeleteResult,false=={Y=:=ok}] of
	      []->
		  ok;
	      Err ->
		  {error,[?MODULE,?LINE,'error deleting modules',Err]}
	  end,
   Reply.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
zone()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(zone,1,I) of
	  {zone,Z}->
	      Z;
	  false ->
	      []
      end,
    R.

capabilities()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(capabilities,1,I) of
	  {capabilities,C}->
	      C;
	  false ->
	      []
      end,
    R.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_appfiles(ServiceId,VsnInput,NodeIp,NodePort)->  % VsnInput Can be latest !!!
    Ebin=case ServiceId of
	     "lib"->
		 "lib_ebin";
	     "kubelet"->
		 "kubelet_ebin";
	     _->
		 ?SERVICE_EBIN
	 end,
    SenderInfo=#sender_info{ip_addr=NodeIp,
			    port=NodePort,
			    module=?MODULE,line=?LINE},
    Artifact=if_dns:call("repo",repo,read_artifact,[ServiceId,VsnInput]),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules
	     }=Artifact,
    Appfile=filename:join(Ebin,AppFileBaseName),
    ok=file:write_file(Appfile,AppBinary),
    [file:write_file(filename:join(Ebin,ModuleName),Bin)||{ModuleName,Bin}<-Modules],
    {ok,Artifact}.
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start(ServiceId)->
    application:start(list_to_atom(ServiceId)).

upgrade(ServiceId,Vsn,NodeIp,NodePort)->
    Artifact=load_appfiles(ServiceId,Vsn,NodeIp,NodePort),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules
	     }=Artifact,
    GenServerModule=list_to_atom(ServiceId),  
    ModulesToPurge=[Module||Module<-Modules,false==(GenServerModule==Module)],
    update_modules(ModulesToPurge),
    update_server(GenServerModule),
    ok.

update_server(GenServerModule)->
    ok= sys:suspend(GenServerModule),
    false=code:purge(GenServerModule),
    {module,GenServerModule}=code:load_file(GenServerModule),
    ok= sys:change_code(GenServerModule,GenServerModule,"0",[]),
    sys:resume(GenServerModule).

update_modules([])->
    ok;
update_modules([Module|T]) ->
    code:purge(Module),
    code:load_file(Module),
    update_modules(T).


