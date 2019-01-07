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
-include("kube/include/kubelet_data.hrl").
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

load_start_apps(PreLoadApps,MyIp,Port)->
    load_start_apps(PreLoadApps,MyIp,Port,[]).
load_start_apps([],_,_,StartResult)->
    StartResult;
load_start_apps([repo|T],MyIp,Port,Acc) -> %Has to be pre loaded
    ok=application:set_env(repo,ip_addr,MyIp),
    ok=application:set_env(repo,port,Port),
    ok=application:set_env(repo,service_id,"repo"),
    EbinDir=?SERVICE_EBIN,
    Appfile=filename:join(EbinDir,"repo.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(repo,vsn,Vsn),
    R=application:start(repo),
    NewAcc=[{"repo",Vsn,R}|Acc],
    load_start_apps(T,MyIp,Port,NewAcc);

load_start_apps([dns|T],MyIp,Port,Acc) -> %Has to be pre loaded
    ok=application:set_env(dns,ip_addr,MyIp),
    ok=application:set_env(dns,port,Port),
    ok=application:set_env(dns,service_id,"dns"),
    EbinDir=?SERVICE_EBIN,
    Appfile=filename:join(EbinDir,"dns.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(dns,vsn,Vsn),
    R=application:start(dns),
    NewAcc=[{"dns",Vsn,R}|Acc],
    load_start_apps(T,MyIp,Port,NewAcc);
    
load_start_apps([Module|T],MyIp,Port,Acc) ->
    Artifact=load_appfiles(atom_to_list(Module),latest),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules
	     }=Artifact,
    ok=application:set_env(Module,ip_addr,MyIp),
    ok=application:set_env(Module,port,Port),
    ok=application:set_env(Module,service_id,ServiceId),
    ok=application:set_env(Module,vsn,Vsn),
    R=application:start(Module),
    NewAcc=[{ServiceId,Vsn,R}|Acc],
    load_start_apps(T,MyIp,Port,NewAcc).


    
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
load_appfiles(ServiceId,Vsn)->
    Artifact=if_dns:call("repo",repo,read_artifact,[ServiceId,Vsn]),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules
	     }=Artifact,
    EbinDir=?SERVICE_EBIN,
    Appfile=filename:join(EbinDir,AppFileBaseName),
    ok=file:write_file(Appfile,AppBinary),
    [file:write_file(filename:join(EbinDir,ModuleName),Bin)||{ModuleName,Bin}<-Modules],
    {ok,Artifact}.
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start(ServiceId)->
    application:start(list_to_atom(ServiceId)).
upgrade(ServiceId,Vsn)->
    Artifact=load_appfiles(ServiceId,Vsn),
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
