%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(init_etcd).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
%-include_lib("eunit/include/eunit.hrl").
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/data.hrl").

%% --------------------------------------------------------------------
-compile([export_all]).
%-export([start/0,
%	 create/0]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

start(DbaseId)->
    store_nodes(DbaseId),
    build_kube_services(DbaseId),
    build_test_services(DbaseId),
    "ok-succed".

store_nodes(DbaseId)->
    Master=store_node(no_service,"master",{"localhost",40000},{"localhost",40000},"lgh.joq_room",[disc],{30010,30110},DbaseId),
    N1=store_node(no_service,"node_1",{"localhost",40001},{"localhost",40001},"lgh.joq_room",[disc],{31010,31110},DbaseId),
    N2=store_node(no_service,"node_2",{"localhost",40002},{"localhost",40002},"stugan.attic",[ratcatch],{32010,32110},DbaseId),
    N3=store_node(no_service,"node_3",{"localhost",40003},{"localhost",40003},"lgh.joq_room",[],{33010,33110},DbaseId),
    N4=store_node(no_service,"node_4",{"localhost",40004},{"localhost",40004},"lgh.v_room",[],{34010,34110},DbaseId),
    ClusterStatus=#cluster
	{
	  all_nodes=[Master,N1,N2,N3,N4],
	  avalible_nodes=[]
	},
    {ok,object_created}=dbase_dets:create({cluster_status},ClusterStatus,DbaseId),  	
    ok.

store_node(Status,NodeId,Public,Local,Capa,Zone,Ports,DbaseId)->
    NodeInfo=#node_info
	{
	  status=Status,
	  node_id=NodeId,
	  public=Public,
	  local=Local,
	  capability=Capa,
	  zone=Zone,
	  port_range=Ports
	},
    {ok,object_created}=dbase_dets:create({node_spec,NodeId},NodeInfo,DbaseId),    
    NodeInfo.


build_kube_services(DbaseId)->
    build_dns(DbaseId),
    build_log(DbaseId),
    build_controller(DbaseId),
  %  build_api_controller(DbaseId),
    ok.

build_dns(DbaseId)->
    ServiceId="dns",
    Vsn="1.0.0",
    SrcDir="ebin/dns/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/dns/src/dns-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.

build_log(DbaseId)->
    ServiceId="log",
    Vsn="1.0.0",
    SrcDir="ebin/log/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/log/src/log-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.

build_controller(DbaseId)->
    ServiceId="controller",
    Vsn="1.0.0",
    SrcDir="ebin/controller/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/controller/src/controller-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.


build_test_services(DbaseId)->
    build_adder(DbaseId),
    build_subtract(DbaseId),
    build_calc(DbaseId),
    build_divider(DbaseId),    
    build_calc_app(DbaseId).

build_adder(DbaseId)->
    ServiceId="adder",
    Vsn="1.0.0",
    SrcDir="ebin/adder/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/adder/src/adder-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.
    
build_subtract(DbaseId)->
    ServiceId="subtract",
    Vsn="1.0.0",
    SrcDir="ebin/subtract/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/subtract/src/subtract-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.
    
build_calc(DbaseId)->
    ServiceId="calc",
    Vsn="1.0.0",
    SrcDir="ebin/calc/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/calc/src/calc-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.
    
build_divider(DbaseId)->
    ServiceId="divider",
    Vsn="1.0.0",
    SrcDir="ebin/divider/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    Joscafile="services/divider/src/divider-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,ServiceId},[{Vsn,{image,TarFilenName,Binary},{josca_spec,JoscaSpec}}],DbaseId),
    ok.
        
build_calc_app(DbaseId)->
    AppId="calc_app",
    Vsn="1.0.0",
    Joscafile="services/calc/src/calc_app-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({service_info,AppId},[{Vsn,{image,na,na},{josca_spec,JoscaSpec}}],DbaseId),
    ok.

    
    

    

%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------
