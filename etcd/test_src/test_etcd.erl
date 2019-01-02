%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_etcd).
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

start()->
    ok=etcd_lib:boot(),
    build_services(),
    store_nodes(),
    "ok-succed".

store_nodes()->
    Master=store_node(no_service,"master",{"localhost",40000},{"localhost",40000},"lgh.joq_room",[disc],{30010,30110}),
    N1=store_node(no_service,"node_1",{"localhost",40001},{"localhost",40001},"lgh.joq_room",[disc],{31010,31110}),
    N2=store_node(no_service,"node_2",{"localhost",40002},{"localhost",40002},"stugan.attic",[ratcatch],{32010,32110}),
    N3=store_node(no_service,"node_3",{"localhost",40003},{"localhost",40003},"lgh.joq_room",[],{33010,33110}),
    N4=store_node(no_service,"node_4",{"localhost",40004},{"localhost",40004},"lgh.v_room",[],{34010,34110}),
    ClusterStatus=#cluster
	{
	  all_nodes=[Master,N1,N2,N3,N4],
	  avalible_nodes=[]
	},
    {ok,object_created}=dbase_dets:create({cluster_status},ClusterStatus,"etcd.dbase"),  	
    ok.

store_node(Status,NodeId,Public,Local,Capa,Zone,Ports)->
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
    {ok,object_created}=dbase_dets:create({node_spec,NodeId},NodeInfo),    
    NodeInfo.

    

build_services()->
    build_adder(),
    build_subtract(),
    build_calc(),
    build_divider(),    
    build_calc_app().

build_adder()->
    ServiceId="adder",
    Vsn="1.0.0",
    SrcDir="ebin/adder/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=dbase_dets:create({image,ServiceId,Vsn},{TarFilenName,Binary}),
    [{Key,Value}]=etcd:read({image,ServiceId,Vsn}),
    [{{image,ServiceId,Vsn},{TarFilenName,Binary}}]=[{Key,Value}],
%    io:format("~p~n",[{{image,ServiceId,Vsn},TarFilenName}]),
    repo_cmn:unix_untar(TarFilenName,Binary,"tabort"),
    Joscafile="services/adder/src/adder-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({josca_spec,ServiceId,Vsn},JoscaSpec),
    ok.
    
build_subtract()->
    ServiceId="subtract",
    Vsn="1.0.0",
    SrcDir="ebin/subtract/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=etcd:create({image,ServiceId,Vsn},{TarFilenName,Binary}),
    [{Key,Value}]=etcd:read({image,ServiceId,Vsn}),
    [{{image,ServiceId,Vsn},{TarFilenName,Binary}}]=[{Key,Value}],
%    io:format("~p~n",[{{image,ServiceId,Vsn},TarFilenName}]),
    repo_cmn:unix_untar(TarFilenName,Binary,"tabort"),
    Joscafile="services/subtract/src/subtract-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=etcd:create({josca_spec,ServiceId,Vsn},JoscaSpec),
    ok.
    
build_calc()->
    ServiceId="calc",
    Vsn="1.0.0",
    SrcDir="ebin/calc/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=etcd:create({image,ServiceId,Vsn},{TarFilenName,Binary}),
    [{Key,Value}]=etcd:read({image,ServiceId,Vsn}),
    [{{image,ServiceId,Vsn},{TarFilenName,Binary}}]=[{Key,Value}],
%    io:format("~p~n",[{{image,ServiceId,Vsn},TarFilenName}]),
    repo_cmn:unix_untar(TarFilenName,Binary,"tabort"),
    Joscafile="services/calc/src/calc-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=etcd:create({josca_spec,ServiceId,Vsn},JoscaSpec),
    ok.
    
build_divider()->
    ServiceId="divider",
    Vsn="1.0.0",
    SrcDir="ebin/divider/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=etcd:create({image,ServiceId,Vsn},{TarFilenName,Binary}),
    [{Key,Value}]=etcd:read({image,ServiceId,Vsn}),
    [{{image,ServiceId,Vsn},{TarFilenName,Binary}}]=[{Key,Value}],
%    io:format("~p~n",[{{image,ServiceId,Vsn},TarFilenName}]),
    repo_cmn:unix_untar(TarFilenName,Binary,"tabort"),
    Joscafile="services/divider/src/divider-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=etcd:create({josca_spec,ServiceId,Vsn},JoscaSpec),
    ok.
        
build_calc_app()->
    AppId="calc_app",
    Vsn="1.0.0",
    Joscafile="services/calc/src/calc_app-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=etcd:create({josca_spec,AppId,Vsn},JoscaSpec),
    ok.

    
    

    

%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------
%% Build and release a s
build_application_josca_test()->
    AppId="calc_app",
    Vsn="1.0.0",
    JoscaBaseName="services/calc/test_src/calc_app.josca",
    JoscaVsn="1.0.0",
    {ok,JoscaInfo}=file:consult(JoscaBaseName),
    Reply= case  if_dns:call("repo",repo,read_josca_file,[AppId,Vsn]) of
	%	ok;
	       []->
		   case if_dns:call("repo",repo,create_josca,[AppId,Vsn,JoscaInfo]) of
		       {ok,created_object}->
			   ok;
		       Err->
			   Err
		   end;
	       JoscaInfo->
		   ok

	end,
    
    Reply.


read_service_josca_test()->
    [{specification,adder},
     {description,"Josca file for adder"},
     {vsn,"1.0.0"},
     {services,[{"adder","1.0.0"}]},
     {num_instances,2},
     {zone,["stugan.attic"]},
     {geo_red,[]},
     {needed_capabilities,[]},
     {josca_files,[]}
    ]=if_dns:call("repo",repo,read_josca_file,["adder","1.0.0"]),
    ok.

read_app_josca_test()->
    [{specification,calc_app},
     {description,"calculator application"},
     {vsn,"1.0.0"},
     {services,[]},
     {num_instances,1},
     {zone,[]},
     {geo_red,[]},
     {needed_capabilities,[]},
     {josca_files,[{"adder","1.0.0"},{"divider","1.0.0"}]}]=if_dns:call("repo",repo,read_josca_file,["calc_app","1.0.0"]),
    ok.



stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
