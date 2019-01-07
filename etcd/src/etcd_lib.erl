%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(etcd_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").
-include("kube/include/tcp.hrl").
-include("kube/include/data.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================






%%--------------------- API --------------------------------------------
    







%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_service_artifact(ServiceId,Vsn,RepoInfo,DbaseId)->
    {tar_file,{TarName,Binary}}=lists:keyfind(tar_file,1,RepoInfo),
    R1=dbase_dets:create({tar_file,ServiceId,Vsn},{TarName,Binary},DbaseId),

    {app_file,{_AppBaseName,_AppVsn,AppInfo}}=lists:keyfind(app_file,1,RepoInfo),
    R2=dbase_dets:create({app_file,ServiceId,Vsn},AppInfo,DbaseId),

    {josca_file,{_JoscaBaseName,_JoscaVsn,JoscaInfo}}=lists:keyfind(josca_file,1,RepoInfo),
    R3=repo_lib:create_josca(ServiceId,Vsn,JoscaInfo,DbaseId),
    {R1,R2,R3}.

create_josca(AppId,Vsn,JoscaInfo,DbaseId)->
    {R,_}=dbase_dets:create({josca_file,AppId,Vsn},JoscaInfo,DbaseId),
    R.

read_service_artfact(ServiceId,Vsn,DbaseId)->    
    TarInfo=dbase_dets:read({tar_file,ServiceId,Vsn},DbaseId),
    AppInfo=dbase_dets:read({app_file,ServiceId,Vsn},DbaseId),
    JoscaInfo= dbase_dets:read({josca_file,ServiceId,Vsn},DbaseId),
    [{tar_file,TarInfo},{app_file,AppInfo},{josca_file,JoscaInfo}].

read_service_app_file(ServiceId,Vsn,DbaseId)->
    R = case dbase_dets:read({app_file,ServiceId,Vsn},DbaseId)of
	    []->
		[];
	    [{_AppKey,AppInfo}]->
		AppInfo
	end,
    R.	

read_josca_file(Id,Vsn,DbaseId)-> %can be both app or service
    R = case dbase_dets:read({josca_file,Id,Vsn},DbaseId) of
	    []->
		[];
	    [{_JoscaKey,JoscaInfo}]->
		JoscaInfo
	end,
    R.	
read_service_tar_file(ServiceId,Vsn,DbaseId)->
    R = case dbase_dets:read({tar_file,ServiceId,Vsn},DbaseId) of
	    []->
		[];
	    [{_TarKey,TarInfo}]->
		TarInfo
	end,
    R.

delete_service_artifact(ServiceId,Vsn,DbaseId)->
    {R1,_}=dbase_dets:delete({tar_file,ServiceId,Vsn},DbaseId),
    {R2,_}=dbase_dets:delete({app_file,ServiceId,Vsn},DbaseId),
    {R3,_}= dbase_dets:delete({josca_file,ServiceId,Vsn},DbaseId),
    {R1,R2,R3}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_app_artifact(AppId,Vsn,DbaseId)->
    {R,_}= dbase_dets:delete({josca_file,AppId,Vsn},DbaseId),
    R.

%%------------- Catalog --------------------------------------------------
%%
%%
%%---------------------------------------------------------------------------
add_to_catalog(Id,Vsn,DbaseId)->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  dbase_dets:update(service_catalog,[{Id,Vsn}],DbaseId);
	      [{service_catalog,CatalogList}]->
		  case lists:member({Id,Vsn},CatalogList) of
		      false->
			  NewCatalog=[{Id,Vsn}|CatalogList],
			  dbase_dets:delete(service_catalog,DbaseId),
			  dbase_dets:update(service_catalog,NewCatalog,DbaseId);
		      true ->
			  {ok,[?MODULE,?LINE,'already exists',{Id,Vsn}]}
		  end;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

remove_from_catalog(Id,Vsn,DbaseId)->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  {error,[?MODULE,?LINE,'service_catalog has no entires']};
	      [{service_catalog,CatalogList}]->
		  case lists:member({Id,Vsn},CatalogList) of
		      false->
			  {error,[?MODULE,?LINE,'doesnt exists',{Id,Vsn}]};
		      true ->
			  NewCatalog=lists:delete({Id,Vsn},CatalogList),
			  dbase_dets:delete(service_catalog,DbaseId),
			  dbase_dets:update(service_catalog,NewCatalog,DbaseId)
		  end;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

member(Id,Vsn,DbaseId) ->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  {error,[?MODULE,?LINE,'service_catalog has no entires']};
	      [{service_catalog,CatalogList}]->
		  lists:member({Id,Vsn},CatalogList);
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

read_catalog(DbaseId)->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  {error,[?MODULE,?LINE,'service_catalog has no entires']};
	      [{service_catalog,CatalogList}]->
		  CatalogList;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.




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
    {ok,object_created}=dbase_dets:create({node_info,NodeId},NodeInfo,DbaseId),    
    NodeInfo.

    

build_services(DbaseId)->
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
    {ok,object_created}=dbase_dets:create({image,ServiceId,Vsn},{TarFilenName,Binary},DbaseId),
    Joscafile="services/adder/src/adder-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({josca_spec,ServiceId,Vsn},JoscaSpec,DbaseId),
    ok.
    
build_subtract(DbaseId)->
    ServiceId="subtract",
    Vsn="1.0.0",
    SrcDir="ebin/subtract/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=dbase_dets:create({image,ServiceId,Vsn},{TarFilenName,Binary},DbaseId),
    Joscafile="services/subtract/src/subtract-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({josca_spec,ServiceId,Vsn},JoscaSpec,DbaseId),
    ok.
    
build_calc(DbaseId)->
    ServiceId="calc",
    Vsn="1.0.0",
    SrcDir="ebin/calc/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=dbase_dets:create({image,ServiceId,Vsn},{TarFilenName,Binary},DbaseId),
    Joscafile="services/calc/src/calc-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({josca_spec,ServiceId,Vsn},JoscaSpec,DbaseId),
    ok.
    
build_divider(DbaseId)->
    ServiceId="divider",
    Vsn="1.0.0",
    SrcDir="ebin/divider/ebin",
    {ok,{TarFilenName,Binary}}=repo_cmn:create_tar(ServiceId,Vsn,SrcDir),
    {ok,object_created}=dbase_dets:create({image,ServiceId,Vsn},{TarFilenName,Binary},DbaseId),
    Joscafile="services/divider/src/divider-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({josca_spec,ServiceId,Vsn},JoscaSpec,DbaseId),
    ok.
        
build_calc_app(DbaseId)->
    AppId="calc_app",
    Vsn="1.0.0",
    Joscafile="services/calc/src/calc_app-1.0.0.josca",
    JoscaSpec=repo_cmn:create_josca_spec(Joscafile),
    {ok,object_created}=dbase_dets:create({josca_spec,AppId,Vsn},JoscaSpec,DbaseId),
    ok.

    
    
