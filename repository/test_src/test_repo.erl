%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_repo).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("include/repository_data.hrl").
%% --------------------------------------------------------------------
%-export([start/0]).
-export([]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 1. Initial set up
start_test()->
    ok=application:start(repo),   
    ok.

build_100_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.0/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.0",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
    ok.

update_100_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.0/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.0",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
     {ok,artifact_updated}=repo:update_artifact(Artifact),
    ok.

read_100_test()->
    Artifact=repo:read_artifact("template","1.0.0"),
    #artifact{service_id="template",
	      vsn="1.0.0",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
    ok.
%------------------- 
build_101_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.1/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.1",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
    ok.

update_101_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.1/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.1",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
     {ok,artifact_updated}=repo:update_artifact(Artifact),
    ok.

read_100_1_test()->
    #artifact{service_id="template",
	      vsn="1.0.0",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.0"),
    ok.
read_101_0_test()->
    #artifact{service_id="template",
	      vsn="1.0.1",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.1"),
    ok.
read_latest_101_100_test()->
    #artifact{service_id="template",
	      vsn="1.0.1",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template",latest),

    ok.
%-------------------
build_103_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.3/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.3",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
    ok.

update_103_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.3/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.3",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
     {ok,artifact_updated}=repo:update_artifact(Artifact),
    ok.

read_100_2_test()->
    #artifact{service_id="template",
	      vsn="1.0.0",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.0"),
    ok.
read_101_1_test()->
    #artifact{service_id="template",
	      vsn="1.0.1",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.1"),
    ok.

read_103_0_test()->
    #artifact{service_id="template",
	      vsn="1.0.3",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.3"),
    ok.
read_latest_103_101_100_test()->
    #artifact{service_id="template",
	      vsn="1.0.3",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template",latest),

    ok.

%-------------------
build_102_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.2/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.2",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
    ok.

update_102_test()->
    {ok,Artifact}=repo:build_artifact("template","template_1.0.2/ebin"),
    #artifact{service_id="template",
	      vsn="1.0.2",
	      appfile={"template.app",_},
	      modules=_}=Artifact,
     {ok,artifact_updated}=repo:update_artifact(Artifact),
    ok.

read_100_3_test()->
    #artifact{service_id="template",
	      vsn="1.0.0",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.0"),
    ok.
read_101_2_test()->
    #artifact{service_id="template",
	      vsn="1.0.1",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.1"),
    ok.

read_103_1_test()->
    #artifact{service_id="template",
	      vsn="1.0.3",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.3"),
    ok.

read_102_0_test()->
    #artifact{service_id="template",
	      vsn="1.0.2",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template","1.0.2"),
    ok.

read_latest_102_103_101_100_test()->
    #artifact{service_id="template",
	      vsn="1.0.3",
	      appfile={"template.app",_},
	      modules=_}=repo:read_artifact("template",latest),

    ok.


%%---------------- Test to load an application and start it

load_start_test()->
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules}=repo:read_artifact("template","1.0.0"),  
    "template"=ServiceId,
    EbinDir="service_ebin",
    Appfile=filename:join(EbinDir,AppFileBaseName),
    ok=file:write_file(Appfile,AppBinary),
    [file:write_file(filename:join(EbinDir,ModuleName),Bin)||{ModuleName,Bin}<-Modules],
    ok=application:start(template),
    42=template:add(20,22),
    42.0=template:divi(420,10),
    {badrpc,_}=template:divi(420,0),
    {badrpc,_}=rpc:call(node(),template,added_date,[]),
    ok=application:stop(template),
    ok.

upgrade_test()->
    ok=application:start(template), % application is running in the real case 
        #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules}=repo:read_artifact("template","1.0.1"),  
    "template"=ServiceId,
    EbinDir="service_ebin",
    Appfile=filename:join(EbinDir,AppFileBaseName),
    ok=file:write_file(Appfile,AppBinary),
    [file:write_file(filename:join(EbinDir,ModuleName),Bin)||{ModuleName,Bin}<-Modules],
    {ok,[{application,_,Info}]}=file:consult(Appfile),
		  {modules,ModuleList}=lists:keyfind(modules,1,Info),

    GenServerModule=list_to_atom(ServiceId),  
    ModulesToPurge=[Module||Module<-ModuleList,false==(GenServerModule==Module)],
    update_modules(ModulesToPurge),
    update_server(GenServerModule),
%----------
    42=template:add(20,22),
    42.0=template:divi(420,10),
    {error,_}=template:divi(420,0),
    D=date(),

    D=template:added_date(),
    ok=application:stop(template),

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

    
    
stop_test()->  
    application:stop(repo),
    file:delete("repository.dbase"),
    []=os:cmd("rm -r service_ebin/*"),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
