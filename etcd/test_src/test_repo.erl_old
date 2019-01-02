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
-include("services/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------
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
%% --------------------------------------------------------------------
%% Build and release a service and application josca
build_calc_test()->
    ServiceId="calc",
    Vsn="1.0.0",
    SrcDir="ebin/calc/ebin",
    Reply= case if_dns:call("repo",repo,read_service_artifact,[ServiceId,Vsn]) of
	       [{tar_file,[]},{app_file,[]},{josca_file,[]}]->
		   case repo_cmn:user_create_service_tar_file(ServiceId,SrcDir) of
		       {{ok,_},{ok,_},{ok,_}}->
			   ok;
		       Err ->
			   {error,[?MODULE,?LINE,Err]}
		   end;		   
	       _ ->
		   ok
		       
	   end,
    Reply.

build_div_test()->
    ServiceId="divider",
    Vsn="1.0.0",
    SrcDir="ebin/divider/ebin",
    Reply= case if_dns:call("repo",repo,read_service_artifact,[ServiceId,Vsn]) of
	       [{tar_file,[]},{app_file,[]},{josca_file,[]}]->
		   case repo_cmn:user_create_service_tar_file(ServiceId,SrcDir) of
		       {{ok,_},{ok,_},{ok,_}}->
			   ok;
		       Err ->
			   {error,[?MODULE,?LINE,Err]}
		   end;		   
	       _ ->
		   ok
		       
	   end,
    Reply.

build_sub_test()->
    ServiceId="subtract",
    Vsn="1.0.0",
    SrcDir="ebin/subtract/ebin",
    Reply= case if_dns:call("repo",repo,read_service_artifact,[ServiceId,Vsn]) of
	       [{tar_file,[]},{app_file,[]},{josca_file,[]}]->
		   case repo_cmn:user_create_service_tar_file(ServiceId,SrcDir) of
		       {{ok,_},{ok,_},{ok,_}}->
			   ok;
		       Err ->
			   {error,[?MODULE,?LINE,Err]}
		   end;		   
	       _ ->
		   ok
		       
	   end,
    Reply.


build_adder_test()->
    ServiceId="adder",
    Vsn="1.0.0",
    SrcDir="ebin/adder/ebin",
    Reply= case if_dns:call("repo",repo,read_service_artifact,[ServiceId,Vsn]) of
	       [{tar_file,[]},{app_file,[]},{josca_file,[]}]->
		   case repo_cmn:user_create_service_tar_file(ServiceId,SrcDir) of
		       {{ok,_},{ok,_},{ok,_}}->
			   ok;
		       Err ->
			{error,[?MODULE,?LINE,Err]}
		   end;		   
	       _ ->
		   ok
		       
	   end,
    Reply.

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
    
