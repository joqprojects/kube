%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_nfvi).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("../include/tcp.hrl").
-include("../include/dns.hrl").
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
start_test_tabort()->
    nfvi_lib:boot(),
    ok.


%% Build and release a service and application josca
get_zone_capability_1_test()->
    [[{init_args,[{public_ip,"localhost"},
		  {public_port,40002},
		  {service_id,"nfvi"},
		  {vsn,"1.0.0"}]},
      {zone,"stugan.attic"},
      {capabilities,[ratcatch]}]]=if_dns:call("vim",vim,get_all_instances,[]),
    ok.


deploy_service_1_test()->
    ServiceId="adder",
    Vsn="1.0.0",
    Ip=?ADDER_IP,
    Port=?ADDER_PORT,
    {TarName,TarBinary}=if_dns:call("repo",repo,read_service_tar_file,[ServiceId,Vsn]),
    [L1|_]=if_dns:call("vim",vim,get_all_instances,[]),
    {init_args,L2}=lists:keyfind(init_args,1,L1),
    {public_ip,NfviIp}=lists:keyfind(public_ip,1,L2),
    {public_port,NfviPort}=lists:keyfind(public_port,1,L2),	 
    {ok,started}= tcp:call(NfviIp,NfviPort,[nfvi,deploy_service,[Ip,Port,ServiceId,Vsn,{TarName,TarBinary}]]),

    42=if_dns:call("adder",adder,add,[20,22]),
    ok.

deploy_service_already_deployed_test()->
    ServiceId="adder",
    Vsn="1.0.0",
    Ip=?ADDER_IP,
    Port=?ADDER_PORT,
    {TarName,TarBinary}=if_dns:call("repo",repo,read_service_tar_file,[ServiceId,Vsn]),
    [L1|_]=if_dns:call("vim",vim,get_all_instances,[]),
    {init_args,L2}=lists:keyfind(init_args,1,L1),
    {public_ip,NfviIp}=lists:keyfind(public_ip,1,L2),
    {public_port,NfviPort}=lists:keyfind(public_port,1,L2),	 
    {error,[nfvi,139,'Port already exists',30002]}= tcp:call(NfviIp,NfviPort,[nfvi,deploy_service,[Ip,Port,ServiceId,Vsn,{TarName,TarBinary}]]),

    42=if_dns:call("adder",adder,add,[20,22]),
    ok.


remove_service_1_test()->
    ServiceId="adder",
    Vsn="1.0.0",
    Port=?ADDER_PORT,
    [L1|_]=if_dns:call("vim",vim,get_all_instances,[]),
    {init_args,L2}=lists:keyfind(init_args,1,L1),
    {public_ip,NfviIp}=lists:keyfind(public_ip,1,L2),
    {public_port,NfviPort}=lists:keyfind(public_port,1,L2),	 
    _Node=tcp:call(NfviIp,NfviPort,[nfvi,remove_service,[ServiceId,Vsn,Port]]),

    {error,{econnrefused,tcp,69}}=if_dns:call("adder",adder,add,[20,22]),
    ok.

remove_service_already_removed_test()->
    ServiceId="adder",
    Vsn="1.0.0",
    Port=?ADDER_PORT,
    [L1|_]=if_dns:call("vim",vim,get_all_instances,[]),
    {init_args,L2}=lists:keyfind(init_args,1,L1),
    {public_ip,NfviIp}=lists:keyfind(public_ip,1,L2),
    {public_port,NfviPort}=lists:keyfind(public_port,1,L2),	 
    _Node=tcp:call(NfviIp,NfviPort,[nfvi,remove_service,[ServiceId,Vsn,Port]]),

    {error,{econnrefused,tcp,69}}=if_dns:call("adder",adder,add,[20,22]),
    ok.

stop_test()->    
%    Dir=integer_to_list(?ADDER_PORT),
 %   os:cmd("rm -r "++Dir),
 %   [_,Host]=string:tokens(atom_to_list(node()),"@"),    
 %   ServiceNodeStr=integer_to_list(?ADDER_PORT),
 %   ServiceNode=list_to_atom(ServiceNodeStr++"@"++Host),
 %   cmn:stop_node(ServiceNode),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
