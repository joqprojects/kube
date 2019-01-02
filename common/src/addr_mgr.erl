%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(addr_mgr).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("common/include/cmn_nodes.hrl").
%% --------------------------------------------------------------------
%% External exports
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% 

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {application, template,
%% ------------------------------------------------------------------
init_args_public_ip(InitArgs)->
    {public_ip,PublicIpAddr}=lists:keyfind(public_ip,1,InitArgs),
    PublicIpAddr.
init_args_public_port(InitArgs)->
    {public_port,PublicPort}=lists:keyfind(public_port,1,InitArgs),
    PublicPort.

init_args_local_ip(InitArgs)->
    {local_ip,PublicIpAddr}=lists:keyfind(local_ip,1,InitArgs),
    PublicIpAddr.
init_args_local_port(InitArgs)->
    {local_port,PublicPort}=lists:keyfind(local_port,1,InitArgs),
    PublicPort.

init_args_service_id(InitArgs)->
    {service_id,ServiceId}=lists:keyfind(service_id,1,InitArgs),
    ServiceId.

init_args_vsn(InitArgs)->
    {vsn,Vsn}=lists:keyfind(vsn,1,InitArgs),
    Vsn.

update_init_args(PublicIp,PublicPort,LocalIp,LocalPort,ServiceId,Vsn)->
    [{public_ip,PublicIp},{public_port,PublicPort},
     {local_ip,LocalIp},{local_port,LocalPort},
     {service_id,ServiceId},{vsn,Vsn}
    ].
    
    
