%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nfvi_boot).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	 boot/0
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
boot()->
    case application:start(nfvi) of
	ok->
	    case rpc:call(node(),nfvi_boot,boot,[]) of
		{badrpc,_Err}->
	                % call log with err msg
		    timer:sleep(10*60*1000),
		    nfvi_boot:boot();
		ok->
		    InitArgs=create_init_args(),
		    nfvi:init_service(InitArgs)
	    end;
	{error,{already_started,nfvi}} ->
	    ok;
	 {error,_Err}->
	    % call log with err msg
	    timer:sleep(10*60*1000),
	    nfvi_boot:boot()
    end.
	    

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------


%% ====================================================================
%% Internal functions
%% ====================================================================
create_init_args()->
    {ok,L}=file:consult("nfvi.config"),
    {init_args,L2}=lists:keyfind(init_args,1,L),
    {public_ip,NfviIp}=lists:keyfind(public_ip,1,L2),
    {public_port,NfviPort}=lists:keyfind(public_port,1,L2),
    {service_id,ServiceId}=lists:keyfind(service_id,1,L2),
    {vsn,Vsn}=lists:keyfind(vsn,1,L2),
    InitArgs=addr_mgr:update_init_args(NfviIp,NfviPort,ServiceId,Vsn),
    InitArgs.
