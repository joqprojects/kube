%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(if_dns).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").

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
%% --------------------------------------------------------------------

call(ServiceStr,M,F,A)->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Reply=case ServiceStr of
	      "dns"->
%		  io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
%		  R1=rpc:call(node(),tcp,call,[?DNS_IP,?DNS_PORT,?DNS_LOCAL_IP,?DNS_LOCAL_PORT,{M,F,A}]),

		  rpc:call(node(),tcp,call,[?DNS_IP,?DNS_PORT,{M,F,A}]);
%		  io:format(" ~p~n",[{?MODULE,?LINE,R1}]);
	          %tcp:call(?DNS_IP,?DNS_PORT,?DNS_LOCAL_IP,?DNS_LOCAL_PORT,{M,F,A});
	      _->
		%  io:format(" ~p~n",[{?MODULE,?LINE}]),
		  Z=rpc:call(node(),tcp,call,[?DNS_IP,?DNS_PORT,{dns,get_instances,[ServiceStr]}]),
		  %["localhost",20000,{dns,get_instances,["vim"]},'100200273']}
		 % ["localhost",20000,{dns,get_instances,["vim"]},'100200273']
		%  io:format(" get instances ~p~n",[{?MODULE,?LINE,Z}]),
		  case Z of
		%  case tcp:call(?DNS_IP,?DNS_PORT,?DNS_LOCAL_IP,?DNS_LOCAL_PORT,{dns,get_instances,[ServiceStr]}) of
		      []->
			  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err]};
		      %->
		%	  io:format(" ~p~n",[{?MODULE,?LINE,NfviIp,NfviPort,ServiceIp,ServicePort,M,F,A}]),
		%	  tcp:call(NfviIp,NfviPort,ServiceIp,ServicePort,{M,F,A});
		      [{PublicIp,PublicPort,LocalIp,LocalPort}|_Tail]->
			  tcp:call(PublicIp,PublicPort,LocalIp,LocalPort,{M,F,A})
		  end
	  end,
    Reply.
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

