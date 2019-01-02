%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nfvi_lib).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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
boot()->
    {ok,L}=file:consult("nfvi.config"),
    {init_args,L2}=lists:keyfind(init_args,1,L),
    {public_ip,PIp}=lists:keyfind(public_ip,1,L2),
    {public_port,PPort}=lists:keyfind(public_port,1,L2),
    {local_ip,LIp}=lists:keyfind(local_ip,1,L2),
    {local_port,LPort}=lists:keyfind(local_port,1,L2),
    {service_id,Service}=lists:keyfind(service_id,1,L2),
    {vsn,Vsn}=lists:keyfind(vsn,1,L2),
    nfvi:app_start(PIp,PPort,LIp,LPort,Service,Vsn). 
  



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
allocate_port([])->
    no_ports;
allocate_port([Port|T])->
    {ok,Port,T}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_vm(Port)->
    Dir=integer_to_list(Port),
    Ebin=filename:join([Dir,"service_ebin"]),
    NodeName=integer_to_list(Port),
%    io:format("~p~n",[{?MODULE,?LINE,Ebin,NodeName}]),
    cmn:start_node(Ebin,NodeName).
	      
		  

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_image(ServiceId,Vsn,Port)->
    Dir=integer_to_list(Port),
    case filelib:is_dir(Dir) of
	true-> %remove old service that should be removed
	    clean_up(Port);
	false->
	    ok
    end,
    Reply=case file:make_dir(Dir) of
	      ok->
		  case if_dns:call("repo",repo,read_service_tar_file,[ServiceId,Vsn]) of
		      []->
			  {error,[?MODULE,?LINE,'service with vsn not exists in repo',ServiceId,Vsn,Port]};
		      {TarName,TarBinary}->
			  case repo_cmn:unix_untar(TarName,TarBinary,Dir) of
			      ok->
				  {ok,image_loaded};
			      Err->
				  {error,[?MODULE,?LINE,'failed to load image',Err,ServiceId,Vsn,Port]}
			  end
		  end;
	      Err->
		  {error,[?MODULE,?LINE,Err,ServiceId,Vsn,Port]}
	  end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
clean_up(Port)->
    Dir=integer_to_list(Port),
    {ok,Hostname}=inet:gethostname(),
    Node=list_to_atom(Dir++"@"++Hostname),
    rpc:call(Node,init,stop,[]),
    os:cmd("rm -r "++Dir).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_availible_ports(Start,Stop)->
    N=Stop-Start,
    create_availible_ports(N,Start,[]).

create_availible_ports(-1,_Start,Acc)->
    Acc;
create_availible_ports(N,Start,Acc) ->
    NewAcc=[Start+N|Acc],
    create_availible_ports(N-1,Start,NewAcc).

  
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
zone()->
    {ok,I}=file:consult("nfvi.config"),
    R=case lists:keyfind(zone,1,I) of
	  {zone,Z}->
	      Z;
	  false ->
	      []
      end,
    R.

capabilities()->
    {ok,I}=file:consult("nfvi.config"),
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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_service(NodeService,PublicIp,PublicPort,LocalIp,PortService,
	      ServiceId,Vsn)->
%    if_log:call(debug,[{?MODULE,?LINE,NodeService,PublicIp,PublicPort,LocalIp,PortService,
%	      ServiceId,Vsn,net_adm:ping(NodeService)}]),
    Result = case start_service(NodeService,PublicIp,PublicPort,LocalIp,PortService,
	      ServiceId,Vsn,?NUM_TRIES_START_SERVICE,?INTERVAL_START_SERVICE,not_started) of
		 ok->
	%	     if_log:call(debug,[{?MODULE,?LINE,'Service succeded to start',NodeService,PublicIp,PublicPort,LocalIp,PortService,
					% ServiceId,Vsn,net_adm:ping(NodeService)}]),
		     ok;
		 Err->
		     if_log:call(debug,[?MODULE,?LINE,'Service Failed to start',NodeService,PublicIp,PublicPort,LocalIp,PortService,
					ServiceId,Vsn]),
		     Err
	     end,
    Result.

start_service(_NodeService,_PublicIp,_PublicPort_,_LocalIp,_PortService,
	      _ServiceId,_Vsn,_N,_Interval,ok)->
%    io:format(" ~p~n",[{?MODULE,?LINE,NodeService,PublicIp,PublicPort,LocalIp,PortService,
	   %   ServiceId,Vsn,net_adm:ping(NodeService)}]),
   % if_log:call(debug,[?MODULE,?LINE,{ok,ok},N,NodeService,PublicIp,PublicPort,LocalIp,PortService,
	     % ServiceId,Vsn]),
    ok;
start_service(_NodeService,_PublicIp,_PublicPort,_LocalIp,_PortService,
	      _ServiceId,_Vsn,0,_Interval,ok)->
 %   if_log:call(debug,[?MODULE,?LINE,{error,ok},0,NodeService,PublicIp,PublicPort,LocalIp,PortService,
	   %   ServiceId,Vsn]),
    ok;
start_service(_NodeService,_PublicIp,_PublicPort,_LocalIp,_PortService,
	      _ServiceId,_Vsn,0,_Interval,R)->
  %  if_log:call(debug,[?MODULE,?LINE,0,R,NodeService,PublicIp,PublicPort,LocalIp,PortService,
	%      ServiceId,Vsn]),
    R;

start_service(NodeService,PublicIp,PublicPort,LocalIp,PortService,
	      ServiceId,Vsn,N,Interval,_StartState)->
 %   if_log:call(debug,[?MODULE,?LINE,StartState,N,NodeService,PublicIp,PublicPort,LocalIp,PortService,
%	      ServiceId,Vsn]),
    case rpc:call(NodeService,list_to_atom(ServiceId),app_start,[PublicIp,PublicPort,LocalIp,PortService,
	      ServiceId,Vsn]) of
	{ok,ok}->
%	    if_log:call(debug,[?MODULE,?LINE,{ok,ok},StartState,N,NodeService,PublicIp,PublicPort,LocalIp,PortService,
%			       ServiceId,Vsn]),
	    NewN=N,
	    NewStartState=ok;
	{_Err,ok}->
	  %  if_log:call(debug,[?MODULE,?LINE, {Err,ok},StartState,N,NodeService,PublicIp,PublicPort,LocalIp,PortService,
	%		       ServiceId,Vsn]),
	    NewN=N,
	    NewStartState=ok;
	Err ->
	 %   if_log:call(error,[?MODULE,?LINE,'couldnt start service',Err,NodeService,PublicIp,PublicPort,LocalIp,PortService,
	%		       ServiceId,Vsn]),
	    NewN=N-1,
	    NewStartState=Err
    end,
    timer:sleep(Interval),
    start_service(NodeService,PublicIp,PublicPort,LocalIp,PortService,
	      ServiceId,Vsn,NewN,Interval, NewStartState).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

create_node_name(Port)->
    [_,Host]=string:tokens(atom_to_list(node()),"@"),    
    ServiceNodeStr=integer_to_list(Port),
    ServiceNode=list_to_atom(ServiceNodeStr++"@"++Host),
    ServiceNode.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

