%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cmn).

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

-define(JOSCA,
{specification, j1 ,
 [{description, "just for testing infra_master" },
  {vsn, "1.23.42" },
  {services,[{s10,"1.10.0"}]}
  {zone,[]},
  {geo_red,[]}
 ]
}).

josca_all(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    Info.

josca_info(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {specification,_Name,I}=lists:keyfind(specification,1,Info),
    I.

josca_name(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {specification,Name,_I}=lists:keyfind(specification,1,Info),
    Name.

josca_vsn(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {specification,_Name,I}=lists:keyfind(specification,1,Info),
    {vsn,R}=lists:keyfind(vsn,1,I),
    R.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {application, template,
-define (APP,
	 {application, template, 
	  [{description, "template for services " },
	   {vsn, "3.0.0" },
	   {modules,[template,template_lib,template_server]},
	   {registered,[template]},
	   {applications, []},
	   {mod, {template,[]}},
	   {start_phases, []},
           % Added by jle
	   {board_functionality,[]},
	   {needed_services,[]}	
	  ]
	 }).
%% ------------------------------------------------------------------
app_all(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    Info.

app_info(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {application,_AppName,I}=lists:keyfind(application,1,Info),
    I.

app_name(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {application,AppName,_I}=lists:keyfind(application,1,Info),
    AppName.

app_vsn(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {application,_AppName,I}=lists:keyfind(application,1,Info),
    {vsn,R}=lists:keyfind(vsn,1,I),
    R.

app_board_functionality(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {application,_AppName,I}=lists:keyfind(application,1,Info),
    {board_functionality,R}=lists:keyfind(board_functionality,1,I),
    R.

app_needed_services(FullFileName)->
    {ok,Info}=file:consult(FullFileName), 
    {application,_AppName,I}=lists:keyfind(application,1,Info),
    {needed_services,R}=lists:keyfind(needed_services,1,I),
    R.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% 
%% ------------------------------------------------------------------
unique_id(int)->
 %   erlang:phash2({node(), erlang:system_time()});
    erlang:phash2({node(),local_system_time()});
unique_id(atom)->
   % list_to_atom(integer_to_list(erlang:phash2({node(), erlang:system_time()})));
    list_to_atom(integer_to_list(erlang:phash2({node(), local_system_time()})));
unique_id(string)->
%    integer_to_list(erlang:phash2({node(), erlang:system_time()}));
    integer_to_list(erlang:phash2({node(),local_system_time()}));
unique_id(Err) ->
    {error,[?MODULE,?LINE,Err, 'not defined']}.


local_system_time()->
    {A,B,C}=erlang:now(),
    A*1000000*1000000+B*1000000+C.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% 
%% ------------------------------------------------------------------
stop_node(Node)->
    rpc:call(Node,init,stop,[]),
    check_if_node_stopped(Node,20),
    stopped.
    


check_if_node_stopped(Node,Period)->
    N=check_stopped(Node,Period,0,false),
    N.

check_stopped(_Node,_Period,N,true)->
    N;
check_stopped(Node,Period,N,Quit)->
    case net_adm:ping(Node) of
	pang->
	    NewQuit=true,
	    NewN=N;
	pong->
	    timer:sleep(Period),
	    NewQuit=Quit,
	    NewN=N+1
    end,
    check_stopped(Node,Period,NewN,NewQuit).


start_node(Ebin,NodeName)->
    ErlCmd="erl -pa "++Ebin++" "++"-sname "++NodeName++" "++"-detached -setcookie glurk",
    []=os:cmd(ErlCmd),
    [_BoardIdStr,Host]=string:tokens(atom_to_list(node()),"@"),    
    Node=list_to_atom(NodeName++"@"++Host),
    check_if_node_running(Node,100).
    
check_if_node_running(Node,Period)->
    N=check_running(Node,Period,0,false),
    N.

check_running(Node,_Period,20,_)->
%    io:format("~p~n",[{?MODULE,?LINE,'couldnt start node ',Node}]),
    {error,[{?MODULE,?LINE,'couldnt start node ',Node}]};
check_running(Node,_Period,_N,true)->
    {ok,Node};
check_running(Node,Period,N,Quit)->
 %   io:format("~p~n",[{?MODULE,?LINE,Node,N}]),
    case net_adm:ping(Node) of
	pong->
	    NewQuit=true,
	    NewN=N;
	pang->
	    timer:sleep(Period),
	    NewQuit=Quit,
	    NewN=N+1
    end,
    check_running(Node,Period,NewN,NewQuit).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% 
%% ------------------------------------------------------------------

connect_repeat(Node,Period)->
    net_adm:ping(Node),
    timer:sleep(Period),
    rpc:cast(node(),cmn,connect_repeat,[Node,Period]).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

cmp_vsn_strings(Vsn_A,Vsn_B)->
    % Vsn="2.42.120"
    % -> equal,A less B ,A larger B
    [P2Str_A,P1Str_A,P0Str_A]=string:tokens(Vsn_A,"."),
    [P2Str_B,P1Str_B,P0Str_B]=string:tokens(Vsn_B,"."),
    P2_A=list_to_integer(P2Str_A),
    P2_B=list_to_integer(P2Str_B),
    case {(P2_A<P2_B),(P2_A>P2_B)} of
	{false,false}->
	    P1_A=list_to_integer(P1Str_A),
	    P1_B=list_to_integer(P1Str_B),
	    case {(P1_A<P1_B),(P1_A>P1_B)} of
		{false,false}->
		    P0_A=list_to_integer(P0Str_A),
		    P0_B=list_to_integer(P0Str_B),
		    case {(P0_A<P0_B),(P0_A>P0_B)} of
			{false,false}->
			    Reply=equal;
			{true,false}->
			    Reply=less;
			{false,true} ->
			    Reply=larger
		    end;
		{true,false}->
		    Reply=less;
		{false,true} ->
		    Reply=larger
	    end;
	{true,false}->
	    Reply=less;
	{false,true} ->
	    Reply=larger
    end,
    Reply.
