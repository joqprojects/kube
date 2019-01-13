%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(josca).

% 
%% --------------------------------------------------------------------
%% Include files1
%% --------------------------------------------------------------------

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
info(FullFileName)->
    {ok,Info}=file:consult(FullFileName),
    {specification,NameStr}=lists:keyfind(specification,1,Info),
    {type,Type}=lists:keyfind(type,1,Info),
    {vsn,VsnStr}=lists:keyfind(vsn,1,Info),
    {Type,NameStr,VsnStr,Info}.

files(Info)->
    R= case lists:keyfind(josca_files,1,Info) of
	   false->
	       [];
	   {josca_files,JoscaFiles}->
	       JoscaFiles
       end,
    R.
type(I)->
    R= case  lists:keyfind(type,1,I) of
	   false->
	       [];
	   {type,Type}->
	       Type
       end,
    R.

exported_services(I)->
    R= case  lists:keyfind(exported_services,1,I) of
	   false->
	       [];
	   {services,Service}->
	       Service
       end,
    R.

num_instances(I)->
    {num_instances,R}=lists:keyfind(num_instances,1,I),
    R.
zone(I)->
    R= case  lists:keyfind(zone,1,I) of
	   false->
	       [];
	   {zone,Zone}->
	       Zone
       end,
    R.

needed_capabilities(I)->
    R= case  lists:keyfind(needed_capabilities,1,I) of
	   false->
	       [];
	   {needed_capabilities,Capa}->
	       Capa
       end,
    R.

geo_red(I)->
    R= case  lists:keyfind(geo_red,1,I) of
	   false->
	       [];
	   {geo_red,GeoRed}->
	       GeoRed
       end,
    R.
dependencies(I)->
    R= case  lists:keyfind(dependencies,1,I) of
	   false->
	       [];
	   {dependencies,Dependencies}->
	       Dependencies
       end,
    R.    

%% --------------------------------------------------------------------------


start_order(Name,Vsn)->
  %  io:format("~p~n",[{?MODULE,?LINE,Name,Vsn}]),
    Result=case if_dns:call("catalog",catalog,read,[Name,Vsn]) of
	       {error,Err}->
		   {error,[?MODULE,?LINE,Err]};
	       {ok,_,JoscaInfo}->
		   Acc=case lists:keyfind(type,1,JoscaInfo) of 
			   {type,application}->
			       [];
			    {type,service} ->
			       {exported_services,[{ServiceId,VsnService}]}=lists:keyfind(exported_services,1,JoscaInfo),
			       [{ServiceId,VsnService}]
		       end,
		  % io:format("~p~n",[{?MODULE,?LINE,Acc}]),
		   {dependencies,JoscaFiles}=lists:keyfind(dependencies,1,JoscaInfo),
		  % io:format("~p~n",[{?MODULE,?LINE,JoscaFiles}]),
		   dfs(JoscaFiles,Acc)
	   end,
    Result.


dfs([],Acc)->
    Acc;
dfs([{Name,Vsn}|T],Acc)->
    io:format("~p~n",[{?MODULE,?LINE,Name,Vsn,Acc}]),    
    case if_dns:call("catalog",catalog,read,[Name,Vsn]) of
	{error,Err}->
	    JoscaFiles=[],
	    Acc1=Acc,
	    [{error,[?MODULE,?LINE,Err]}|Acc];
	{ok,_,JoscaInfo}->
	    case lists:keyfind(type,1,JoscaInfo) of 
		{type,application}->
		    Acc1=Acc;
		{type,service} ->
		    {exported_services,[{ServiceId,VsnService}]}=lists:keyfind(exported_services,1,JoscaInfo),
			 Acc1=[{ServiceId,VsnService}|Acc]
	    end,
	    {dependencies,JoscaFiles}=lists:keyfind(dependencies,1,JoscaInfo)
    end,
    Acc2=dfs(JoscaFiles,Acc1),
    io:format("~p~n",[{?MODULE,?LINE,Acc2}]),
    dfs(T,Acc2).

%%-----------------------------------------------------------------------------
