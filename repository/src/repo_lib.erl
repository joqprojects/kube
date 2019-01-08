%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(repo_lib).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/repository_data.hrl").
%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-compile([export_all]).
%-export([add/2,divi/2]).
%%
%% API Functions
%%
%%---------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------


build_artifact(ServiceId,EbinDir)->
    
    Reply=case filelib:is_dir(EbinDir) of
	      false->
		  {error,[?MODULE,?LINE,'dir eexists',EbinDir]};
	      true->
		  AppFileBaseName=ServiceId++".app",
		  Appfile=filename:join([EbinDir,AppFileBaseName]),
		  {ok,[{application,_,Info}]}=file:consult(Appfile),
		  {modules,Modules}=lists:keyfind(modules,1,Info),
		  ModuleList=build_binaries(Modules,EbinDir,[]),
		  {vsn,Vsn}=lists:keyfind(vsn,1,Info),
		  {ok,AppBinary}=file:read_file(Appfile),
		  Artifact=#artifact{service_id=ServiceId,
				     vsn=Vsn,
				     appfile={AppFileBaseName,AppBinary},
				     modules=ModuleList
				    },
		  {ok,Artifact}
	  end,
    Reply.

build_binaries([],_,ModuleList) ->
    ModuleList;

build_binaries([Module|T],EbinDir,ModuleList)->
 %   io:format("~p~n",[{?MODULE,?LINE,Module,EbinDir}]),
    BaseName=atom_to_list(Module)++".beam",
    FullFileName=filename:join(EbinDir,BaseName),
   case file:read_file(FullFileName) of
       {error,enoent}->
	   NewModuleList=ModuleList,
	   io:format("{error,enoent} ~p~n",[{?MODULE,?LINE,Module,EbinDir}]);
        {ok,Binary}->
	   NewModuleList=[{BaseName,Binary}|ModuleList]
   end,
    build_binaries(T,EbinDir,NewModuleList).    



update_artifact(Artifact,DbaseId)->
    #artifact{service_id=ServiceId,
		       vsn=Vsn,
		       appfile=_,
		       modules=_
		      }=Artifact,
    Reply=case dbase_dets:read({artifact_record,ServiceId},DbaseId) of
	      []->
		  ArtifactRecord=#artifact_record{
				    service_id=ServiceId,
				    latest_vsn=Vsn,
				    vsn_list=[Vsn]
				   },
		  {ok,_}=dbase_dets:create({artifact_record,ServiceId},ArtifactRecord,DbaseId),
		  {ok,_}=dbase_dets:create({artifact,ServiceId,Vsn},Artifact,DbaseId),
		  {ok,artifact_updated};

	[{{artifact_record,ServiceId},ArtifactRecord}] ->
	    case lists:member(Vsn,ArtifactRecord#artifact_record.vsn_list) of
		true->
		    {error,[?MODULE,?LINE,'Service eexists',ServiceId,Vsn]};
		false ->
		    LatestVsn=ArtifactRecord#artifact_record.latest_vsn,
		    VsnList=
		    case repo_lib:cmp_vsn_strings(Vsn,LatestVsn) of
			larger->
			    NewLatestVsn=Vsn;
			_ ->
			    NewLatestVsn=LatestVsn
		    end,			 
		    NewVsnList=[Vsn|ArtifactRecord#artifact_record.vsn_list],
		    NewArtifactRecord=#artifact_record{
				      service_id=ServiceId,
				      latest_vsn=NewLatestVsn,
				      vsn_list=NewVsnList
				     },
		    {ok,_}=dbase_dets:update({artifact_record,ServiceId},NewArtifactRecord,DbaseId),
		    {ok,_}=dbase_dets:create({artifact,ServiceId,Vsn},Artifact,DbaseId),
		    {ok,artifact_updated}
	    end
    end,
    Reply.

read_artifact(ServiceId,latest,DbaseId)-> 
     Reply=case dbase_dets:read({artifact_record,ServiceId},DbaseId) of
	       []->
		   {error,[?MODULE,?LINE,'Service eexists',ServiceId]};
	       [{{artifact_record,ServiceId},ArtifactRecord}] ->
		   Vsn=ArtifactRecord#artifact_record.latest_vsn,
		   read_artifact(ServiceId,Vsn,DbaseId)
	   end,
    Reply;

read_artifact(ServiceId,Vsn,DbaseId)-> 
    Reply=case dbase_dets:read({artifact,ServiceId,Vsn},DbaseId) of
	      []->
		  {error,[?MODULE,?LINE,'Service eexists',ServiceId,Vsn]};
	      [{{artifact,ServiceId,Vsn},Artifact}]->
		  Artifact
	  end,
    Reply.	      



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

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
