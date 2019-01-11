%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(catalog_lib).

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
all(Name,DbaseId)->
    Reply=case dbase_dets:read(Name,DbaseId) of
	      []->
		  {error,[?MODULE,?LINE,'eexists',Name]};
	      [{Name,{{latest,_VsnLatest},BinList}}]->
		  BinList;
	      Err ->
		  io:format("p~n~",[{?MODULE,?LINE,Err}]),
		  Err
	  end,
    Reply.

create(Name,Vsn,Bin,DbaseId) ->
    Reply=case dbase_dets:read(Name,DbaseId) of
	      []->
	      dbase_dets:create(Name,{{latest,Vsn},[{Vsn,Bin}]},DbaseId);
	      [{Name,{{latest,_VsnLatest},BinList}}]->
		  case lists:keyfind(Vsn,1,BinList) of
		      false->
			  NewBinList=[{Vsn,Bin}|BinList],
			  UpdatedLatestVsn=find_latest_josca(NewBinList),
		%	  io:format("p~n~",[{?MODULE,?LINE,NewBinList}]),			  
			  dbase_dets:update(Name,{{latest,UpdatedLatestVsn},NewBinList},DbaseId);			  
		      {Vsn,Bin}->
			  {error,[?MODULE,?LINE,'already exists',Name,Vsn]}
		  end
	  end,   
    Reply.

read(Name,Vsn,DbaseId) ->
    Reply=case dbase_dets:read(Name,DbaseId) of
	      []->
		  {error,[?MODULE,?LINE,'eexists',Name,Vsn]};
	      [{Name,{{latest,VsnLatest},BinList}}]->
		  case Vsn of
		      latest->
			  SearchVsn=VsnLatest;
		      Vsn->
			  SearchVsn=Vsn
		  end,
		  case lists:keyfind(SearchVsn,1,BinList) of
		      false->
			  {error,[?MODULE,?LINE,'eexists',Name,Vsn]};
		      {_,Bin}->
			  FileName=Name++".josca",
			  {ok,FileName,Bin}
		  end
	  end,
    Reply.
	      
update(Name,Vsn,Bin,DbaseId) ->
    Reply=case dbase_dets:read(Name,DbaseId) of
	      []->
		  {error,[?MODULE,?LINE,'eexists',Name,Vsn]};
	      [{Name,{{latest,_VsnLatest},BinList}}]->
		  case lists:keyfind(Vsn,1,BinList) of
		      false->
			  {error,[?MODULE,?LINE,'eexists',Name,Vsn]};
		      {Vsn,_}->
			  NewBinList=[{Vsn,Bin}|lists:keydelete(Vsn,1,BinList)],
			  UpdatedLatest=find_latest_josca(NewBinList),
			  dbase_dets:update(Name,{{latest,UpdatedLatest},NewBinList},DbaseId)
		  end
	  end,
    Reply.

delete(Name,Vsn,DbaseId) ->
    Reply=case dbase_dets:read(Name,DbaseId) of
	      []->
		  {error,[?MODULE,?LINE,'eexists',Name,Vsn]};
	      [{Name,{{latest,_VsnLatest},BinList}}]->
		  case lists:keyfind(Vsn,1,BinList) of
		      false->
			  {error,[?MODULE,?LINE,'eexists',Name,Vsn]};
		      {Vsn,_}->
			  NewBinList=lists:keydelete(Vsn,1,BinList),
			  UpdatedLatest=find_latest_josca(NewBinList),
			  dbase_dets:update(Name,{{latest,UpdatedLatest},NewBinList},DbaseId)
		  end
	  end,
    Reply.


find_latest_josca([])->
    {error,[?MODULE,?LINE,'empty josca Binary list']};
find_latest_josca([{Vsn,_}|T])->
    find_latest_josca(T,Vsn).  

find_latest_josca([],VsnLatest)->
    VsnLatest;
find_latest_josca([{Vsn,_}|T],VsnLatest)->
    case cmn:cmp_vsn_strings(Vsn,VsnLatest) of
	less->
	    NewVsnLatest=VsnLatest;
	_ ->
	    NewVsnLatest=Vsn
    end,
find_latest_josca(T,NewVsnLatest).    
