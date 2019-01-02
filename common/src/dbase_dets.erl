%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_dets).



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports
-compile(export_all).
-export([create_dbase/2,delete_dbase/1,exists_dbase/1]).
-export([create/3,read/2,update/3,delete/2,all_objects/1]).
%         delete/1,exists/1,create/2,get/2,store/3,all/1,remove/2]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
match(Id,Pattern)->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    Reply=dets:match(Descriptor,Pattern),
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exists]}
    end,
    Reply.
match_delete(Id,Pattern)->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    Reply=dets:match_delete(Descriptor,Pattern),
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exists]}
    end,
    Reply.



create_dbase(Type,Id) ->
    case filelib:is_file(Id) of 
	true->
	    Reply={ok,dbase_already_exsist};
	false->
	    {ok,Descriptor}=dets:open_file(Id,[{type,Type}]),
	    dets:close(Descriptor),
	    Reply={ok,dbase_created}
    end,
    Reply.


delete_dbase(Id) ->
    case filelib:is_file(Id) of 
	true->
	    file:delete(Id),
	    Reply={ok,dbase_deleted};
	false->
	   Reply={ok,dbase_not_exist}
    end,
    Reply.

exists_dbase(Id) ->
    Reply=filelib:is_file(Id),
    Reply.




create(Key,Value,Id) ->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    case dets:lookup(Descriptor, Key) of
		[]->
		    ok=dets:insert(Descriptor, {Key,Value}),
		    Reply={ok,object_created};
		_X->
		    Reply = {error,[?MODULE,?LINE,Key,already_created]}
	    end,
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exists]}
    end,
    Reply.

update(Key,Value,Id)->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    ok=dets:insert(Descriptor, {Key,Value}),
	    Reply={ok,object_updated},
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exists]}
    end,
    Reply.


read(Key,Id)->
    Reply=l_get(Key,Id),
    Reply.


delete(Key,Id) ->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    case dets:lookup(Descriptor, Key) of
		[]->
		    Reply = {error,[?MODULE,?LINE,Key,no_entry]};
		_X->
		    ok=dets:delete(Descriptor, Key),
		   % [{Key,Value}]=X,
		    Reply={ok,Key}
	    end,
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exits]}
    end,
    Reply.


all_objects(Id) ->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    Key=dets:first(Descriptor),
	    Reply=get_all(Descriptor,Key,[]),
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exits]}
    end,
    Reply.




%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
get_all(_Desc,'$end_of_table',Acc)->
    {ok,Acc};
get_all(Desc,Key,Acc)->  
    Status=dets:lookup(Desc, Key),
    Acc1=lists:append(Status,Acc),
    Key1=dets:next(Desc,Key),
    get_all(Desc,Key1,Acc1).

%% Function: l_get/0
%% Description:local get funtion used by several server functions
%% Returns: {ok,Value}|{error,Errcode}
%% --------------------------------------------------------------------
l_get(Key,Id)->
    case filelib:is_file(Id) of 
	true->
	    {ok,Descriptor}=dets:open_file(Id),
	    case dets:lookup(Descriptor, Key) of
		{error,Reason}->
		    Reply = {error,[?MODULE,?LINE,Key,Id,Reason]};
		ListOfObjects->
		   % io:format("~p~n",[{?MODULE,?LINE,Object}]),
		    Reply=ListOfObjects
	    end,
	    dets:close(Descriptor);
	false->
	    Reply = {error,[?MODULE,?LINE,Id,dbase_dont_exits]}
    end,
    Reply.
