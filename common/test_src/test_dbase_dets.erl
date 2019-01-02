%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(test_dbase_dets).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------

%%
%% API Functions
%%

-record(service_register,
	{
	  brd_ip,
	  brd_port,
	  service_node,
	  serviceStr,
	  vsnStr
	}).

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------

dbase_dets_dbase_set_test()->
    Id=test_set,
    Type=set,
    false=dbase_dets:exists_dbase(Id),
    {ok,dbase_created}=dbase_dets:create_dbase(Type,Id),
    true=dbase_dets:exists_dbase(Id),
    {ok,dbase_already_exsist}=dbase_dets:create_dbase(Type,Id),
    {ok,dbase_not_exist}=dbase_dets:delete_dbase(glurk),
    {ok,dbase_deleted}=dbase_dets:delete_dbase(Id),
    {ok,dbase_not_exist}=dbase_dets:delete_dbase(Id),
    {ok,dbase_created}=dbase_dets:create_dbase(Type,Id),
    ok.
dbase_dets_set_test()->
    Id=test_set,
    {ok,object_created}=dbase_dets:create(key1,value1,Id),
    {ok,[{key1,value1}]}=dbase_dets:read(key1,Id),
    {ok,[]}=dbase_dets:read(glurk,Id),
    {error,[dbase_dets,_Line,key1,already_created]}=dbase_dets:create(key1,value1,Id),
    {ok,[{key1,value1}]}=dbase_dets:read(key1,Id),
    {ok,object_updated}=dbase_dets:update(key1,value11,Id),
    {ok,[{key1,value11}]}=dbase_dets:read(key1,Id),
    {ok,key1}=dbase_dets:delete(key1,Id),
    {ok,[]}=dbase_dets:read(key1,Id),

    DnsReg= #service_register{brd_ip="192.168.0.10", brd_port=11000,service_node='infra@joq-desktop',
			      serviceStr="service",vsnStr="1.0.0"
			      },
    {ok,object_created}=dbase_dets:create(keyRecord,DnsReg,Id),
    {ok,[{keyRecord,DnsDbase}]}=dbase_dets:read(keyRecord,Id),
    "192.168.0.10"=DnsDbase#service_register.brd_ip,
    {ok,dbase_deleted}=dbase_dets:delete_dbase(Id),
    ok.
dbase_dets_dbase_bag_test()->
    Id=test_bag,
    Type=bag,
    {ok,dbase_created}=dbase_dets:create_dbase(Type,Id),
    {ok,dbase_already_exsist}=dbase_dets:create_dbase(Type,Id),
    {ok,dbase_not_exist}=dbase_dets:delete_dbase(glurk),
    {ok,dbase_deleted}=dbase_dets:delete_dbase(Id),
    {ok,dbase_not_exist}=dbase_dets:delete_dbase(Id),

    {ok,dbase_created}=dbase_dets:create_dbase(Type,Id),
    ok.
dbase_dets_bag_test()->
    Id=test_bag,
    {ok,object_created}=dbase_dets:create(key1,value1,Id),
    {ok,[{key1,value1}]}=dbase_dets:read(key1,Id),
    {ok,[]}=dbase_dets:read(glurk,Id),
    {error,[dbase_dets,_Line,key1,already_created]}=dbase_dets:create(key1,value1,Id),
    {ok,[{key1,value1}]}=dbase_dets:read(key1,Id),
    {ok,object_updated}=dbase_dets:update(key1,value11,Id),
    {ok,[{key1,value1},{key1,value11}]}=dbase_dets:read(key1,Id),
    {ok,key1}=dbase_dets:delete(key1,Id),
    {ok,[]}=dbase_dets:read(key1,Id),
    {ok,dbase_deleted}=dbase_dets:delete_dbase(Id),
    ok.

dbase_dets_dbase_duplicated_bag_test()->
    Id=test_duplicated_bag,
    Type=bag,
    {ok,dbase_created}=dbase_dets:create_dbase(Type,Id),
    {ok,dbase_already_exsist}=dbase_dets:create_dbase(Type,Id),
    {ok,dbase_not_exist}=dbase_dets:delete_dbase(glurk),
    {ok,dbase_deleted}=dbase_dets:delete_dbase(Id),
    {ok,dbase_not_exist}=dbase_dets:delete_dbase(Id),

    {ok,dbase_created}=dbase_dets:create_dbase(Type,Id),
    ok.
dbase_dets_duplicated_bag_test()->
    Id=test_duplicated_bag,
    {ok,object_created}=dbase_dets:create(key1,value1,Id),
    {ok,[{key1,value1}]}=dbase_dets:read(key1,Id),
    {ok,[]}=dbase_dets:read(glurk,Id),
    {error,[dbase_dets,_Line,key1,already_created]}=dbase_dets:create(key1,value1,Id),
    {ok,[{key1,value1}]}=dbase_dets:read(key1,Id),
    {ok,object_updated}=dbase_dets:update(key1,value11,Id),
    {ok,[{key1,value1},{key1,value11}]}=dbase_dets:read(key1,Id),
    {ok,key1}=dbase_dets:delete(key1,Id),
    {ok,[]}=dbase_dets:read(key1,Id),
    {ok,dbase_deleted}=dbase_dets:delete_dbase(Id),
    ok.


clean_up_stop_test()->
    {error,enoent}=file:delete(test_bag),
    {error,enoent}=file:delete(test_set),
    {error,enoent}=file:delete("glurk.dets"),
    ok.

stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.



