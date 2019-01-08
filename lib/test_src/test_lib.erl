%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :1
%%% 
%%% -------------------------------------------------------------------
-module(test_lib).
 
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
-export([]).
%%
%% API Functions
%%


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------

start_test()->
    application:start(lib),
    ok.

delete_all_test()->
  %  glurk=upnpc:delete_all_ports(),
    ok.
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------


upnpc_add_test()->
    NewPort=12345,
    Service="Service_1",
    ok = upnpc:add(Service,NewPort),
    ok.

upnpc_member_1_test()->
    NewPort=12345,
    true=upnpc:member(NewPort),
    ok.

upnpc_delete_test()->
    NewPort=12345,
    ok =upnpc:delete(NewPort),
    
    ok.

upnpc_member_2_test()->
    NewPort=12345,
    false=upnpc:member(NewPort),
    ok.
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
num_elem_test()->
    L=[["192.168.0.120",10500,"Service_1","1.0.2",{error,[service,23,'alredy',"file"]}],
       qwet,"asd",2,45],
    0=misc:num_elem([]),
    5= misc:num_elem(L),
    ok.



%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
