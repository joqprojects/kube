%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :1
%%% 
%%% -------------------------------------------------------------------
-module(test_repo_cmn).
  
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


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
repo_cmn_create_service_test()->
    ServiceId="adder",
    SrcDir="adder/ebin",
    [{tar_File,TarBaseName,TarFileBinary},
     {app_file,AppBaseName,AppVsn,_AppBinary},
     {josca_file,_JoscaBaseName,_JoscaVsn,_JoscaBinary}]=repo_cmn:user_create_service_tar_file(ServiceId,SrcDir),
    
    file:write_file(TarBaseName,TarFileBinary),
    % Create new service dir - first check if exists
    ServiceDir=ServiceId++"-"++AppVsn,
    case filelib:is_dir(ServiceDir) of
	true->
	    os:cmd("rm -r "++ServiceDir);
	false ->
	    ok
    end,
    file:make_dir(ServiceDir),
    repo_cmn:unix_untar(TarBaseName,TarFileBinary,ServiceDir),
    {ok,_}=file:consult(filename:join([ServiceDir,ServiceId,"ebin",ServiceId++".app"])),
    os:cmd("rm -r "++ServiceDir),
    ok.




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
