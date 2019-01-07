%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(if_repo).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([divi/2,add/2]).
%%
%% API Functions
%%

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
add(A,B)->
    Reply=try repo:add(A,B) of
	Sum->
	    Sum
    catch
	exit:Reason->
	    Reason
    end,
    %io:format("Reply ~p~n",[Reply]),
    Reply.


divi(A,B)->
    Reply=try repo:divi(A,B) of
	Result->
	    Result
    catch
	exit:Reason->
	    Reason
    end,
   % io:format("Reply ~p~n",[Reply]),
    Reply.
%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
