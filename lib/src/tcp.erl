%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(tcp).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-include("kube/include/trace_debug.hrl").
-include("kube/include/tcp.hrl").
-include("certificate/cert.hrl").

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([call/4,call/3,cast/3,server_seq/1,server_parallel/1,par_connect/1]).


%%
%% API Function
%%
% call(RecIpAddr,RecPort,{M,F,A},{SenderIpAddr,SenderPort,SenderModule,SenderLine})->
call(IpAddr,Port,{os,cmd,A},SenderInfo)->
    send_call(IpAddr,Port,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],SenderInfo);

call(IpAddr,Port,{M,F,A},SenderInfo)->
    send_call(IpAddr,Port,[{M,F,A},?KEY_MSG],SenderInfo).

send_call(Addr,Port,Msg,SenderInfo)->
    case gen_tcp:connect(Addr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	  %  io:format("ok Socket  ~p~n",[{?MODULE,?LINE,Addr,Port,Msg,inet:socknames(Socket)}]),
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
	    receive
		{tcp,Socket,Bin}->
		    Result=binary_to_term(Bin),
		    %io:format("send Result ~p~n",[{?MODULE,?LINE,Result,inet:socknames(Socket)}]),	    
		    gen_tcp:close(Socket);
		{error,Err} ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg}]),
		    Result={error,[?MODULE,?LINE,Err,SenderInfo]},
		    gen_tcp:close(Socket)
	    after ?TIMEOUT_TCPCLIENT ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,time_out,Addr,Port,Msg,SenderInfo}]),
		    Result={error,[?MODULE,?LINE,tcp_timeout,Addr,Port,Msg,SenderInfo]},
		    gen_tcp:close(Socket)
	    end;
	{error,Err} ->
	    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg,SenderInfo}]),
	    Result={error,{Err,?MODULE,?LINE}}
    end,	
    Result.

%%------------------------------------------------------------------------------------------------    
    

call(IpAddr,Port,{os,cmd,A})->
    send_call(IpAddr,Port,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG]);

call(IpAddr,Port,{M,F,A})->
    send_call(IpAddr,Port,[{M,F,A},?KEY_MSG]).

send_call(Addr,Port,Msg)->
    case gen_tcp:connect(Addr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	  %  io:format("ok Socket  ~p~n",[{?MODULE,?LINE,Addr,Port,Msg,inet:socknames(Socket)}]),
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
	    receive
		{tcp,Socket,Bin}->
		    Result=binary_to_term(Bin),
		    %io:format("send Result ~p~n",[{?MODULE,?LINE,Result,inet:socknames(Socket)}]),	    
		    gen_tcp:close(Socket);
		{error,Err} ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg}]),
		    Result={error,[?MODULE,?LINE,Err]},
		    gen_tcp:close(Socket)
	    after ?TIMEOUT_TCPCLIENT ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,time_out,Addr,Port,Msg}]),
		    Result={error,[?MODULE,?LINE,tcp_timeout,Addr,Port,Msg]},
		    gen_tcp:close(Socket)
	    end;
	{error,Err} ->
	    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg}]),
	    Result={error,{Err,?MODULE,?LINE}}
    end,	
    Result.

cast(IpAddr,Port,{os,cmd,A})->
    send_cast(IpAddr,Port,[{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG]);
cast(IpAddr,Port,{M,F,A})->
    send_cast(IpAddr,Port,[{M,F,A},?KEY_MSG]).


send_cast(Addr,Port,Msg)->
   % io:format(" ~p~n",[{?MODULE,?LINE,Msg}]),
    case gen_tcp:connect(Addr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	    Result=ok,
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
 	    gen_tcp:close(Socket);
%	    _Pid=spawn(fun()-> cast_local(Socket,Msg) end);
	{error,Err} ->
	    Result={error,{Err,?MODULE,?LINE}}
    end,	
    Result.   

% Receive part
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
server_seq(Port)->
    {ok, LSock}=gen_tcp:listen(Port,?SERVER_SETUP),  
    seq_loop(LSock).

seq_loop(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    single(Socket),
    seq_loop(LSock).

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
server_parallel(Port)->
    {ok, LSock}=gen_tcp:listen(Port,?SERVER_SETUP),
    spawn(fun()-> par_connect(LSock) end),
    receive
	wait_for_ever->
	    ok
    end.

par_connect(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    spawn(fun()-> par_connect(LSock) end),
    single(Socket).
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
single(Socket)->
    receive
	{tcp, Socket, RawData}->
	    case binary_to_term(RawData) of
		[{M,F,A},?KEY_MSG]->
		    Reply=rpc:call(node(),M,F,A),
		    gen_tcp:send(Socket,term_to_binary(Reply));
		[{call,{M,F,A}},?KEY_MSG]->
		    Reply=rpc:call(node(),M,F,A),
		    gen_tcp:send(Socket,term_to_binary(Reply));
		[{cast,{M,F,A}},?KEY_MSG]->
		    io:format(" ~p~n",[{?MODULE,?LINE,{cast,{M,F,A}}}]),
		    A=rpc:cast(node(),M,F,A),
		    io:format("Error ~p~n",[{?MODULE,?LINE,A}]);
		Err->
		    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
	    end;
	{tcp_closed,Socket} ->
	    exit
    end.
