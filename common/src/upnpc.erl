%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%

%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(upnpc).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("common/include/cmn_nodes.hrl").
%% --------------------------------------------------------------------
%% External exports
-compile(export_all).

-define(CHECK_UPNP_TIME,10000).
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




%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {application, template,
%% ------------------------------------------------------------------
get_all_ports()->
    L1=os:cmd("upnpc -l"),
    [_|L2]=lists:reverse(string:tokens(L1,"\n")),
    Key=" i protocol exPort->inAddr:inPort description remoteHost leaseTime",
    L3=get_port_info(Key,L2,false,[]),
    Ports=extract_ports(L3,[]),
    Ports.
delete_all_ports()->
    Ports=upnpc:get_all_ports(),
    [upnpc:delete_port(Port)||Port<-Ports].
    

add(ServiceStr,Port)->
    PortStr=integer_to_list(Port),
    UpnpcCmd="upnpc -e "++ServiceStr++" -r "++PortStr++" TCP ",  
    os:cmd(UpnpcCmd),
    ok.

add_port(ServiceStr,Port)->
    case upnpc:member(Port) of
	false->
	    PortStr=integer_to_list(Port),
	    UpnpcCmd="upnpc -e "++ServiceStr++" -r "++PortStr++" TCP ",  
	    os:cmd(UpnpcCmd),
	    timer:sleep(?CHECK_UPNP_TIME),
	    case upnpc:member(Port) of
		false->
		    {error,[?MODULE,?LINE,'couldnt add port',Port]};
		true->
		    ok
	    end;
	true ->
	    {error,[?MODULE,?LINE,'already exists port',Port]}
    end.
	    

delete(Port)->	    
    PortStr=integer_to_list(Port),
    UpnpcCmd="upnpc -d "++PortStr++" TCP ",  
    os:cmd(UpnpcCmd),
    ok.
    
delete_port(Port)->
    case upnpc:member(Port) of
	true->
	    PortStr=integer_to_list(Port),
	    UpnpcCmd="upnpc -d "++PortStr++" TCP ",  
	    os:cmd(UpnpcCmd),
	    case upnpc:member(Port) of
		true->
		    {error,[?MODULE,?LINE,'couldnt delete port',Port]};
		false->
		    ok
	    end;
	false ->
	    {error,[?MODULE,?LINE,'tried to deltet non existing port',Port]}
    end.
	    
	    
member(Port)->
    L1=os:cmd("upnpc -l"),
    [_|L2]=lists:reverse(string:tokens(L1,"\n")),
    Key=" i protocol exPort->inAddr:inPort description remoteHost leaseTime",
    L3=get_port_info(Key,L2,false,[]),
    Ports=extract_ports(L3,[]),
    lists:member(Port,Ports).

extract_ports([],Acc)->
    Acc;
extract_ports([Elem|T],Acc)->
    [A|_]=string:tokens(Elem,"->"),
    [_,_,PortStr]=string:tokens(A," "),
    NewAcc=[list_to_integer(PortStr)|Acc],
    extract_ports(T,NewAcc).


get_port_info(_Key,_L,true,Acc)->
    Acc;
get_port_info(_Key,[],false,_) ->
    [];
get_port_info(_Key,_L,true,Acc)->
    Acc;
get_port_info(Key,[Elem|T],_Finish,Acc)->
    case Key==Elem of
	true->
	    NewAcc=Acc,
	    NewFinish=true;
	false ->
	    NewAcc=[Elem|Acc],
	    NewFinish=false
    end,
    get_port_info(Key,T,NewFinish,NewAcc).
