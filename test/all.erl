%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=cluster_test:start(),
    ok=application_test:start(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=kube_control:start(),
    pong=log:ping(),
    pong=rd:ping(),
    pong=etcd:ping(),    
    pong=kube_control:ping(),
    ok.
