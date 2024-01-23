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
-module(dependent_apps).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


-define(MainLogDir,"logs").
-define(LocalLogDir,"to_be_changed.logs").
-define(LogFile,"logfile").
-define(MaxNumFiles,10).
-define(MaxNumBytes,100000).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=start_dependet_apps(),
    ok=setup(),
    ok.
 
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_dependet_apps()->
      io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    file:del_dir_r(?MainLogDir),
    ok=application:start(log),
    pong=log:ping(),
    LocalLogDir=atom_to_list(node())++".logs",
    ok=log:create_logger(?MainLogDir,LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes),
    
    ok=application:start(rd),
    pong=rd:ping(),
    ok=application:start(etcd),
    pong=etcd:ping(),

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.
