%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control).

-define(Iterations,100).
%% API
-export([
	 create_dir/1,
	 delete_dir/1,
	 create_dirs/1,
	 delete_dirs/1,
	 define_workers/3,
	 start_node/1,
	 start_nodes/1,
	 stop_node/1,	 
	 is_node_started/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_dir(Dir)->
    file:make_dir(Dir).

create_dirs(WorkerDefinitions)->
    create_dirs(WorkerDefinitions,[]).

create_dirs([],Result)->
    Result;
create_dirs([WorkerDefinition|T],Acc)->
    Dir=proplists:get_value(dir,WorkerDefinition),
    R=file:make_dir(Dir),
    create_dirs(T,[{R,Dir}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
delete_dir(Dir)->
    file:del_dir_r(Dir).

delete_dirs(WorkerDefinitions)->
    delete_dirs(WorkerDefinitions,[]).


delete_dirs([],Result)->
    Result;
delete_dirs([WorkerDefinition|T],Acc)->
    Dir=proplists:get_value(dir,WorkerDefinition),
    R=file:del_dir_r(Dir),
    delete_dirs(T,[{R,Dir}|Acc]).



%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
define_workers(NumWorkers,CookieStr,HostName)->
    define_workers(NumWorkers,CookieStr,HostName,[]).
    
define_workers(0,_CookieStr,_HostName,Workers)->
    Workers;
define_workers(N,CookieStr,HostName,Acc) ->
    NStr=integer_to_list(N),						
    NodeName=NStr++"_"++CookieStr,
    Dir=NStr++"_"++CookieStr,
    Node=list_to_atom(NodeName++"@"++HostName),
    define_workers(N-1,CookieStr,HostName,[[{nodename,NodeName},{dir,Dir},{node,Node}]|Acc]).		 

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start_nodes(WorkerDefinitions)->
    start_nodes(WorkerDefinitions,[]).

start_nodes([],Result)->
    Result;
start_nodes([WorkerDefinition|T],Acc)->
    NodeName=proplists:get_value(nodename,WorkerDefinition),
    Node=proplists:get_value(node,WorkerDefinition),
    Result=case start_node(NodeName) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,Node} ->
		   {ok,Node};
	       {ok,WrongNode} ->
		   {error,["wrong Node ",WrongNode,NodeName,Node,?MODULE,?LINE]}
	   end,
    start_nodes(T,[Result|Acc]).

start_node(NodeName)->
    {ok,Host}=net:gethostname(),
    CookieStr=atom_to_list(erlang:get_cookie()),
    ErlArgs=" -setcookie "++CookieStr,
    Result=case slave:start(Host,NodeName,ErlArgs) of
	       {error,{already_running, ProviderNode}}->
		   {error,["Already running ",?MODULE,?LINE,ProviderNode,NodeName,CookieStr,ErlArgs]};
	       {error,Reason}->
		   {error,["Failed to start Node ",?MODULE,?LINE,Reason,NodeName,CookieStr,ErlArgs]};
	       {ok,ProviderNode}->	
		   {ok,ProviderNode}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
stop_node(ProviderNode)->
    slave:stop(ProviderNode),
    is_node_stopped(ProviderNode).   

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_node_started(Node)->
    is_node_started(?Iterations,Node,false).

is_node_started(_N,_Node,true)->
    true;
is_node_started(0,_Node,Boolean) ->
    Boolean;
is_node_started(N,Node,_) ->
  %  io:format(" ~p~n",[{N,Node,erlang:get_cookie(),?MODULE,?LINE}]),
    Boolean=case net_adm:ping(Node) of
		pang->
		    timer:sleep(30),
		    false;
		pong->
		    true
	    end,
    is_node_started(N-1,Node,Boolean).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_node_stopped(Node)->
    is_node_stopped(?Iterations,Node,false).

is_node_stopped(_N,_Node,true)->
    true;
is_node_stopped(0,_Node,Boolean) ->
    Boolean;
is_node_stopped(N,Node,_) ->
    Boolean=case net_adm:ping(Node) of
		pong->
		    timer:sleep(500),
		    false;
		pang->
		    true
	    end,
    is_node_stopped(N-1,Node,Boolean).
