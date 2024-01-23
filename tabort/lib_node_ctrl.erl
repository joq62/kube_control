%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_node_ctrl). 


-include("node.hrl").
-include("log.api").
-define(Iterations,100).
%% API
-export([
	 allocate/1,

	 create_worker/1,
	 delete_worker/1,
	 create_node_info/3,
	 
	 is_node_started/1,
	 is_node_stopped/1

	 ]).

%%%===================================================================
%%% API
%%%===================================================================

%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
allocate([])->
    {error,["No WorkerNodes available",?MODULE,?LINE]};
allocate( [NodeInfo|T])->
    {ok,NodeInfo,T++[NodeInfo]}.
    
%%--------------------------------------------------------------------
%% @doc
%% create worker directory and starts one  worker on the host 
%% @end
%%--------------------------------------------------------------------
create_worker(NodeInfo)->
    delete_worker(NodeInfo),
    WorkerDir=NodeInfo#node_info.worker_dir,
    ?LOG_NOTICE("WorkerDir",[WorkerDir]),
   case filelib:is_dir(WorkerDir) of
       true->
	   timer:sleep(1000),
	   delete_worker(NodeInfo),
	   ?LOG_NOTICE("Try to delete again ",[filelib:is_dir(WorkerDir)]);
       false->
	   ok
   end,
    Result=case file:make_dir(WorkerDir) of
	       {error,Reson}->
		   {error,["Failed to create a dir for ",NodeInfo#node_info.worker_dir,Reson,?MODULE,?LINE]};
	       ok ->
		   ErlArgs=" -setcookie "++NodeInfo#node_info.cookie_str,
		   case slave:start(NodeInfo#node_info.hostname,NodeInfo#node_info.nodename,ErlArgs) of
		       {error,{already_running,WorkerNode}}->
			   {error,["Already running ",WorkerNode,NodeInfo,ErlArgs,?MODULE,?LINE]};
		       {error,Reason}->
			   {error,["Failed to start Node ",Reason,NodeInfo,ErlArgs,?MODULE,?LINE]};
		       {ok,_WorkerNode}->
			   {ok,NodeInfo} 
			       
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% create worker directory and starts one  worker on the host 
%% @end
%%--------------------------------------------------------------------
delete_worker(NodeInfo)->
    WorkerNode=NodeInfo#node_info.worker_node,
    WorkerDir=NodeInfo#node_info.worker_dir,
    erlang:monitor_node(WorkerNode,false),
    file:del_dir_r(WorkerDir),
    slave:stop(WorkerNode),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_node_info(NumWorkers,HostName,CookieStr)->
    create_node_info(NumWorkers,HostName,CookieStr,[]).

create_node_info(0,_HostName,_CookieStr,NodeInfoRecord)->
    NodeInfoRecord;
create_node_info(N,HostName,CookieStr,Acc) ->
    NStr=integer_to_list(N),						
    NodeName=NStr++"_"++CookieStr,
    WorkerDir=NStr++"_"++CookieStr,
    WorkerNode=list_to_atom(NodeName++"@"++HostName),
    NodeInfoRecord=#node_info{worker_node=WorkerNode,worker_dir=WorkerDir,nodename=NodeName,
			      hostname=HostName,
			      cookie_str=CookieStr},
    create_node_info(N-1,HostName,CookieStr,[NodeInfoRecord|Acc]).


					 
%%%===================================================================
%%% Internal functions
%%%==================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load_start_infra([],_NodeInfo,Acc)->
    case [{error,Reason}||{error,Reason}<-Acc] of
	[]->
	    Deployments=[Deployment||{ok,Deployment}<-Acc],
	    {ok,Deployments};
	ErrorList->
	    {error,["Failed to init new worker",ErrorList,?MODULE,?LINE]}
    end;

load_start_infra([ApplSpec|T],NodeInfo,Acc) ->
    Result=case appl_ctrl:load_appl(NodeInfo,ApplSpec) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,Deployment}->
		   case appl_ctrl:start_appl(Deployment) of
		       {error,Reason}->
			   {error,Reason};
		       ok->
			  {ok,Deployment}
		   end
	   end,
    load_start_infra(T,NodeInfo,[Result|Acc]).

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
		    timer:sleep(30),
		    false;
		pang->
		    true
	    end,
    is_node_stopped(N-1,Node,Boolean).
