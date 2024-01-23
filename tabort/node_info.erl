%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(node_info).


-include("node.hrl").
-include("appl.hrl").


%% API
-export([
	find/2,
	 keyfind_deployment/3
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
keyfind_deployment(worker_node,WantedNode,DeploymentInfoList)->
    NodeApplList=[{DeploymentInfo#deployment_info.node_info,
		   DeploymentInfo#deployment_info.appl_info}||DeploymentInfo<-DeploymentInfoList],
    Result=case [#deployment_info{node_info=NodeInfo,
				  appl_info=ApplInfo}||{NodeInfo,ApplInfo}<-NodeApplList,
						       WantedNode=:=NodeInfo#node_info.worker_node] of
	       []->
		   false;
	       NewDeploymentInfoList->
		   NewDeploymentInfoList
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
find([],_Node)->
    false;
find(RunningWorkerNodes,Node)->
    find(RunningWorkerNodes,Node,[]).


find([],_Node,Acc)->
    case Acc of
	[]->
	    false;
	[NodeInfo]->
	    NodeInfo
    end;
find([NodeInfo|T],Node,Acc)->
    if 
	Node=:=NodeInfo#node_info.worker_node ->
	    NewAcc=[NodeInfo],
	    NewT=[];
	true->
	    NewAcc=Acc,
	    NewT=T
    end,
    find(NewT,Node,NewAcc).

%%%===================================================================
%%% Internal functions
%%%===================================================================
