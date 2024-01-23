%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(deployment_info).


-include("node.hrl").
-include("appl.hrl").
%% API
-export([
	 keyfind/3
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

keyfind(worker_node,WantedNode,DeploymentList)->
    keyfind_node(DeploymentList,WantedNode,[]).
    
keyfind_node([],_WantedNode,Acc)->
    Acc;
keyfind_node([Deployment|T],WantedNode,Acc) ->
    NodeInfo=Deployment#deployment.node_info,
    case NodeInfo#node_info.worker_node of
	WantedNode->
	    NewAcc=[Deployment|Acc];
	_ ->
	    NewAcc=Acc
    end,
    keyfind_node(T,WantedNode,NewAcc).

%%%===================================================================
%%% Internal functions
%%%===================================================================
