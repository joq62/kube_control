%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(application_test).

-include("node.hrl").
-include("appl.hrl").
-include("control_config.hrl").



-define(ClusterId,"test_cluster").
-define(HostNameNumWorkers,[{"c50",6}]).
-define(TestAppId,"adder").
-define(TestApp,adder).

-define(TestHost,"c50").

%% API
-export([start/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok=setup(),
    ok=deploy_remove_application(),


  %  ok=delete_cluster(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
deploy_remove_application()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    {ok,?TestAppId,WorkerNode}=kube_control:deploy_application(?TestAppId,?TestHost),
    
    pong=rpc:call(WorkerNode,?TestApp,ping,[],5000),
    42=rpc:call(WorkerNode,?TestApp,add,[20,22],5000),
  
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,[{ok,KubeletInfo}]}=kube_control:new_cluster(?ClusterId,?HostNameNumWorkers),
    'kubelet_a@c50'=maps:get(node,KubeletInfo),
    "c50"=maps:get(hostname,KubeletInfo),
    Nodes=lists:sort(nodes()),
    ['1_a@c50','2_a@c50','3_a@c50','4_a@c50','5_a@c50','6_a@c50','kubelet_a@c50']=Nodes,
    ok.
