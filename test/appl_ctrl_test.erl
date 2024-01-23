%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(appl_ctrl_test).


-include("node.hrl").
-include("appl.hrl").


-define(TestAppl,"adder").
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

    ok=test0(),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
%    init:stop(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test0()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),    
    {ok,NodeInfo}=node_ctrl:allocate(),
    WorkerNode=NodeInfo#node_info.worker_node,
    
    {ok,Deployment_log}=load_start(NodeInfo,"log"),
    {ok,Deployment_rd}=load_start(NodeInfo,"resource_discovery"),
    
    {ok,Deployment_adder}=load_start(NodeInfo,"adder"),

    42=rpc:call(WorkerNode,adder,add,[20,22],3000),
    io:format("which applications ~p~n",[{rpc:call(WorkerNode,application,which_applications,[],3000),?MODULE,?LINE}]),
    
    ok=stop_unload(Deployment_adder,"adder"),
    {badrpc,_}=rpc:call(WorkerNode,adder,add,[20,22],3000),
    io:format("which applications ~p~n",[{rpc:call(WorkerNode,application,which_applications,[],3000),?MODULE,?LINE}]),

    ok.



%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
stop_unload(DeploymentInfo,ApplSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=appl_ctrl:stop_appl(DeploymentInfo),
    NodeInfo=DeploymentInfo#deployment_info.node_info,
    WorkerNode=NodeInfo#node_info.worker_node,
    
    {ok,App}=etcd_application:get_app(ApplSpec),    
    {badrpc,_}=rpc:call(WorkerNode,App,ping,[],6000),
    
    ok=appl_ctrl:unload_appl(DeploymentInfo),
    ApplInfo=DeploymentInfo#deployment_info.appl_info,
    ApplDir=ApplInfo#appl_info.appl_dir,
    false=filelib:is_dir(ApplDir),
    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load_start(NodeInfo,ApplSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,DeploymentInfo}=appl_ctrl:load_appl(NodeInfo,ApplSpec),
    ok=appl_ctrl:start_appl(DeploymentInfo),

    WorkerNode=NodeInfo#node_info.worker_node,
    {ok,App}=etcd_application:get_app(ApplSpec),    
    pong=rpc:call(WorkerNode,App,ping,[],6000),
    {ok,DeploymentInfo}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_0()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    io:format("nodes() ~p~n",[{nodes(),?MODULE,?FUNCTION_NAME}]),

    %% infra appl
    {ok,DeploymentId}=appl_ctrl:load_appl("resource_discovery"),
    {ok,WorkerNode}=appl_ctrl:start_appl(DeploymentId),

    {ok,DeploymentId}=appl_ctrl:load_appl("adder",DeploymentId),
    {ok,WorkerNode}=appl_ctrl:start_appl(DeploymentId),

    
    
    
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=node_ctrl:ping(),
   
    ok.
