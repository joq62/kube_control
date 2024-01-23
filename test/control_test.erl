%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(control_test).

-include("node.hrl").
-include("appl.hrl").
-include("control_config.hrl").


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
    ok=control_start(),
    ok=rd_test(),
  %  ok=kill_restart_node(),


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
-define(LocalResourceTuples,[]).
-define(TargetTypes,[adder]).

rd_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    ok=rd:detect_target_resources(?TargetTypes,?MaxDetectTime),
    
    42=rd:call(adder,add,[20,22],5000),
    
    NodeInfoList=lists:sort(node_ctrl:node_info_list()),
    [
     {node_info,'1_a@c50',"1_a","1_a","c50","a"},
     {node_info,'2_a@c50',"2_a","2_a","c50","a"},
     {node_info,'3_a@c50',"3_a","3_a","c50","a"},
     {node_info,'4_a@c50',"4_a","4_a","c50","a"},
     {node_info,'5_a@c50',"5_a","5_a","c50","a"},
     {node_info,'6_a@c50',"6_a","6_a","c50","a"},
     {node_info,'7_a@c50',"7_a","7_a","c50","a"},
     {node_info,'8_a@c50',"8_a","8_a","c50","a"},
     {node_info,'9_a@c50',"9_a","9_a","c50","a"}
    ]=NodeInfoList,
    
    WhichAppl=[{N#node_info.worker_node,rpc:call(N#node_info.worker_node,application,which_applications,[],5000)}||N<-NodeInfoList],
    
    io:format("WhichAppl ~p~n",[{WhichAppl,?MODULE,?LINE}]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
control_start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    NodeInfoList=lists:sort(node_ctrl:node_info_list()),
    [
     {node_info,'1_a@c50',"1_a","1_a","c50","a"},
     {node_info,'2_a@c50',"2_a","2_a","c50","a"},
     {node_info,'3_a@c50',"3_a","3_a","c50","a"},
     {node_info,'4_a@c50',"4_a","4_a","c50","a"},
     {node_info,'5_a@c50',"5_a","5_a","c50","a"},
     {node_info,'6_a@c50',"6_a","6_a","c50","a"},
     {node_info,'7_a@c50',"7_a","7_a","c50","a"},
     {node_info,'8_a@c50',"8_a","8_a","c50","a"},
     {node_info,'9_a@c50',"9_a","9_a","c50","a"}
    ]=NodeInfoList,
    
    WhichAppl=[{N#node_info.worker_node,rpc:call(N#node_info.worker_node,application,which_applications,[],5000)}||N<-NodeInfoList],
    
    io:format("WhichAppl ~p~n",[{WhichAppl,?MODULE,?LINE}]),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill_restart_node()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Check correct creation of NodeIfo and create_worker
    NodeInfoList=lists:sort(node_ctrl:node_info_list()),
    [
     {node_info,'1_a@c50',"1_a","1_a","c50","a"},
     {node_info,'2_a@c50',"2_a","2_a","c50","a"},
     {node_info,'3_a@c50',"3_a","3_a","c50","a"},
     {node_info,'4_a@c50',"4_a","4_a","c50","a"},
     {node_info,'5_a@c50',"5_a","5_a","c50","a"},
     {node_info,'6_a@c50',"6_a","6_a","c50","a"},
     {node_info,'7_a@c50',"7_a","7_a","c50","a"},
     {node_info,'8_a@c50',"8_a","8_a","c50","a"},
     {node_info,'9_a@c50',"9_a","9_a","c50","a"}
    ]=NodeInfoList,
    NodeInfoList=lists:sort(node_ctrl:running_worker_nodes()),
    
    %% killl '3_a@c50'
   % rpc:call('3_a@c50',init,stop,[],5000),
    slave:stop('3_a@c50'),
    pang=net_adm:ping('3_a@c50'),
    timer:sleep(100),
    pong=net_adm:ping('3_a@c50'),
    NodeInfoList=lists:sort(node_ctrl:running_worker_nodes()),
  %  NodeInfoList=node_ctrl:running_worker_nodes(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
check_nodes_init()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Check correct creation of NodeIfo and create_worker
    NodeInfoList=lists:sort(node_ctrl:node_info_list()),
    [
     {node_info,'1_a@c50',"1_a","1_a","c50","a"},
     {node_info,'2_a@c50',"2_a","2_a","c50","a"},
     {node_info,'3_a@c50',"3_a","3_a","c50","a"},
     {node_info,'4_a@c50',"4_a","4_a","c50","a"},
     {node_info,'5_a@c50',"5_a","5_a","c50","a"},
     {node_info,'6_a@c50',"6_a","6_a","c50","a"},
     {node_info,'7_a@c50',"7_a","7_a","c50","a"},
     {node_info,'8_a@c50',"8_a","8_a","c50","a"},
     {node_info,'9_a@c50',"9_a","9_a","c50","a"}
    ]=NodeInfoList,
    StartResult=[node_ctrl:create_worker(N#node_info.nodename,N#node_info.worker_dir)||N<-NodeInfoList],
    NodeInfoList=lists:sort([N||{ok,N}<-StartResult]),
    
    NodeInfoList=lists:sort(node_ctrl:running_worker_nodes()),
    
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_0()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    NodeName="test_node_1", 
    NodeDir="test_node_1", 
    file:del_dir_r(NodeDir),
    {ok,Nir}=node_ctrl:create_worker(NodeName, NodeDir),
    true=filelib:is_dir(NodeDir),
    pong=net_adm:ping(Nir#node_info.worker_node),
    true=lists:member(Nir#node_info.worker_node,nodes()),
   
    slave:stop(Nir#node_info.worker_node),
    timer:sleep(1000),

    true=filelib:is_dir(Nir#node_info.worker_dir),
    pong=net_adm:ping(Nir#node_info.worker_node),
    true=lists:member(Nir#node_info.worker_node,nodes()),

    ok=node_ctrl:delete_worker(Nir),
    false=filelib:is_dir(Nir#node_info.worker_node),
    pang=net_adm:ping(Nir#node_info.worker_node),
    false=lists:member(Nir#node_info.worker_node,nodes()),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=node_ctrl:ping(),
    pong=orchestrator:ping(),
   
    ok.
