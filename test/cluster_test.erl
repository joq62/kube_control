%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(cluster_test).

-include("node.hrl").
-include("appl.hrl").
-include("control_config.hrl").



-define(ClusterId,"test_cluster").
-define(HostNameNumWorkers,[{"c50",6}]).

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
    ok=new_delete_cluster(),


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
new_delete_cluster()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% Check kube_control:delete_cluster(?ClusterId) 
    {error,["Cluster is not created "]}=kube_control:delete_cluster(?ClusterId),

    %% Check kube_control:new_cluster(?ClusterId,?HostNameNumWorkers),
    {ok,[{ok,KubeletInfo}]}=kube_control:new_cluster(?ClusterId,?HostNameNumWorkers),
    'kubelet_a@c50'=maps:get(node,KubeletInfo),
    "c50"=maps:get(hostname,KubeletInfo),

    %% Check kube_control:delete_cluster("glurk_id") works for wrong cluster Id
    {error,["Unknown ClusterId, The current ClusterId is = ",?ClusterId]}=kube_control:delete_cluster("glurk_id"),
    
    %% Check kube_control:new_cluster(?ClusterId,?HostNameNumWorkers) doesnt creates a new cluster
    {error,["Cluster is already created with the cluster name ",?ClusterId]}=kube_control:new_cluster(?ClusterId,?HostNameNumWorkers),
    {error,["Cluster is already created with the cluster name ",?ClusterId]}=kube_control:new_cluster("clusterglurk",?HostNameNumWorkers),

    io:format("KubeletNode and workers are started ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Check that kubelet and worker nodes are started correctly
    Nodes=lists:sort(nodes()),
    ['1_a@c50','2_a@c50','3_a@c50','4_a@c50','5_a@c50','6_a@c50','kubelet_a@c50']=Nodes,
    
    io:format("KubeletNode and workers are started ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    WhichApplications=[{N,rpc:call(N,application,which_applications,[],5000)}||N<-Nodes],
    [
      {'1_a@c50',[{log,"An OTP application","0.1.0"},
		  {rd,"An OTP application","0.1.0"},
		  {stdlib,"ERTS  CXC 138 10","3.17"},
		  {kernel,"ERTS  CXC 138 10","8.2"}]},
      {'2_a@c50',[{log,"An OTP application","0.1.0"},
		  {rd,"An OTP application","0.1.0"},
		  {stdlib,"ERTS  CXC 138 10","3.17"},
		  {kernel,"ERTS  CXC 138 10","8.2"}]},
      {'3_a@c50',[{log,"An OTP application","0.1.0"},
		  {rd,"An OTP application","0.1.0"},
		  {stdlib,"ERTS  CXC 138 10","3.17"},
		  {kernel,"ERTS  CXC 138 10","8.2"}]},
      {'4_a@c50',[{log,"An OTP application","0.1.0"},
		  {rd,"An OTP application","0.1.0"},
		  {stdlib,"ERTS  CXC 138 10","3.17"},
		  {kernel,"ERTS  CXC 138 10","8.2"}]},
      {'5_a@c50',[{log,"An OTP application","0.1.0"},
		  {rd,"An OTP application","0.1.0"},
		  {stdlib,"ERTS  CXC 138 10","3.17"},
		  {kernel,"ERTS  CXC 138 10","8.2"}]},
      {'6_a@c50',[{log,"An OTP application","0.1.0"},
		  {rd,"An OTP application","0.1.0"},
		  {stdlib,"ERTS  CXC 138 10","3.17"},
		  {kernel,"ERTS  CXC 138 10","8.2"}]},
      {'kubelet_a@c50',[{kubelet,"An OTP application","0.1.0"},
			{rd,"An OTP application","0.1.0"},
			{log,"An OTP application","0.1.0"},
			{stdlib,"ERTS  CXC 138 10","3.17"},
			{kernel,"ERTS  CXC 138 10","8.2"}]}
    ]=WhichApplications,


    %% Check kube_control:delete_cluster("glurk_id") works for wrong cluster Id
    {error,["Unknown ClusterId, The current ClusterId is = ",?ClusterId]}=kube_control:delete_cluster("glurk_id"),

    % Check kube_control:delete_cluster(?ClusterId) 
    {ok,[{true,DelResult}]}=kube_control:delete_cluster(?ClusterId),
    'kubelet_a@c50'=maps:get(node,DelResult),
    "c50"=maps:get(hostname,DelResult),
    
    %% Check kube_control:delete_cluster("glurk_id") works for wrong cluster Id
    {error,["Cluster is not created "]}=kube_control:delete_cluster("glurk_id"),
   
    %% Check kube_control:delete_cluster(?ClusterId) 
    {error,["Cluster is not created "]}=kube_control:delete_cluster(?ClusterId),

      
    %% Cehck that Kubelet and workernodes are stopped
    timer:sleep(2000),
    []=nodes(),
    io:format("KubeletNode and workers are stopped ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok.
