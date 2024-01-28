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

-define(TestDeployment,"test_c50").
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
    
    %% Check error input
    {error,_}=kube_control:deploy_application("glurk",?TestHost),
    {error,_}=kube_control:deploy_application(?TestAppId,"glurk"),
    
    %% Check normal case 

    {ok,HostName}=net:gethostname(),
    {ok,DeploymentList}=etcd_deployment:get_deployment_list(?TestDeployment),

    [
     {ok,"adder",'kubelet_a@c50','1_a@c50'},
     {ok,"adder",'kubelet_a@c50','2_a@c50'},
     {ok,"adder",'kubelet_a@c50','3_a@c50'},
     {ok,"divi",'kubelet_a@c50','1_a@c50'},
     {ok,"divi",'kubelet_a@c50','2_a@c50'}
    ] =lists:sort([kube_control:deploy_application(ApplicationId,HostName)||{ApplicationId,HostName}<-DeploymentList]),
    ApplicationList=[{N,rpc:call(N,application,which_applications,[],5000)}||N<-lists:sort(nodes())],
    
    RdTarget=[rd:add_target_resource_type(Type)||Type<-[adder,divi]],
    rd:trade_resources(),
    timer:sleep(3000),

    [{adder,'1_a@c50'},{adder,'2_a@c50'},{adder,'3_a@c50'}]=lists:sort(rd:fetch_resources(adder)),
    [{divi,'1_a@c50'},{divi,'2_a@c50'}]=lists:sort(rd:fetch_resources(divi)),   
    
    2.0=rd:call(divi,divi,[20,10],5000),
    42=rd:call(adder,add,[20,22],5000),
 
    %%Delete divi
    
    {ok,_}=kube_control:remove_application("divi",'1_a@c50'),
    {ok,_}=kube_control:remove_application("divi",'2_a@c50'),
    rd:trade_resources(),
    timer:sleep(3000),
    [{adder,'1_a@c50'},{adder,'2_a@c50'},{adder,'3_a@c50'}]=lists:sort(rd:fetch_resources(adder)),
    [{divi,'1_a@c50'},{divi,'2_a@c50'}]=lists:sort(rd:fetch_resources(divi)),   
    {badrpc,_}=rd:call(divi,divi,[20,10],5000),
    
    


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
