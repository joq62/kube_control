%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(provider_test).

%% API
-export([start/0]).

-define(DeploymentSpec,"test_c50").

-define(LocalResourceTuples,[]).
-define(TargetTypes,[adder,divi]). 

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
    ok=test_0(),
    ok=test_1(),
    ok=test_2(),
      
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
%    init:stop(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_0()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
  %  ok=control_provider_server:set_wanted_state(?DeploymentSpec),
    {error,["Wanted State is already deployed ",control_provider_server,_]}=control_provider_server:set_wanted_state(?DeploymentSpec),
    ok.
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Announce to resource_discovery
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    
    timer:sleep(3000),
  %  [{adder,'2_a@c50'}]=rd:fetch_resources(adder),
    42=rd:call(adder,adder,add,[20,22],5000),
    42=rd:call(adder,add,[20,22],5000),

    
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% start another adder 
    {ok,Id2}=control_provider_server:load_provider("adder"),
    ok=control_provider_server:start_provider(Id2),
    
    timer:sleep(3000),
    [Resource1,Resource2,Resource3]=rd:fetch_resources(adder),
    io:format("Resource1,Resource2 ~p~n",[{Resource1,Resource2,?MODULE,?FUNCTION_NAME}]),
    {adder,Node1}=Resource1,
    pong=rpc:call(Node1,adder,ping,[],5000),
    ok=control_provider_server:stop_provider(Id2),
    ok=control_provider_server:unload_provider(Id2),
    [Resource1,Resource3]=rd:fetch_resources(adder),
    
    
    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    pong=control_provider_server:ping(),
    ok.
