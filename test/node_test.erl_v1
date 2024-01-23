%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(node_test).

-include("node_record.hrl").
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
%    ok=test_0(),
    ok=test_1(),
    
      
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

  %  glurk=control_node:dbg_get_state(),
    
    io:format("control_node:get_free() ~p~n",[{control_node:get_free(),?MODULE,?FUNCTION_NAME}]),

    io:format("control_node:get_allocated() ~p~n",[{control_node:get_allocated(),?MODULE,?FUNCTION_NAME}]),
    io:format("control_node:get_not_created() ~p~n",[{control_node:get_not_created(),?MODULE,?FUNCTION_NAME}]),
    io:format("control_node:get_deleted() ~p~n",[{control_node:get_deleted(),?MODULE,?FUNCTION_NAME}]),

    glurk.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    D=date(),
   
    [
     {node_record,_,"c50","2_a","2_a","a",free,{D,_},'2_a@c50'},
     {node_record,_,"c50","3_a","3_a","a",free,{D,_},'3_a@c50'},
     {node_record,_,"c50","4_a","4_a","a",free,{D,_},'4_a@c50'},
     {node_record,_,"c50","5_a","5_a","a",free,{D,_},'5_a@c50'},
     {node_record,_,"c50","6_a","6_a","a",free,{D,_},'6_a@c50'},
     {node_record,_,"c50","7_a","7_a","a",free,{D,_},'7_a@c50'},
     {node_record,_,"c50","8_a","8_a","a",free,{D,_},'8_a@c50'},
     {node_record,_,"c50","9_a","9_a","a",free,{D,_},'9_a@c50'}
    ]=control_node:get_free(),
    [{node_record,_,"c50","1_a","1_a","a",allocated,{_,_},'1_a@c50'}]=control_node:get_allocated(),
    []=control_node:get_deleted(),
    []=control_node:get_not_created(),


    % Allocate 

    {ok,AllocateId_2,'2_a@c50',"2_a"}=control_node:allocate(),
    {ok,AllocateId_3,'3_a@c50',"3_a"}=control_node:allocate(),
    [
     {node_record,_,"c50","4_a","4_a","a",free,{D,_},'4_a@c50'},
     {node_record,_,"c50","5_a","5_a","a",free,{D,_},'5_a@c50'},
     {node_record,_,"c50","6_a","6_a","a",free,{D,_},'6_a@c50'},
     {node_record,_,"c50","7_a","7_a","a",free,{D,_},'7_a@c50'},
     {node_record,_,"c50","8_a","8_a","a",free,{D,_},'8_a@c50'},
     {node_record,_,"c50","9_a","9_a","a",free,{D,_},'9_a@c50'}
    ]=control_node:get_free(),
    
    [
     {node_record,_,"c50","3_a","3_a","a",allocated,{D,_},'3_a@c50'},
     {node_record,_,"c50","2_a","2_a","a",allocated,{D,_},'2_a@c50'},
     {node_record,_,"c50","1_a","1_a","a",allocated,{D,_},'1_a@c50'}
    
    ]=control_node:get_allocated(),
    []=control_node:get_deleted(),
    []=control_node:get_not_created(),

    
    % free
    {error,["No matched for allocate_id ",45,control_node,_]}=control_node:free(45),
    ok=control_node:free(AllocateId_2),
    
    [
     {node_record,_,"c50","9_a","9_a","a",free,{_,_},'9_a@c50'},
     {node_record,_,"c50","8_a","8_a","a",free,{_,_},'8_a@c50'},
     {node_record,_,"c50","7_a","7_a","a",free,{_,_},'7_a@c50'},
     {node_record,_,"c50","6_a","6_a","a",free,{_,_},'6_a@c50'},
     {node_record,_,"c50","5_a","5_a","a",free,{_,_},'5_a@c50'},
     {node_record,_,"c50","4_a","4_a","a",free,{_,_},'4_a@c50'},
     
     {node_record,_,"c50","2_a","2_a","a",free,{_,_},'2_a@c50'}
    
    ]=control_node:get_free(),
    
    [
     {node_record,_,"c50","1_a","1_a","a",allocated,{D,_},'1_a@c50'},
     {node_record,_,"c50","3_a","3_a","a",allocated,{D,_},'3_a@c50'}
    ]=control_node:get_allocated(),
    
    []=control_node:get_deleted(),
    []=control_node:get_not_created(),
    
    %% kill node
    slave:stop('8_a@c50'),

    All=[{R#node_record.node,rpc:call(R#node_record.node,application,which_applications,[],5000)}||R<-control_node:get_free()],
    io:format("All ~p~n",[{All,?MODULE,?LINE}]),
    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok.
