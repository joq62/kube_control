%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  2 Dec 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_orchestrator).

-include("node.hrl").
-include("appl.hrl").
-include("control_config.hrl").

%% API
-export([
	 create_workers/0,
	 load_start_infra/2,
	 load_start/2,

	 wanted_state/1,
	 load_start_wanted_state/1
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load_start_wanted_state(LocalWantedAppls)->
    load_start_wanted_state(LocalWantedAppls,[]).

load_start_wanted_state([],Acc)->
    Acc;
load_start_wanted_state([ApplSpec|T],Acc)->    
    Result=case node_ctrl:allocate() of
	       {error,Reason}->
		   {error,Reason};	     
	       {ok,NodeInfo}->
		   case load_start(ApplSpec,NodeInfo) of
		       {error,Reason}->
			   {error,Reason};	     
		       {ok,DeploymentInfo}->
			   {ok,DeploymentInfo}
		   end
	   end,
    load_start_wanted_state(T,[Result|Acc]).    
		

    
	    

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
wanted_state(DeploymentSpec)->
   case etcd_deployment:get_deployment_list(DeploymentSpec) of
       {error,Reason}->
	   {error,Reason};
        {ok,DeploymentList}->
	   {ok,HostName}=net:gethostname(),
	   LocalDeploymentList=[ApplSpec||{ApplSpec,XHostName}<-DeploymentList,
					  HostName=:=XHostName],
	   {ok,LocalDeploymentList}
   end.

%%--------------------------------------------------------------------
%% @doc
%% load and start infra appls
%% @end
%%--------------------------------------------------------------------
load_start_infra(InfraAppls,RunningNodeInfoList)->
    load_start_infra(InfraAppls,RunningNodeInfoList,[]).

load_start_infra([],_RunningNodeInfoList,Acc)->
    Error=[{error,Reason}||{error,Reason}<-Acc],
    case Error of
	[]->
	    OkStart=[DeploymentInfo||{ok,DeploymentInfo}<-Acc],
	    case OkStart of
		[]->
		    {error,["No application was started were created ",?MODULE,?LINE]};
		OkStart->
		    {ok,OkStart}
	    end;
	Error ->
	    {error,["Failed to start workers ",Error,?MODULE,?LINE]}
    end;

load_start_infra(["log"|T],RunningNodeInfoList,Acc)->
    StartResultList=[load_start("log",NodeInfo)||NodeInfo<-RunningNodeInfoList],
    InitLoggerResult=[init_logger_file(DeploymentInfo)||{ok,DeploymentInfo}<-StartResultList],
    ErrorStartResult=[{error,Reason}||{error,Reason}<-StartResultList],
    NewAcc=lists:append([InitLoggerResult,ErrorStartResult,Acc]),
    load_start_infra(T,RunningNodeInfoList,NewAcc);

load_start_infra([ApplSpec|T],RunningNodeInfoList,Acc)->
    StartResult=[load_start(ApplSpec,NodeInfo)||NodeInfo<-RunningNodeInfoList],
    NewAcc=lists:append(StartResult,Acc),
    load_start_infra(T,RunningNodeInfoList,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init_logger_file(DeploymentInfo)->
    NodeInfo=DeploymentInfo#deployment_info.node_info,
    ApplInfo=DeploymentInfo#deployment_info.appl_info,
    WorkerNode=NodeInfo#node_info.worker_node,
    NodeName=NodeInfo#node_info.nodename, 

    %%---------------- Check if need for creating a main log dir
    
    case rpc:call(WorkerNode,filelib,is_dir,[?MainLogDir],5000) of
	{badrpc,Reason}->
	    {error,[badrpc,Reason,?MODULE,?LINE]};
	false->
	    case rpc:call(WorkerNode,file,make_dir,[?MainLogDir],5000) of
		{badrpc,Reason}->
		    {error,[badrpc,Reason,?MODULE,?LINE]};
		{error,Reason}->
		    {error,["Failed to make dir ",Reason,?MODULE,?LINE]};
		ok ->
		    %%---------- create logger files
   		    NodeNodeLogDir=filename:join(?MainLogDir,NodeName),
		    case rpc:call(WorkerNode,log,create_logger,[NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes],5000) of
			{badrpc,Reason1}->
			    {error,[badrpc,Reason1,?MODULE,?LINE]};
			{error,Reason1}->
			    {error,["Failed to create logger file ",Reason1,?MODULE,?LINE]};
			ok ->
			    {ok,DeploymentInfo}
		    end
	    end;
	true ->
	    %%---------- create logger files
	    NodeNodeLogDir=filename:join(?MainLogDir,NodeName),
	    case rpc:call(WorkerNode,log,create_logger,[NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes],5000) of
		{badrpc,Reason1}->
		    {error,[badrpc,Reason1,?MODULE,?LINE]};
		{error,Reason1}->
		    {error,["Failed to create logger file ",Reason1,?MODULE,?LINE]};
		ok ->
		    {ok,DeploymentInfo}
	    end
    end.
    


%%--------------------------------------------------------------------
%% @doc
%% create workers
%% @end
%%--------------------------------------------------------------------
create_workers()->
    Result=case rpc:call(node(),node_ctrl,ping,[],5000) of
	       {badrpc,Reason}->
		   {error,["node_ctrl not available ",badrpc,Reason,?MODULE,?LINE]};
	       pong ->
		   case node_ctrl:node_info_list() of
		       []->
			   {error,["node_ctrl has not created NodeInfo list ",?MODULE,?LINE]};
		       NodeInfoList->
			   io:format("NodeInfoList ~p~n",[{NodeInfoList,?MODULE,?LINE}]),
			   StartResult=[node_ctrl:create_worker(N#node_info.nodename,N#node_info.worker_dir)||N<-NodeInfoList],
			   Error=[{error,Reason}||{error,Reason}<-StartResult],
			   case Error of
			       []->
				   OkStart=[NodeInfo||{ok,NodeInfo}<-StartResult],
				   case OkStart of
				       []->
					   {error,["No worker were created ",?MODULE,?LINE]};
				       OkStart->
					   {ok,OkStart}
				   end;
			       Error ->
				   {error,["Failed to start workers ",Error,?MODULE,?LINE]}
			   end
		   end
	   end,
    Result.
		

%%--------------------------------------------------------------------
%% @doc
%% load and start an application on a specific node 
%% @end
%%--------------------------------------------------------------------			  
load_start(ApplSpec,NodeInfo)->
    case appl_ctrl:load_appl(NodeInfo,ApplSpec) of
	{error,Reason}->
	    {error,Reason};
	{ok,DeploymentInfo}->
	    case appl_ctrl:start_appl(DeploymentInfo) of
		{error,Reason}->
		    {error,Reason};	
		ok->
		    WorkerNode=NodeInfo#node_info.worker_node,
		    case etcd_application:get_app(ApplSpec) of
			{error,Reason}->
			    {error,Reason};
			{ok,App}->
			    case rpc:call(WorkerNode,App,ping,[],6000) of
				{badrpc,Reason}->
				    {error,["Failed to connect to application ",WorkerNode,App,Reason]};
				pong->
				    {ok,DeploymentInfo}
			    end
		    end
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

