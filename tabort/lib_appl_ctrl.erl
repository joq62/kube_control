%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_appl_ctrl).


-include("node.hrl").
-include("appl.hrl").
% -include("infra_appls.hrl").



%% API
-export([
	 load_appl/2,
	 unload_appl/1,
	 start_appl/1,
	 stop_appl/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load_appl(NodeInfo,ApplSpec)->
    Result=case etcd_application:get_app(ApplSpec) of
	       {error,Reason}->
		   {error,[ApplSpec,Reason]};
	       {ok,App}->
		   case etcd_application:get_git_path(ApplSpec) of
		       {error,Reason}->
			   {error,[ApplSpec,Reason]};
		       {ok,GitPath}->
			   WorkerNode=NodeInfo#node_info.worker_node,
			   WorkerDir=NodeInfo#node_info.worker_dir,
			   case load(WorkerNode,WorkerDir,ApplSpec,App,GitPath) of
			       {error,Reason}->
				   {error,[ApplSpec,Reason]};
			       {ok,ApplDir}->
				   erlang:monitor_node(WorkerNode,true),
				   ApplInfo=#appl_info{appl_spec=ApplSpec,
							     appl_dir=ApplDir},
				   
				   {ok,#deployment_info{
					  node_info=NodeInfo,
					  appl_info=ApplInfo}}
			   end
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
unload_appl(DeploymentInfo)->
    NodeInfo=DeploymentInfo#deployment_info.node_info,
    WorkerNode=NodeInfo#node_info.worker_node,
    
    ApplInfo=DeploymentInfo#deployment_info.appl_info,
    ApplSpec=ApplInfo#appl_info.appl_spec,
    Result=case etcd_application:get_app(ApplSpec) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,App} ->
		   case rpc:call(WorkerNode,application,unload,[App],5*5000) of
		       {badrpc,Reason}->
			   {error,["Failed to unload  Application on Node ",App,WorkerNode,Reason,?MODULE,?LINE]};
		       {error,Reason}->
			   {error,["Failed to unload Application on Node ",App,WorkerNode,Reason,?MODULE,?LINE]};
		       ok->
			   ApplDir=ApplInfo#appl_info.appl_dir,
			   case rpc:call(WorkerNode,file,del_dir_r,[ApplDir],5000) of
			       {badrpc,Reason}->
				   {error,["Failed to delete ApplDir on Node ",ApplDir,App,WorkerNode,Reason,?MODULE,?LINE]};
			       {error,Reason}->
				   {error,["Failed to delete ApplDir on Node ",ApplDir,App,WorkerNode,Reason,?MODULE,?LINE]};
			       ok->
				   ok
			   end
		   end
	   end,
    Result.
   
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start_appl(DeploymentInfo)->
   
    NodeInfo=DeploymentInfo#deployment_info.node_info,
    WorkerNode=NodeInfo#node_info.worker_node,
    
    ApplInfo=DeploymentInfo#deployment_info.appl_info,
    ApplSpec=ApplInfo#appl_info.appl_spec,

    Result=case etcd_application:get_app(ApplSpec) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,App} ->
		   case rpc:call(WorkerNode,application,start,[App,permanent],5*5000) of
		       {badrpc,Reason}->
			   {error,["badrpc Failed to start Application on Node ",App,WorkerNode,Reason,?MODULE,?LINE]};
		       {error,Reason}->
			   {error,["Failed to start Application on Node ",App,WorkerNode,Reason,?MODULE,?LINE]};
		       ok->
			   ok
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
stop_appl(DeploymentInfo)->
    NodeInfo=DeploymentInfo#deployment_info.node_info,
    WorkerNode=NodeInfo#node_info.worker_node,
    
    ApplInfo=DeploymentInfo#deployment_info.appl_info,
    ApplSpec=ApplInfo#appl_info.appl_spec,
    ApplicationDir=ApplInfo#appl_info.appl_dir,
    Result=case etcd_application:get_app(ApplSpec) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,App} ->
		   case rpc:call(WorkerNode,application,stop,[App],5000) of
		       {badrpc,Reason}->
			   {error,["Failed to stop Application on Node ",App,WorkerNode,Reason,?MODULE,?LINE]};
		       {error,Reason}->
			   {error,["Failed to stop Application on Node ",App,WorkerNode,Reason,?MODULE,?LINE]};
		       ok->
			   Ebin=filename:join(ApplicationDir,"ebin"),
			   Priv=filename:join(ApplicationDir,"priv"),		   
			   rpc:call(WorkerNode,code,del_path,[Ebin],5000),
			   rpc:call(WorkerNode,code,del_path,[Priv],5000),
			   ok
		   end
	   end,
    Result.
			    


    
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load(WorkerNode,WorkerDir,Application,App,GitPath)->
    %% Create dir for provider in IaasDir
    ApplicationDir=filename:join(WorkerDir,Application),
    rpc:call(WorkerNode,file,del_dir_r,[ApplicationDir],5000),
    Result=case  rpc:call(WorkerNode,file,make_dir,[ApplicationDir],5000) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,?MODULE,?LINE]};
	       {error,Reason}->
		   {error,["Failed to create ApplicationDir ",ApplicationDir,?MODULE,?LINE,Reason]};
	       ok->
		   case rpc:call(node(),os,cmd,["git clone "++GitPath++" "++ApplicationDir],3*5000) of
		       {badrpc,Reason}->
			   {error,["Failed to git clone ",?MODULE,?LINE,Reason]};
		       _GitResult-> 
			   %% Add dir paths for erlang vm 
			   %% Ebin allways
			   Ebin=filename:join(ApplicationDir,"ebin"),
			   %% Check if a priv dir is a available to add into 
			   PrivDir=filename:join(ApplicationDir,"priv"),
			   AddPatha=case filelib:is_dir(PrivDir) of
					false->
					     [Ebin];
					true->
					     [Ebin,PrivDir]
				    end,
			   case rpc:call(WorkerNode,code,add_pathsa,[AddPatha],5000) of 
			       {error,bad_directory}->
				   {error,[" Failed to add Ebin path in node , bad_directory ",AddPatha,WorkerNode,?MODULE,?LINE]};
			        {badrpc,Reason}->
				   {error,["Failed to add path to Ebin dir ",Ebin,?MODULE,?LINE,Reason]};
			       ok->
				   case rpc:call(WorkerNode,application,load,[App],5000) of
				       {badrpc,Reason}->
					   {error,["Failed to load Application  ",App,?MODULE,?LINE,Reason]};
				       {error,Reason}->
					   {error,["Failed to load Application  ",App,?MODULE,?LINE,Reason]};
				       ok->
					   {ok,ApplicationDir}
				   end
			   end
		   end
	   end,
    Result.
