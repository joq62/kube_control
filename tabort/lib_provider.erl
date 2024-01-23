%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_provider).
 
%% API
-export([

	 set_wanted_state/1,

	 load_start/1,

	 load/1,
	 load/5,
	 unload/4,
	 start/2,
	 stop/2,

	 is_alive/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

set_wanted_state(DeploymentSpec)->
    
    Result=case etcd_deployment:get_deployment_list(DeploymentSpec) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,DeploymentList}->
		   {ok,MyHostName}=net:gethostname(),
		   FilteredDeploymentList=[{Provider,HostName}||{Provider,HostName}<-DeploymentList,
								MyHostName=:=HostName],

		   {Successfull,Failed}=load_start(FilteredDeploymentList,[]),	   
		   {ok,{Successfull,Failed}}
	   end,
    Result.

load_start([],LoadStart)->
    Successfull=[{Id,Node,NodeDir,Provider,App}||{ok,Id,Node,NodeDir,Provider,App}<-LoadStart],
    Failed=[{error,Reason}||{error,Reason}<-LoadStart],
    {Successfull,Failed};    
load_start([{Provider,_HostName}|T],Acc)->
    Result=case control_node:allocate() of
	       {error,Reason}->
		   {error,[Provider,Reason]};
	       {ok,Id,Node,NodeDir}->
		   case etcd_application:get_app(Provider) of
		       {error,Reason}->
			   {error,[Provider,Reason]};
		       {ok,App}->
			   case etcd_application:get_git_path(Provider) of
			       {error,Reason}->
				   {error,[Provider,Reason]};
			       {ok,GitPath}->
				   case load(Node,NodeDir,Provider,App,GitPath) of
				       {error,Reason}->
					   {error,[Provider,Reason]};
				       {ok,ProviderDir}->
					   case start(Node,App) of
					       {error,Reason}->
						   {error,[Provider,Reason]};
					       ok->
						   {ok,Id,Node,ProviderDir,Provider,App}
					   end
				   end
			   end
		   end
	   end,
    load_start(T,[Result|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_alive(ProviderNode,App)->
    case rpc:call(ProviderNode,App,ping,[],5000) of
	{badrpc,_Reason}->
	    false;
	pong->
	    true
    end.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load_start(Provider)->
    Result=case load(Provider) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,Id,Node,ProviderDir,Provider,App} ->
		   case start(Node,App) of
		       {error,Reason}->
			   control_node:free(Id),
			   {error,Reason};
		       ok->
			   {ok,Id,Node,ProviderDir,Provider,App}
		   end
	   end,					 
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
load(Provider)->
    Result=case control_node:allocate() of
	       {error,Reason}->
		   {error,[Provider,Reason]};
	       {ok,Id,Node,NodeDir}->
		   case etcd_application:get_app(Provider) of
		       {error,Reason}->
			   {error,[Provider,Reason]};
		       {ok,App}->
			   case etcd_application:get_git_path(Provider) of
			       {error,Reason}->
				   {error,[Provider,Reason]};
			       {ok,GitPath}->
				   case load(Node,NodeDir,Provider,App,GitPath) of
				       {error,Reason}->
					   {error,[Provider,Reason]};
				       {ok,ProviderDir}->
					   {ok,Id,Node,ProviderDir,Provider,App}
				   end
			   end
		   end
	   end,
    Result.
					   

load(ProviderNode,IaasDir,Provider,App,GitPath)->
    %% Create dir for provider in IaasDir
    ProviderDir=filename:join(IaasDir,Provider),
    rpc:call(ProviderNode,file,del_dir_r,[ProviderDir],5000),
    Result=case  rpc:call(ProviderNode,file,make_dir,[ProviderDir],5000) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,?MODULE,?LINE]};
	       {error,Reason}->
		   {error,["Failed to create ProviderDir ",ProviderDir,?MODULE,?LINE,Reason]};
	       ok->
		   case rpc:call(node(),os,cmd,["git clone "++GitPath++" "++ProviderDir],3*5000) of
		       {badrpc,Reason}->
			   {error,["Failed to git clone ",?MODULE,?LINE,Reason]};
		       _GitResult-> 
			   %% Add dir paths for erlang vm 
			   %% Ebin allways
			   Ebin=filename:join(ProviderDir,"ebin"),
			   %% Check if a priv dir is a available to add into 
			   PrivDir=filename:join(ProviderDir,"priv"),
			   AddPatha=case filelib:is_dir(PrivDir) of
					false->
					     [Ebin];
					true->
					     [Ebin,PrivDir]
				    end,
			   case rpc:call(ProviderNode,code,add_pathsa,[AddPatha],5000) of 
			       {error,bad_directory}->
				   {error,[" Failed to add Ebin path in node , bad_directory ",AddPatha,ProviderNode,?MODULE,?LINE]};
			        {badrpc,Reason}->
				   {error,["Failed to add path to Ebin dir ",Ebin,?MODULE,?LINE,Reason]};
			       ok->
				   case rpc:call(ProviderNode,application,load,[App],5000) of
				       {badrpc,Reason}->
					   {error,["Failed to load Application  ",App,?MODULE,?LINE,Reason]};
				       {error,Reason}->
					   {error,["Failed to load Application  ",App,?MODULE,?LINE,Reason]};
				       ok->
					   {ok,ProviderDir}
				   end
			   end
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
unload(Id,Node,ProviderDir,App)->
    rpc:call(Node,application,stop,[App],5000),
    rpc:call(node(),file,del_dir_r,[ProviderDir],5000),
    Result=case control_node:free(Id) of
	       {error,Reason}->
		   {error,[Id,Node,ProviderDir,App,Reason]};
	       ok->
		   ok
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start(ProviderNode,App)->
    Result=case rpc:call(ProviderNode,application,start,[App],5*5000) of
	       {badrpc,Reason}->
		   {error,["Failed to start Application on Node  ",App,ProviderNode,?MODULE,?LINE,Reason]};
	       {error,Reason}->
		   {error,["Failed to tart  ",App,ProviderNode,?MODULE,?LINE,Reason]};
	       ok->
		   ok
	   end,
    Result.
			    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
stop(ProviderNode,App)->
    Result=case rpc:call(ProviderNode,application,stop,[App],5000) of
	       {badrpc,Reason}->
		   {error,["Failed to stop Application on Node  ",App,ProviderNode,Reason,?MODULE,?LINE]};
	       {error,Reason}->
		   {error,["Failed to stop  ",App,ProviderNode,Reason,?MODULE,?LINE]};
	       ok->
		   ok
	   end,
    Result.
			    

